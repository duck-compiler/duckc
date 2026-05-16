use std::collections::{HashMap, HashSet};

use crate::parser2::parser::{
    DefId, DefKind, DuckType, Expr, ExprKind, ExtensionDecl, Field, FmtPart, FunTypeParam,
    FunctionDecl, Item, JsxAttr, JsxAttrValue, JsxNode, MatchArm, Param, SourceFile, StructDecl,
    SymbolDef, SymbolTable, TypeAliasDecl, TypeDescription, TypeExpr, Typed, WithSpan,
};

use crate::semantics2::type_infer::InferOutput;

// mirrors go_type_mangled from lower.rs
fn type_to_go_mangled_elem(desc: &TypeDescription<Typed>, symbols: &SymbolTable) -> String {
    match desc {
        TypeDescription::Int => "int64".into(),
        TypeDescription::UInt => "uint64".into(),
        TypeDescription::Float => "float64".into(),
        TypeDescription::Bool(_) => "bool".into(),
        TypeDescription::Char => "rune".into(),
        TypeDescription::Byte => "byte".into(),
        TypeDescription::String(_) => "string".into(),
        TypeDescription::Array(inner) => {
            format!("Slice_{}", type_to_go_mangled_elem(&inner.desc, symbols))
        }
        TypeDescription::TypeName {
            type_ref,
            type_params,
        } if type_params.is_empty() => {
            let sym = symbols.get(*type_ref);
            if matches!(sym.kind, DefKind::Struct) {
                format!("Ptr_{}", sym.name)
            } else {
                sym.name.clone()
            }
        }
        TypeDescription::Tuple(elems) if elems.is_empty() => "any".into(),
        TypeDescription::Tuple(elems) => {
            format!("Ptr_{}", tuple_struct_name_from_elems(elems, symbols))
        }
        TypeDescription::Tag(name) => format!("Ptr___DuckTag_{name}"),
        TypeDescription::Duck(duck) => {
            let mut pairs: Vec<String> = duck
                .fields
                .iter()
                .map(|f| {
                    let cap = {
                        let s = &f.name.value;
                        let mut c = s.chars();
                        match c.next() {
                            None => String::new(),
                            Some(fc) => fc.to_uppercase().collect::<String>() + c.as_str(),
                        }
                    };
                    let inner = type_to_go_mangled_elem(&f.type_expr.desc, symbols);
                    format!("Has{}_{}", cap, inner)
                })
                .collect();
            pairs.sort();
            format!("Duck_{}", pairs.join("_"))
        }
        _ => "any".into(),
    }
}

fn tuple_struct_name_from_elems(elems: &[TypeExpr<Typed>], symbols: &SymbolTable) -> String {
    let parts: Vec<String> = elems
        .iter()
        .map(|e| type_to_go_mangled_elem(&e.desc, symbols))
        .collect();
    format!("Tup_{}", parts.join("_"))
}

// mirrors go_type_str from lower.rs
fn type_to_go_str(desc: &TypeDescription<Typed>, symbols: &SymbolTable) -> String {
    match desc {
        TypeDescription::Int => "int".into(),
        TypeDescription::UInt => "uint64".into(),
        TypeDescription::Float => "float64".into(),
        TypeDescription::Bool(_) => "bool".into(),
        TypeDescription::Char => "rune".into(),
        TypeDescription::Byte => "byte".into(),
        TypeDescription::String(_) => "string".into(),
        TypeDescription::Array(inner) => format!("[]{}", type_to_go_str(&inner.desc, symbols)),
        TypeDescription::TypeName {
            type_ref,
            type_params,
        } if type_params.is_empty() => {
            let sym = symbols.get(*type_ref);
            if matches!(sym.kind, DefKind::Struct) {
                format!("*{}", sym.name)
            } else {
                sym.name.clone()
            }
        }
        TypeDescription::Tuple(elems) if elems.is_empty() => "any".into(),
        TypeDescription::Tuple(elems) => {
            format!("*{}", tuple_struct_name_from_elems(elems, symbols))
        }
        TypeDescription::Tag(name) => format!("*__DuckTag_{name}"),
        _ => "any".into(),
    }
}

fn duck_struct_name_from_desc(
    fields: &[crate::parser2::parser::Field<Typed>],
    symbols: &SymbolTable,
) -> String {
    let mut parts: Vec<(String, String)> = fields
        .iter()
        .map(|f| {
            let cap = {
                let s = &f.name.value;
                let mut c = s.chars();
                match c.next() {
                    None => String::new(),
                    Some(fc) => fc.to_uppercase().to_string() + c.as_str(),
                }
            };
            // Duck-typed fields are stored as `any` in Go structs (same as lower_duck_lit).
            let mangled = match &f.type_expr.desc {
                TypeDescription::Duck(_) => "any".into(),
                other => type_to_go_mangled_elem(other, symbols),
            };
            (cap, mangled)
        })
        .collect();
    parts.sort_by(|a, b| a.0.cmp(&b.0));
    let flat: Vec<String> = parts.into_iter().flat_map(|(n, t)| [n, t]).collect();
    format!("Duck_{}", flat.join("_"))
}

fn call_from_json(
    desc: &TypeDescription<Typed>,
    symbols: &SymbolTable,
    val: &str,
    depth: usize,
) -> String {
    match desc {
        TypeDescription::Int => format!("int_FromJson({val})"),
        TypeDescription::UInt => format!(
            "func() (uint64, error) {{ _v, _e := uint_FromJson({val}); return uint64(_v), _e }}()"
        ),
        TypeDescription::Float => format!("float64_FromJson({val})"),
        TypeDescription::Bool(_) => format!("bool_FromJson({val})"),
        TypeDescription::Char => format!("rune_FromJson({val})"),
        TypeDescription::Byte => format!("byte_FromJson({val})"),
        TypeDescription::String(_) => format!("string_FromJson({val})"),
        TypeDescription::TypeName {
            type_ref,
            type_params,
        } if type_params.is_empty() => {
            let name = symbols.get(*type_ref).name.clone();
            format!("{name}_FromJson({val})")
        }
        TypeDescription::Or(variants) | TypeDescription::And(variants) => {
            let variants_cloned: Vec<TypeExpr<Typed>> = variants.iter().cloned().collect();
            let mut code = String::new();
            for v in &variants_cloned {
                let b_type = from_json_go_type(v, symbols);
                let inner_call = call_from_json(&v.desc, symbols, val, depth + 1);
                code.push_str(&format!(
                    "{{\nvar _b {b_type}\n_b, _err := {inner_call}\n_ = _b\nif _err == nil {{\nreturn _b, nil\n}}\n}}\n"
                ));
            }
            code.push_str("return nil, errors.New(\"union parsing failed\")");
            format!("func() (any, error) {{\n{code}\n}}()")
        }
        TypeDescription::Tag(name) => format!("__DuckTag_{name}_FromJson({val})"),
        TypeDescription::Tuple(elems) if elems.is_empty() => {
            "func() (any, error) { return nil, nil }()".into()
        }
        TypeDescription::Tuple(elems) => {
            let struct_name = tuple_struct_name_from_elems(elems, symbols);
            format!("{struct_name}_FromJson({val})")
        }
        TypeDescription::Array(inner) => {
            let inner_go_type = type_to_go_str(&inner.desc, symbols);
            let r = format!("_jafr{depth}");
            let p = format!("_jafp{depth}");
            let e = format!("_jafe{depth}");
            let i = format!("_jafi{depth}");
            let inner_val = format!("{p}[{i}]");
            let inner_call = call_from_json(&inner.desc, symbols, &inner_val, depth + 1);
            format!(
                "func() ([]{inner_go_type}, error) {{ var {r} []{inner_go_type}; {p}, _, {e} := scan_json_array_parts({val}); if {e} != nil {{ return {r}, {e} }}; {r} = make([]{inner_go_type}, len({p})); for {i} := range {r} {{ _tmp{depth}, {e}2 := {inner_call}; if {e}2 != nil {{ return {r}, {e}2 }}; {r}[{i}] = _tmp{depth} }}; return {r}, nil }}()"
            )
        }
        TypeDescription::Duck(duck) => {
            let struct_name = duck_struct_name_from_desc(&duck.fields, symbols);
            format!("{struct_name}_FromJson({val})")
        }
        _ => "func() (any, error) { return nil, errors.New(\"parse_json: unsupported type\") }()"
            .into(),
    }
}

fn from_json_go_type(te: &TypeExpr<Typed>, symbols: &SymbolTable) -> String {
    match &te.desc {
        TypeDescription::Int => "int".into(),
        TypeDescription::UInt => "uint64".into(),
        TypeDescription::Float => "float64".into(),
        TypeDescription::Bool(_) => "bool".into(),
        TypeDescription::Char => "rune".into(),
        TypeDescription::Byte => "byte".into(),
        TypeDescription::String(_) => "string".into(),
        TypeDescription::Array(inner) => format!("[]{}", from_json_go_type(inner, symbols)),
        TypeDescription::TypeName { type_ref, .. } => {
            format!("*{}", symbols.get(*type_ref).name)
        }
        TypeDescription::Or(_) | TypeDescription::And(_) => "any".into(),
        TypeDescription::Tag(name) => format!("*__DuckTag_{name}"),
        TypeDescription::Tuple(elems) if elems.is_empty() => "any".into(),
        TypeDescription::Tuple(elems) => {
            format!("*{}", tuple_struct_name_from_elems(elems, symbols))
        }
        TypeDescription::Duck(duck) => {
            let struct_name = duck_struct_name_from_desc(&duck.fields, symbols);
            format!("*{struct_name}")
        }
        _ => "any".into(),
    }
}

pub fn monomorphize(out: InferOutput) -> InferOutput {
    let mut pass = MonoPass::new(out.symbols);
    let source_file = pass.transform_source_file(out.source_file);
    InferOutput {
        source_file,
        symbols: pass.symbols,
        errors: out.errors,
        module_def_ids: out.module_def_ids,
        global_scope: out.global_scope,
        generic_type_aliases: pass.generic_type_aliases,
    }
}

struct MonoPass {
    symbols: SymbolTable,
    generic_fns: HashMap<DefId, FunctionDecl<Typed>>,
    generic_structs: HashMap<DefId, StructDecl<Typed>>,
    /// Memoization: (original DefId, mangled param string) -> specialized DefId.
    specializations: HashMap<(DefId, String), DefId>,
    /// Generic methods in extension/impl blocks, keyed by struct DefId then method name.
    generic_ext_methods: HashMap<DefId, HashMap<String, FunctionDecl<Typed>>>,
    /// The target TypeExpr for each struct's extension, needed to emit specialized extensions.
    ext_targets: HashMap<DefId, TypeExpr<Typed>>,
    /// Tracks which extension method specializations have already been emitted.
    ext_specializations: HashSet<(DefId, String, String)>,
    /// Non-generic methods of generic structs; specialized when the struct is specialized.
    non_generic_ext_methods: HashMap<DefId, Vec<FunctionDecl<Typed>>>,
    /// depth cap to prevent unbounded recursion via chain/rev
    struct_method_depth: u32,
    new_items: Vec<Item<Typed>>,
    /// Generic type aliases (those with one or more type params), keyed by DefId.
    generic_type_aliases: HashMap<DefId, TypeAliasDecl<Typed>>,
    /// When inside a specialized extension method body, holds the receiver struct's info
    /// so that `self.method<T>()` calls can be specialized even when `self.ty = Any`.
    current_receiver_struct: Option<DefId>,
    current_receiver_type_params: Vec<TypeExpr<Typed>>,
}

impl MonoPass {
    fn new(symbols: SymbolTable) -> Self {
        Self {
            symbols,
            generic_fns: HashMap::new(),
            generic_structs: HashMap::new(),
            specializations: HashMap::new(),
            generic_ext_methods: HashMap::new(),
            ext_targets: HashMap::new(),
            ext_specializations: HashSet::new(),
            non_generic_ext_methods: HashMap::new(),
            struct_method_depth: 0,
            new_items: Vec::new(),
            generic_type_aliases: HashMap::new(),
            current_receiver_struct: None,
            current_receiver_type_params: Vec::new(),
        }
    }

    fn transform_source_file(&mut self, sf: SourceFile<Typed>) -> SourceFile<Typed> {
        for item in &sf.items {
            match item {
                Item::Function(f) if !f.generics.is_empty() => {
                    if let Some(id) = self.find_fn(&f.name.value) {
                        self.generic_fns.insert(id, f.clone());
                    }
                }
                Item::Struct(s) if !s.generics.is_empty() => {
                    if let Some(id) = self.find_struct(&s.name.value) {
                        self.generic_structs.insert(id, s.clone());
                    }
                }
                Item::Extension(ext) => {
                    if let TypeDescription::TypeName {
                        type_ref: struct_id,
                        type_params: target_params,
                    } = &ext.target.desc
                    {
                        let struct_id = *struct_id;
                        let is_generic_target = !target_params.is_empty();
                        for m in &ext.methods {
                            if !m.generics.is_empty() {
                                self.generic_ext_methods
                                    .entry(struct_id)
                                    .or_default()
                                    .insert(m.name.value.clone(), m.clone());
                                self.ext_targets
                                    .entry(struct_id)
                                    .or_insert_with(|| ext.target.clone());
                            } else if is_generic_target {
                                self.non_generic_ext_methods
                                    .entry(struct_id)
                                    .or_default()
                                    .push(m.clone());
                            }
                        }
                    }
                }
                Item::TypeAlias(a) => {
                    if let Some(id) = self.find_type_alias(&a.name.value) {
                        self.generic_type_aliases.insert(id, a.clone());
                    }
                }
                _ => {}
            }
        }

        let mut items: Vec<Item<Typed>> = sf
            .items
            .into_iter()
            .filter_map(|item| match item {
                Item::Function(f) if !f.generics.is_empty() => None,
                Item::Struct(s) if !s.generics.is_empty() => None,
                Item::TypeAlias(a) if !a.generics.is_empty() => None,
                Item::TypeAlias(a) => {
                    // Fully expand the RHS so the lowerer sees a concrete type (not a reference
                    // to a generic alias that was just erased above).
                    let expanded = self.expand_alias_type(a.type_expr.clone());
                    Some(Item::TypeAlias(TypeAliasDecl {
                        type_expr: expanded,
                        ..a
                    }))
                }
                Item::Function(f) => Some(Item::Function(self.transform_fn(f))),
                Item::Extension(e) => Some(Item::Extension(self.transform_ext(e))),
                other => Some(other),
            })
            .collect();

        items.extend(self.new_items.drain(..));
        SourceFile { items }
    }

    fn find_fn(&self, name: &str) -> Option<DefId> {
        self.symbols
            .iter()
            .find(|(_, d)| d.name == name && matches!(d.kind, DefKind::Function { .. }))
            .map(|(id, _)| id)
    }

    fn find_struct(&self, name: &str) -> Option<DefId> {
        self.symbols
            .iter()
            .find(|(_, d)| d.name == name && matches!(d.kind, DefKind::Struct))
            .map(|(id, _)| id)
    }

    fn find_type_alias(&self, name: &str) -> Option<DefId> {
        self.symbols
            .iter()
            .find(|(_, d)| d.name == name && matches!(d.kind, DefKind::TypeAlias))
            .map(|(id, _)| id)
    }

    /// Expand a generic type alias instantiation. Returns `None` if `alias_id` is not a
    /// registered generic alias or the param counts don't match.
    fn expand_generic_type_alias(
        &self,
        alias_id: DefId,
        alias_params: &[TypeExpr<Typed>],
    ) -> Option<TypeExpr<Typed>> {
        let alias = self.generic_type_aliases.get(&alias_id)?;
        if alias.generics.len() != alias_params.len() {
            return None;
        }
        let subs: HashMap<String, TypeExpr<Typed>> = alias
            .generics
            .iter()
            .zip(alias_params.iter())
            .map(|(g, t)| (g.value.name.value.clone(), t.clone()))
            .collect();
        Some(subst_type(&alias.type_expr, &subs, &self.symbols))
    }

    /// Expand a type alias reference (generic or non-generic) to its underlying type,
    /// recursively until no more alias layers remain.
    fn expand_alias_type(&self, te: TypeExpr<Typed>) -> TypeExpr<Typed> {
        if let TypeDescription::TypeName {
            type_ref,
            ref type_params,
        } = te.desc
        {
            if let Some(expanded) = self.expand_generic_type_alias(type_ref, type_params) {
                return self.expand_alias_type(expanded);
            }
        }
        te
    }

    fn transform_fn(&mut self, f: FunctionDecl<Typed>) -> FunctionDecl<Typed> {
        FunctionDecl {
            body: self.transform_expr(f.body),
            ..f
        }
    }

    fn transform_ext(&mut self, e: ExtensionDecl<Typed>) -> ExtensionDecl<Typed> {
        ExtensionDecl {
            methods: e
                .methods
                .into_iter()
                .filter(|m| m.generics.is_empty())
                .map(|m| self.transform_fn(m))
                .collect(),
            ..e
        }
    }

    fn transform_expr(&mut self, e: Expr<Typed>) -> Expr<Typed> {
        let span = e.span;
        let ty = e.ty.clone();

        match e.kind {
            // Generic function call: rewrite callee to the specialized DefId.
            ExprKind::Call {
                callee,
                type_params,
                args,
            } if !type_params.is_empty() && !any_has_generic_param(&type_params, &self.symbols) => {
                let callee = self.transform_expr(*callee);
                let args: Vec<_> = args.into_iter().map(|a| self.transform_expr(a)).collect();

                // Case 1: bare identifier regular generic function
                if let ExprKind::Ident(callee_id) = &callee.kind {
                    if self.generic_fns.contains_key(callee_id) {
                        let new_id = self.specialize_fn(*callee_id, type_params);
                        let fn_ty = self
                            .symbols
                            .get(new_id)
                            .ty
                            .clone()
                            .unwrap_or_else(|| ty.clone());
                        let ret_ty = match &fn_ty.desc {
                            TypeDescription::Fun { return_type, .. } => *return_type.clone(),
                            _ => ty.clone(),
                        };
                        let new_callee = Expr::typed(ExprKind::Ident(new_id), fn_ty, callee.span);
                        return Expr::typed(
                            ExprKind::Call {
                                callee: Box::new(new_callee),
                                type_params: vec![],
                                args,
                            },
                            ret_ty,
                            span,
                        );
                    }
                }

                // Case 2: field access generic extension method
                // borrow callee before taking ownership below
                let ext_info: Option<(DefId, String, String, Vec<TypeExpr<Typed>>)> =
                    if let ExprKind::Field { base, field } = &callee.kind {
                        if let TypeDescription::TypeName {
                            type_ref: struct_id,
                            type_params: struct_type_params,
                        } = &base.ty.desc
                        {
                            if self
                                .generic_ext_methods
                                .get(struct_id)
                                .map_or(false, |m| m.contains_key(field.value.as_str()))
                            {
                                Some((
                                    *struct_id,
                                    field.value.clone(),
                                    mangle_name(&field.value, &type_params),
                                    struct_type_params.clone(),
                                ))
                            } else if let Some(expanded) =
                                self.expand_generic_type_alias(*struct_id, struct_type_params)
                            {
                                // Receiver is a generic type alias — expand to find the underlying struct.
                                if let TypeDescription::TypeName {
                                    type_ref: real_struct_id,
                                    type_params: real_struct_tps,
                                } = expanded.desc
                                {
                                    if self
                                        .generic_ext_methods
                                        .get(&real_struct_id)
                                        .map_or(false, |m| m.contains_key(field.value.as_str()))
                                    {
                                        Some((
                                            real_struct_id,
                                            field.value.clone(),
                                            mangle_name(&field.value, &type_params),
                                            real_struct_tps,
                                        ))
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        } else if let ExprKind::Ident(ident_id) = &base.kind {
                            // `self.method<T>()` inside a specialized extension method body:
                            // self.ty == Any because the receiver is never typed by inference.
                            // Use the saved receiver context to find the right ext method.
                            if self.symbols.get(*ident_id).name == "self" {
                                if let Some(recv_struct_id) = self.current_receiver_struct {
                                    if self
                                        .generic_ext_methods
                                        .get(&recv_struct_id)
                                        .map_or(false, |m| m.contains_key(field.value.as_str()))
                                    {
                                        Some((
                                            recv_struct_id,
                                            field.value.clone(),
                                            mangle_name(&field.value, &type_params),
                                            self.current_receiver_type_params.clone(),
                                        ))
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                if let Some((struct_id, method_name, mangled, struct_type_params)) = ext_info {
                    self.specialize_ext_method(
                        struct_id,
                        &method_name,
                        type_params,
                        struct_type_params,
                    );
                    let (base, field_span, callee_span, callee_ty) = match callee.kind {
                        ExprKind::Field { base, field } => {
                            (*base, field.span, callee.span, callee.ty)
                        }
                        _ => unreachable!(),
                    };
                    let new_callee = Expr::typed(
                        ExprKind::Field {
                            base: Box::new(base),
                            field: WithSpan::new(mangled, field_span),
                        },
                        callee_ty,
                        callee_span,
                    );
                    return Expr::typed(
                        ExprKind::Call {
                            callee: Box::new(new_callee),
                            type_params: vec![],
                            args,
                        },
                        ty,
                        span,
                    );
                }

                Expr::typed(
                    ExprKind::Call {
                        callee: Box::new(callee),
                        type_params,
                        args,
                    },
                    ty,
                    span,
                )
            }

            // Calls with INFERRED type params: detect and specialize generic functions.
            ExprKind::Call {
                callee,
                type_params,
                args,
            } if type_params.is_empty() => {
                let callee_span = callee.span;
                let callee = self.transform_expr(*callee);
                let args: Vec<_> = args.into_iter().map(|a| self.transform_expr(a)).collect();

                if let ExprKind::Ident(callee_id) = &callee.kind {
                    let callee_id = *callee_id;
                    if let Some(gfn) = self.generic_fns.get(&callee_id).cloned() {
                        let inferred = find_generic_bindings(&gfn, &args, &self.symbols);
                        if inferred.len() == gfn.generics.len()
                            && !any_has_generic_param(&inferred, &self.symbols)
                        {
                            let new_id = self.specialize_fn(callee_id, inferred);
                            let fn_ty = self
                                .symbols
                                .get(new_id)
                                .ty
                                .clone()
                                .unwrap_or_else(|| callee.ty.clone());
                            let ret_ty = match &fn_ty.desc {
                                TypeDescription::Fun { return_type, .. } => *return_type.clone(),
                                _ => ty.clone(),
                            };
                            let new_callee =
                                Expr::typed(ExprKind::Ident(new_id), fn_ty, callee_span);
                            return Expr::typed(
                                ExprKind::Call {
                                    callee: Box::new(new_callee),
                                    type_params: vec![],
                                    args,
                                },
                                ret_ty,
                                span,
                            );
                        }
                    }
                }
                Expr::typed(
                    ExprKind::Call {
                        callee: Box::new(callee),
                        type_params,
                        args,
                    },
                    ty,
                    span,
                )
            }

            // Generic struct literal: rewrite name to the specialized DefId.
            ExprKind::StructLit {
                name,
                type_params,
                fields,
            } if !type_params.is_empty()
                && self.generic_structs.contains_key(&name)
                && !any_has_generic_param(&type_params, &self.symbols) =>
            {
                let fields = fields
                    .into_iter()
                    .map(|(l, v)| (l, self.transform_expr(v)))
                    .collect();
                let new_id = self.specialize_struct(name, type_params);
                let new_ty = TypeExpr::new(
                    TypeDescription::TypeName {
                        type_ref: new_id,
                        type_params: vec![],
                    },
                    span,
                );
                Expr::typed(
                    ExprKind::StructLit {
                        name: new_id,
                        type_params: vec![],
                        fields,
                    },
                    new_ty,
                    span,
                )
            }

            // Struct literal whose name is a generic type alias: expand the alias and
            // specialize the underlying struct, keeping the expression type as the original
            // concrete struct with its type params so that generic method lookup still works.
            ExprKind::StructLit {
                name,
                type_params,
                fields,
            } if !type_params.is_empty()
                && self.generic_type_aliases.contains_key(&name)
                && !any_has_generic_param(&type_params, &self.symbols) =>
            {
                let fields = fields
                    .into_iter()
                    .map(|(l, v)| (l, self.transform_expr(v)))
                    .collect();
                if let Some(expanded) = self.expand_generic_type_alias(name, &type_params) {
                    if let TypeDescription::TypeName {
                        type_ref: struct_id,
                        type_params: struct_tps,
                    } = expanded.desc
                    {
                        if self.generic_structs.contains_key(&struct_id)
                            && !any_has_generic_param(&struct_tps, &self.symbols)
                        {
                            let new_id = self.specialize_struct(struct_id, struct_tps.clone());
                            // Keep expression type as the underlying concrete generic instantiation
                            // (not the specialized DefId) so method-call lookup via generic_ext_methods
                            // still finds the right entry.
                            let new_ty = TypeExpr::new(
                                TypeDescription::TypeName {
                                    type_ref: struct_id,
                                    type_params: struct_tps,
                                },
                                span,
                            );
                            return Expr::typed(
                                ExprKind::StructLit {
                                    name: new_id,
                                    type_params: vec![],
                                    fields,
                                },
                                new_ty,
                                span,
                            );
                        }
                    }
                }
                Expr::typed(
                    self.transform_children(ExprKind::StructLit {
                        name,
                        type_params,
                        fields,
                    }),
                    ty,
                    span,
                )
            }

            kind => Expr::typed(self.transform_children(kind), ty, span),
        }
    }

    fn transform_children(&mut self, kind: ExprKind<Typed>) -> ExprKind<Typed> {
        match kind {
            ExprKind::Block(stmts) => {
                ExprKind::Block(stmts.into_iter().map(|s| self.transform_expr(s)).collect())
            }
            ExprKind::Let {
                is_mut,
                name,
                type_ann,
                value,
            } => ExprKind::Let {
                is_mut,
                name,
                type_ann: type_ann.map(|ann| self.expand_alias_type(ann)),
                value: Box::new(self.transform_expr(*value)),
            },
            ExprKind::Const {
                name,
                type_ann,
                value,
            } => ExprKind::Const {
                name,
                type_ann: type_ann.map(|ann| self.expand_alias_type(ann)),
                value: Box::new(self.transform_expr(*value)),
            },
            ExprKind::Assign { target, value } => ExprKind::Assign {
                target: Box::new(self.transform_expr(*target)),
                value: Box::new(self.transform_expr(*value)),
            },
            ExprKind::AddAssign { target, value } => ExprKind::AddAssign {
                target: Box::new(self.transform_expr(*target)),
                value: Box::new(self.transform_expr(*value)),
            },
            ExprKind::SubAssign { target, value } => ExprKind::SubAssign {
                target: Box::new(self.transform_expr(*target)),
                value: Box::new(self.transform_expr(*value)),
            },
            ExprKind::MulAssign { target, value } => ExprKind::MulAssign {
                target: Box::new(self.transform_expr(*target)),
                value: Box::new(self.transform_expr(*value)),
            },
            ExprKind::DivAssign { target, value } => ExprKind::DivAssign {
                target: Box::new(self.transform_expr(*target)),
                value: Box::new(self.transform_expr(*value)),
            },
            ExprKind::ModAssign { target, value } => ExprKind::ModAssign {
                target: Box::new(self.transform_expr(*target)),
                value: Box::new(self.transform_expr(*value)),
            },
            ExprKind::ShrAssign { target, value } => ExprKind::ShrAssign {
                target: Box::new(self.transform_expr(*target)),
                value: Box::new(self.transform_expr(*value)),
            },
            ExprKind::ShlAssign { target, value } => ExprKind::ShlAssign {
                target: Box::new(self.transform_expr(*target)),
                value: Box::new(self.transform_expr(*value)),
            },
            ExprKind::Add(a, b) => ExprKind::Add(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::Sub(a, b) => ExprKind::Sub(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::Mul(a, b) => ExprKind::Mul(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::Div(a, b) => ExprKind::Div(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::Mod(a, b) => ExprKind::Mod(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::BitAnd(a, b) => ExprKind::BitAnd(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::BitOr(a, b) => ExprKind::BitOr(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::BitXor(a, b) => ExprKind::BitXor(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::BitNot(a) => ExprKind::BitNot(Box::new(self.transform_expr(*a))),
            ExprKind::Shl(a, b) => ExprKind::Shl(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::Shr(a, b) => ExprKind::Shr(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::Eq(a, b) => ExprKind::Eq(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::Neq(a, b) => ExprKind::Neq(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::Lt(a, b) => ExprKind::Lt(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::Lte(a, b) => ExprKind::Lte(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::Gt(a, b) => ExprKind::Gt(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::Gte(a, b) => ExprKind::Gte(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::And(a, b) => ExprKind::And(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::Or(a, b) => ExprKind::Or(
                Box::new(self.transform_expr(*a)),
                Box::new(self.transform_expr(*b)),
            ),
            ExprKind::Not(a) => ExprKind::Not(Box::new(self.transform_expr(*a))),
            ExprKind::Neg(a) => ExprKind::Neg(Box::new(self.transform_expr(*a))),
            ExprKind::Ref(a) => ExprKind::Ref(Box::new(self.transform_expr(*a))),
            ExprKind::RefMut(a) => ExprKind::RefMut(Box::new(self.transform_expr(*a))),
            ExprKind::Deref(a) => ExprKind::Deref(Box::new(self.transform_expr(*a))),
            ExprKind::Field { base, field } => ExprKind::Field {
                base: Box::new(self.transform_expr(*base)),
                field,
            },
            ExprKind::Index { base, index } => ExprKind::Index {
                base: Box::new(self.transform_expr(*base)),
                index: Box::new(self.transform_expr(*index)),
            },
            ExprKind::ScopeRes { base, member } => ExprKind::ScopeRes {
                base: Box::new(self.transform_expr(*base)),
                member,
            },
            ExprKind::Call {
                callee,
                type_params,
                args,
            } => ExprKind::Call {
                callee: Box::new(self.transform_expr(*callee)),
                type_params,
                args: args.into_iter().map(|a| self.transform_expr(a)).collect(),
            },
            ExprKind::As { value, type_expr } => ExprKind::As {
                value: Box::new(self.transform_expr(*value)),
                type_expr,
            },
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => ExprKind::If {
                condition: Box::new(self.transform_expr(*condition)),
                then_branch: Box::new(self.transform_expr(*then_branch)),
                else_branch: else_branch.map(|e| Box::new(self.transform_expr(*e))),
            },
            ExprKind::While { condition, body } => ExprKind::While {
                condition: Box::new(self.transform_expr(*condition)),
                body: Box::new(self.transform_expr(*body)),
            },
            ExprKind::For {
                binding,
                is_mut,
                iterable,
                body,
            } => ExprKind::For {
                binding,
                is_mut,
                iterable: Box::new(self.transform_expr(*iterable)),
                body: Box::new(self.transform_expr(*body)),
            },
            ExprKind::Return(v) => ExprKind::Return(v.map(|e| Box::new(self.transform_expr(*e)))),
            ExprKind::Match {
                value,
                arms,
                else_arm,
            } => ExprKind::Match {
                value: Box::new(self.transform_expr(*value)),
                arms: arms
                    .into_iter()
                    .map(|arm| MatchArm {
                        guard: arm.guard.map(|g| Box::new(self.transform_expr(*g))),
                        body: self.transform_expr(arm.body),
                        ..arm
                    })
                    .collect(),
                else_arm: else_arm.map(|e| Box::new(self.transform_expr(*e))),
            },
            ExprKind::StructLit {
                name,
                type_params,
                fields,
            } => ExprKind::StructLit {
                name,
                type_params,
                fields: fields
                    .into_iter()
                    .map(|(l, v)| (l, self.transform_expr(v)))
                    .collect(),
            },
            ExprKind::DuckLit(fields) => ExprKind::DuckLit(
                fields
                    .into_iter()
                    .map(|(l, v)| (l, self.transform_expr(v)))
                    .collect(),
            ),
            ExprKind::Array(elems) => {
                ExprKind::Array(elems.into_iter().map(|e| self.transform_expr(e)).collect())
            }
            ExprKind::Tuple(elems) => {
                ExprKind::Tuple(elems.into_iter().map(|e| self.transform_expr(e)).collect())
            }
            ExprKind::Lambda {
                is_mut,
                params,
                return_type,
                body,
            } => ExprKind::Lambda {
                is_mut,
                params,
                return_type,
                body: Box::new(self.transform_expr(*body)),
            },
            ExprKind::Async(e) => ExprKind::Async(Box::new(self.transform_expr(*e))),
            ExprKind::Defer(e) => ExprKind::Defer(Box::new(self.transform_expr(*e))),
            ExprKind::FmtString(parts) => ExprKind::FmtString(
                parts
                    .into_iter()
                    .map(|p| match p {
                        FmtPart::Literal(s) => FmtPart::Literal(s),
                        FmtPart::Expr(e) => FmtPart::Expr(self.transform_expr(e)),
                    })
                    .collect(),
            ),
            ExprKind::Jsx(node) => ExprKind::Jsx(Box::new(self.mono_jsx_node(*node))),
            leaf => leaf,
        }
    }

    fn mono_jsx_node(&mut self, node: JsxNode<Typed>) -> JsxNode<Typed> {
        match node {
            JsxNode::Text(s) => JsxNode::Text(s),
            JsxNode::Expr(e) => JsxNode::Expr(Box::new(self.transform_expr(*e))),
            JsxNode::Element {
                tag,
                attrs,
                children,
            } => JsxNode::Element {
                tag,
                attrs: attrs
                    .into_iter()
                    .map(|a| JsxAttr {
                        name: a.name,
                        value: match a.value {
                            JsxAttrValue::Bool => JsxAttrValue::Bool,
                            JsxAttrValue::Str(s) => JsxAttrValue::Str(s),
                            JsxAttrValue::Expr(e) => {
                                JsxAttrValue::Expr(Box::new(self.transform_expr(*e)))
                            }
                        },
                    })
                    .collect(),
                children: children
                    .into_iter()
                    .map(|c| self.mono_jsx_node(c))
                    .collect(),
            },
        }
    }

    fn specialize_fn(&mut self, id: DefId, type_params: Vec<TypeExpr<Typed>>) -> DefId {
        let key = (id, mangle_name("", &type_params));
        if let Some(&cached) = self.specializations.get(&key) {
            return cached;
        }

        let gfn = self.generic_fns[&id].clone();
        let subs: HashMap<String, TypeExpr<Typed>> = gfn
            .generics
            .iter()
            .zip(type_params.iter())
            .map(|(g, t)| (g.value.name.value.clone(), t.clone()))
            .collect();

        let mangled = mangle_name(&gfn.name.value, &type_params);

        let params: Vec<Param<Typed>> = gfn
            .params
            .iter()
            .map(|p| Param {
                name: p.name.clone(),
                type_expr: subst_type(&p.type_expr, &subs, &self.symbols),
                is_mut: p.is_mut,
            })
            .collect();
        let return_type = gfn
            .return_type
            .as_ref()
            .map(|t| subst_type(t, &subs, &self.symbols));
        let body = subst_expr(gfn.body.clone(), &subs, &self.symbols);

        // Special-case: inject the Go deserialization body for parse_json<T>.
        let body = if gfn.name.value == "parse_json" && type_params.len() == 1 {
            let t = &type_params[0];
            let call = call_from_json(&t.desc, &self.symbols, "json_str", 0);
            let go_code = format!(
                "obj, _err := ({call})\nif _err != nil {{\nreturn &__DuckTag_err{{}}\n}}\nreturn obj"
            );
            Expr::typed(ExprKind::InlineGo(go_code), body.ty.clone(), body.span)
        } else {
            body
        };

        let fn_ty = make_fn_ty(&params, &return_type, gfn.span);
        let new_id = self.symbols.insert(SymbolDef {
            kind: DefKind::Function {
                is_static: gfn.is_static,
                is_client: gfn.is_client,
            },
            name: mangled.clone(),
            span: gfn.span,
            scope_depth: 0,
            ty: Some(fn_ty),
        });

        // register early so recursive calls inside the body resolve correctly
        self.specializations.insert(key, new_id);

        let body = self.remap_locals(body);
        let body = self.transform_expr(body);

        self.new_items.push(Item::Function(FunctionDecl {
            name: WithSpan::new(mangled, gfn.span),
            generics: vec![],
            params,
            return_type,
            body,
            is_static: gfn.is_static,
            is_client: gfn.is_client,
            span: gfn.span,
        }));

        new_id
    }

    fn specialize_struct(&mut self, id: DefId, type_params: Vec<TypeExpr<Typed>>) -> DefId {
        let key = (id, mangle_name("", &type_params));
        if let Some(&cached) = self.specializations.get(&key) {
            return cached;
        }

        let gs = self.generic_structs[&id].clone();
        let subs: HashMap<String, TypeExpr<Typed>> = gs
            .generics
            .iter()
            .zip(type_params.iter())
            .map(|(g, t)| (g.value.name.value.clone(), t.clone()))
            .collect();

        let mangled = mangle_name(&gs.name.value, &type_params);
        let fields: Vec<Field<Typed>> = gs
            .fields
            .iter()
            .map(|f| Field {
                name: f.name.clone(),
                type_expr: subst_type(&f.type_expr, &subs, &self.symbols),
            })
            .collect();

        let new_id = self.symbols.insert(SymbolDef {
            kind: DefKind::Struct,
            name: mangled.clone(),
            span: gs.span,
            scope_depth: 0,
            ty: None,
        });

        self.specializations.insert(key, new_id);
        self.new_items.push(Item::Struct(StructDecl {
            name: WithSpan::new(mangled, gs.span),
            generics: vec![],
            fields,
            derived: gs.derived.clone(),
            span: gs.span,
        }));

        // depth cap prevents infinite recursion via rev/chain
        const MAX_STRUCT_METHOD_DEPTH: u32 = 3;
        let non_generic_methods = self
            .non_generic_ext_methods
            .get(&id)
            .cloned()
            .unwrap_or_default();
        if !non_generic_methods.is_empty() && self.struct_method_depth < MAX_STRUCT_METHOD_DEPTH {
            self.struct_method_depth += 1;
            let target_ty = TypeExpr::new(
                TypeDescription::TypeName {
                    type_ref: new_id,
                    type_params: vec![],
                },
                gs.span,
            );
            let mut specialized_methods: Vec<FunctionDecl<Typed>> = Vec::new();
            for m in &non_generic_methods {
                let params: Vec<Param<Typed>> = m
                    .params
                    .iter()
                    .map(|p| Param {
                        name: p.name.clone(),
                        type_expr: subst_type(&p.type_expr, &subs, &self.symbols),
                        is_mut: p.is_mut,
                    })
                    .collect();
                let return_type = m
                    .return_type
                    .as_ref()
                    .map(|t| subst_type(t, &subs, &self.symbols));
                let body = subst_expr(m.body.clone(), &subs, &self.symbols);
                let body = self.remap_locals(body);
                let body = self.transform_expr(body);
                specialized_methods.push(FunctionDecl {
                    name: m.name.clone(),
                    generics: vec![],
                    params,
                    return_type,
                    body,
                    is_static: m.is_static,
                    is_client: m.is_client,
                    span: m.span,
                });
            }
            self.struct_method_depth -= 1;
            self.new_items.push(Item::Extension(ExtensionDecl {
                target: target_ty,
                methods: specialized_methods,
                span: gs.span,
            }));
        }

        new_id
    }

    /// Emits one specialized `Item::Extension` for a generic extension method.
    /// Idempotent: calling it twice with the same arguments emits only one item.
    fn specialize_ext_method(
        &mut self,
        struct_id: DefId,
        method_name: &str,
        type_params: Vec<TypeExpr<Typed>>,
        struct_type_params: Vec<TypeExpr<Typed>>,
    ) {
        let mangled = mangle_name(method_name, &type_params);
        let struct_mangled = mangle_name("", &struct_type_params);
        let key = (struct_id, struct_mangled.clone(), mangled.clone());
        if !self.ext_specializations.insert(key) {
            return;
        }

        let gm = self.generic_ext_methods[&struct_id][method_name].clone();
        let target = self.ext_targets[&struct_id].clone();

        let struct_subs: HashMap<String, TypeExpr<Typed>> = self
            .generic_structs
            .get(&struct_id)
            .map(|gs| {
                gs.generics
                    .iter()
                    .zip(struct_type_params.iter())
                    .map(|(g, t)| (g.value.name.value.clone(), t.clone()))
                    .collect()
            })
            .unwrap_or_default();

        let method_subs: HashMap<String, TypeExpr<Typed>> = gm
            .generics
            .iter()
            .zip(type_params.iter())
            .map(|(g, t)| (g.value.name.value.clone(), t.clone()))
            .collect();

        // method params take precedence if names collide
        let mut subs = struct_subs;
        subs.extend(method_subs);

        // use the concrete specialized struct DefId so lower_extension doesn't drop this
        let concrete_target = if !struct_type_params.is_empty() {
            let cache_key = (struct_id, mangle_name("", &struct_type_params));
            let specialized_id = if let Some(&id) = self.specializations.get(&cache_key) {
                id
            } else if self.generic_structs.contains_key(&struct_id) {
                self.specialize_struct(struct_id, struct_type_params.clone())
            } else {
                // Cannot specialize: fall back to substituting the target type expression.
                // lower_extension will emit the method on the concrete (non-generic) target.
                return;
            };
            TypeExpr::new(
                TypeDescription::TypeName {
                    type_ref: specialized_id,
                    type_params: vec![],
                },
                target.span,
            )
        } else {
            target
        };

        let params: Vec<Param<Typed>> = gm
            .params
            .iter()
            .map(|p| Param {
                name: p.name.clone(),
                type_expr: subst_type(&p.type_expr, &subs, &self.symbols),
                is_mut: p.is_mut,
            })
            .collect();
        let return_type = gm
            .return_type
            .as_ref()
            .map(|t| subst_type(t, &subs, &self.symbols));
        let body = subst_expr(gm.body.clone(), &subs, &self.symbols);
        let body = self.remap_locals(body);
        let prev_recv_struct = self.current_receiver_struct;
        let prev_recv_tps = std::mem::take(&mut self.current_receiver_type_params);
        self.current_receiver_struct = Some(struct_id);
        self.current_receiver_type_params = struct_type_params.clone();
        let body = self.transform_expr(body);
        self.current_receiver_struct = prev_recv_struct;
        self.current_receiver_type_params = prev_recv_tps;

        let specialized = FunctionDecl {
            name: WithSpan::new(mangled, gm.span),
            generics: vec![],
            params,
            return_type,
            body,
            is_static: gm.is_static,
            is_client: gm.is_client,
            span: gm.span,
        };

        self.new_items.push(Item::Extension(ExtensionDecl {
            target: concrete_target,
            methods: vec![specialized],
            span: gm.span,
        }));
    }

    /// fresh DefIds per specialization so lowerer claim_local queues don't collide
    fn remap_locals(&mut self, body: Expr<Typed>) -> Expr<Typed> {
        let mut local_ids: Vec<DefId> = Vec::new();
        collect_local_idents(&body, &self.symbols, &mut local_ids);
        if local_ids.is_empty() {
            return body;
        }

        let mut remap: HashMap<DefId, DefId> = HashMap::new();
        for old_id in local_ids {
            let d = self.symbols.get(old_id);
            // skip self - lowerer resolves it by stable id
            if d.name == "self" {
                continue;
            }
            let new_def = SymbolDef {
                kind: d.kind.clone(),
                name: d.name.clone(),
                span: d.span,
                scope_depth: d.scope_depth,
                ty: d.ty.clone(),
            };
            let new_id = self.symbols.insert(new_def);
            remap.insert(old_id, new_id);
        }

        remap_idents(body, &remap)
    }
}

fn find_generic_bindings(
    fn_decl: &crate::parser2::parser::FunctionDecl<Typed>,
    args: &[crate::parser2::parser::Expr<Typed>],
    symbols: &SymbolTable,
) -> Vec<TypeExpr<Typed>> {
    let n = fn_decl.generics.len();
    let mut bindings: Vec<Option<TypeExpr<Typed>>> = vec![None; n];
    for (param, arg) in fn_decl.params.iter().zip(args.iter()) {
        unify_generic(&param.type_expr, &arg.ty, fn_decl, symbols, &mut bindings);
    }
    bindings.into_iter().flatten().collect()
}

fn unify_generic(
    param: &TypeExpr<Typed>,
    arg: &TypeExpr<Typed>,
    fn_decl: &crate::parser2::parser::FunctionDecl<Typed>,
    symbols: &SymbolTable,
    bindings: &mut Vec<Option<TypeExpr<Typed>>>,
) {
    match &param.desc {
        TypeDescription::TypeName {
            type_ref,
            type_params,
        } if type_params.is_empty() => {
            if matches!(symbols.get(*type_ref).kind, DefKind::GenericParam) {
                let name = symbols.get(*type_ref).name.clone();
                if let Some(idx) = fn_decl
                    .generics
                    .iter()
                    .position(|g| g.value.name.value == name)
                {
                    if bindings[idx].is_none() {
                        bindings[idx] = Some(arg.clone());
                    }
                }
            }
        }
        TypeDescription::Array(inner) => {
            if let TypeDescription::Array(arg_inner) = &arg.desc {
                unify_generic(inner, arg_inner, fn_decl, symbols, bindings);
            }
        }
        TypeDescription::Tuple(elems) => {
            if let TypeDescription::Tuple(arg_elems) = &arg.desc {
                for (e, a) in elems.iter().zip(arg_elems.iter()) {
                    unify_generic(e, a, fn_decl, symbols, bindings);
                }
            }
        }
        _ => {}
    }
}

/// true if any type expr still contains an unresolved GenericParam
fn any_has_generic_param(type_params: &[TypeExpr<Typed>], symbols: &SymbolTable) -> bool {
    type_params
        .iter()
        .any(|tp| type_has_generic_param(tp, symbols))
}

fn type_has_generic_param(te: &TypeExpr<Typed>, symbols: &SymbolTable) -> bool {
    match &te.desc {
        TypeDescription::TypeName {
            type_ref,
            type_params,
        } => {
            matches!(symbols.get(*type_ref).kind, DefKind::GenericParam)
                || type_has_generic_param_slice(type_params, symbols)
        }
        TypeDescription::Array(elem) => type_has_generic_param(elem, symbols),
        TypeDescription::Ref(inner) | TypeDescription::RefMut(inner) => {
            type_has_generic_param(inner, symbols)
        }
        _ => false,
    }
}

fn type_has_generic_param_slice(tps: &[TypeExpr<Typed>], symbols: &SymbolTable) -> bool {
    tps.iter().any(|tp| type_has_generic_param(tp, symbols))
}

fn mangle_name(base: &str, type_params: &[TypeExpr<Typed>]) -> String {
    let parts: Vec<String> = type_params.iter().map(type_param_name).collect();
    format!("{}__{}", base, parts.join("__"))
}

fn type_param_name(te: &TypeExpr<Typed>) -> String {
    match &te.desc {
        TypeDescription::Int => "Int".into(),
        TypeDescription::UInt => "UInt".into(),
        TypeDescription::Float => "Float".into(),
        TypeDescription::Bool(_) => "Bool".into(),
        TypeDescription::Char => "Char".into(),
        TypeDescription::Byte => "Byte".into(),
        TypeDescription::String(_) => "String".into(),
        TypeDescription::Any => "Any".into(),
        TypeDescription::TypeName {
            type_ref,
            type_params,
        } => {
            if type_params.is_empty() {
                format!("N{}", type_ref.0)
            } else {
                let inner: Vec<String> = type_params.iter().map(type_param_name).collect();
                format!("N{}_{}", type_ref.0, inner.join("_"))
            }
        }
        TypeDescription::TemplParam(name) => name.clone(),
        TypeDescription::Tuple(elems) if elems.is_empty() => "EmptyTup".into(),
        TypeDescription::Tuple(elems) => {
            let parts: Vec<String> = elems.iter().map(type_param_name).collect();
            format!("Tup_{}", parts.join("_"))
        }
        TypeDescription::Array(inner) => format!("Slice_{}", type_param_name(inner)),
        TypeDescription::Tag(name) => format!("Tag_{name}"),
        TypeDescription::Or(variants) => {
            let parts: Vec<String> = variants.iter().map(type_param_name).collect();
            format!("Union_{}", parts.join("_"))
        }
        TypeDescription::And(variants) => {
            let parts: Vec<String> = variants.iter().map(type_param_name).collect();
            format!("Inter_{}", parts.join("_"))
        }
        TypeDescription::Duck(duck) => {
            let mut parts: Vec<String> = duck
                .fields
                .iter()
                .map(|f| format!("{}_{}", f.name.value, type_param_name(&f.type_expr)))
                .collect();
            parts.sort();
            format!("Duck_{}", parts.join("_"))
        }
        _ => "Any".into(),
    }
}

fn make_fn_ty(
    params: &[Param<Typed>],
    return_type: &Option<TypeExpr<Typed>>,
    span: crate::parser2::parser::Span,
) -> TypeExpr<Typed> {
    let param_tys: Vec<FunTypeParam<Typed>> = params
        .iter()
        .map(|p| FunTypeParam {
            label: Some(p.name.clone()),
            type_expr: p.type_expr.clone(),
        })
        .collect();
    let ret = return_type
        .clone()
        .unwrap_or_else(|| TypeExpr::new(TypeDescription::Statement, span));
    TypeExpr::new(
        TypeDescription::Fun {
            params: param_tys,
            return_type: Box::new(ret),
            is_mut: false,
            is_variadic: false,
        },
        span,
    )
}

pub(crate) fn subst_type(
    te: &TypeExpr<Typed>,
    subs: &HashMap<String, TypeExpr<Typed>>,
    symbols: &SymbolTable,
) -> TypeExpr<Typed> {
    let desc = match &te.desc {
        TypeDescription::TemplParam(name) => {
            if let Some(concrete) = subs.get(name) {
                return concrete.clone();
            }
            TypeDescription::TemplParam(name.clone())
        }
        // bare TypeName with no params may be a GenericParam (how resolver encodes T)
        TypeDescription::TypeName {
            type_ref,
            type_params,
        } if type_params.is_empty() => {
            if matches!(symbols.get(*type_ref).kind, DefKind::GenericParam) {
                let param_name = &symbols.get(*type_ref).name;
                if let Some(concrete) = subs.get(param_name) {
                    return concrete.clone();
                }
            }
            TypeDescription::TypeName {
                type_ref: *type_ref,
                type_params: vec![],
            }
        }
        TypeDescription::Fun {
            params,
            return_type,
            is_mut,
            is_variadic,
        } => TypeDescription::Fun {
            params: params
                .iter()
                .map(|p| FunTypeParam {
                    label: p.label.clone(),
                    type_expr: subst_type(&p.type_expr, subs, symbols),
                })
                .collect(),
            return_type: Box::new(subst_type(return_type, subs, symbols)),
            is_mut: *is_mut,
            is_variadic: *is_variadic,
        },
        TypeDescription::Array(elem) => {
            TypeDescription::Array(Box::new(subst_type(elem, subs, symbols)))
        }
        TypeDescription::Ref(inner) => {
            TypeDescription::Ref(Box::new(subst_type(inner, subs, symbols)))
        }
        TypeDescription::RefMut(inner) => {
            TypeDescription::RefMut(Box::new(subst_type(inner, subs, symbols)))
        }
        TypeDescription::TypeName {
            type_ref,
            type_params,
        } => TypeDescription::TypeName {
            type_ref: *type_ref,
            type_params: type_params
                .iter()
                .map(|tp| subst_type(tp, subs, symbols))
                .collect(),
        },
        TypeDescription::Tuple(elems) => {
            TypeDescription::Tuple(elems.iter().map(|e| subst_type(e, subs, symbols)).collect())
        }
        TypeDescription::Or(vs) => {
            TypeDescription::Or(vs.iter().map(|v| subst_type(v, subs, symbols)).collect())
        }
        TypeDescription::And(ps) => {
            TypeDescription::And(ps.iter().map(|p| subst_type(p, subs, symbols)).collect())
        }
        TypeDescription::Duck(duck) => TypeDescription::Duck(DuckType {
            fields: duck
                .fields
                .iter()
                .map(|f| Field {
                    name: f.name.clone(),
                    type_expr: subst_type(&f.type_expr, subs, symbols),
                })
                .collect(),
        }),
        other => other.clone(),
    };
    TypeExpr::new(desc, te.span)
}

fn subst_expr(
    e: Expr<Typed>,
    subs: &HashMap<String, TypeExpr<Typed>>,
    symbols: &SymbolTable,
) -> Expr<Typed> {
    let ty = subst_type(&e.ty, subs, symbols);
    let kind = subst_kind(e.kind, subs, symbols);
    Expr::typed(kind, ty, e.span)
}

fn subst_kind(
    kind: ExprKind<Typed>,
    subs: &HashMap<String, TypeExpr<Typed>>,
    symbols: &SymbolTable,
) -> ExprKind<Typed> {
    let se = |e: Expr<Typed>| subst_expr(e, subs, symbols);
    let sb = |e: Box<Expr<Typed>>| Box::new(subst_expr(*e, subs, symbols));
    let st = |te: TypeExpr<Typed>| subst_type(&te, subs, symbols);

    match kind {
        ExprKind::Block(stmts) => ExprKind::Block(stmts.into_iter().map(se).collect()),
        ExprKind::Let {
            is_mut,
            name,
            type_ann,
            value,
        } => ExprKind::Let {
            is_mut,
            name,
            type_ann: type_ann.map(|te| st(te)),
            value: sb(value),
        },
        ExprKind::Const {
            name,
            type_ann,
            value,
        } => ExprKind::Const {
            name,
            type_ann: type_ann.map(|te| st(te)),
            value: sb(value),
        },
        ExprKind::Assign { target, value } => ExprKind::Assign {
            target: sb(target),
            value: sb(value),
        },
        ExprKind::AddAssign { target, value } => ExprKind::AddAssign {
            target: sb(target),
            value: sb(value),
        },
        ExprKind::SubAssign { target, value } => ExprKind::SubAssign {
            target: sb(target),
            value: sb(value),
        },
        ExprKind::MulAssign { target, value } => ExprKind::MulAssign {
            target: sb(target),
            value: sb(value),
        },
        ExprKind::DivAssign { target, value } => ExprKind::DivAssign {
            target: sb(target),
            value: sb(value),
        },
        ExprKind::ModAssign { target, value } => ExprKind::ModAssign {
            target: sb(target),
            value: sb(value),
        },
        ExprKind::ShrAssign { target, value } => ExprKind::ShrAssign {
            target: sb(target),
            value: sb(value),
        },
        ExprKind::ShlAssign { target, value } => ExprKind::ShlAssign {
            target: sb(target),
            value: sb(value),
        },
        ExprKind::Add(a, b) => ExprKind::Add(sb(a), sb(b)),
        ExprKind::Sub(a, b) => ExprKind::Sub(sb(a), sb(b)),
        ExprKind::Mul(a, b) => ExprKind::Mul(sb(a), sb(b)),
        ExprKind::Div(a, b) => ExprKind::Div(sb(a), sb(b)),
        ExprKind::Mod(a, b) => ExprKind::Mod(sb(a), sb(b)),
        ExprKind::BitAnd(a, b) => ExprKind::BitAnd(sb(a), sb(b)),
        ExprKind::BitOr(a, b) => ExprKind::BitOr(sb(a), sb(b)),
        ExprKind::BitXor(a, b) => ExprKind::BitXor(sb(a), sb(b)),
        ExprKind::BitNot(a) => ExprKind::BitNot(sb(a)),
        ExprKind::Shl(a, b) => ExprKind::Shl(sb(a), sb(b)),
        ExprKind::Shr(a, b) => ExprKind::Shr(sb(a), sb(b)),
        ExprKind::Eq(a, b) => ExprKind::Eq(sb(a), sb(b)),
        ExprKind::Neq(a, b) => ExprKind::Neq(sb(a), sb(b)),
        ExprKind::Lt(a, b) => ExprKind::Lt(sb(a), sb(b)),
        ExprKind::Lte(a, b) => ExprKind::Lte(sb(a), sb(b)),
        ExprKind::Gt(a, b) => ExprKind::Gt(sb(a), sb(b)),
        ExprKind::Gte(a, b) => ExprKind::Gte(sb(a), sb(b)),
        ExprKind::And(a, b) => ExprKind::And(sb(a), sb(b)),
        ExprKind::Or(a, b) => ExprKind::Or(sb(a), sb(b)),
        ExprKind::Not(a) => ExprKind::Not(sb(a)),
        ExprKind::Neg(a) => ExprKind::Neg(sb(a)),
        ExprKind::Ref(a) => ExprKind::Ref(sb(a)),
        ExprKind::RefMut(a) => ExprKind::RefMut(sb(a)),
        ExprKind::Deref(a) => ExprKind::Deref(sb(a)),
        ExprKind::Field { base, field } => ExprKind::Field {
            base: sb(base),
            field,
        },
        ExprKind::Index { base, index } => ExprKind::Index {
            base: sb(base),
            index: sb(index),
        },
        ExprKind::ScopeRes { base, member } => ExprKind::ScopeRes {
            base: sb(base),
            member,
        },
        ExprKind::Call {
            callee,
            type_params,
            args,
        } => ExprKind::Call {
            callee: sb(callee),
            type_params: type_params.into_iter().map(|tp| st(tp)).collect(),
            args: args.into_iter().map(se).collect(),
        },
        ExprKind::As { value, type_expr } => ExprKind::As {
            value: sb(value),
            type_expr: st(type_expr),
        },
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => ExprKind::If {
            condition: sb(condition),
            then_branch: sb(then_branch),
            else_branch: else_branch.map(|e| sb(e)),
        },
        ExprKind::While { condition, body } => ExprKind::While {
            condition: sb(condition),
            body: sb(body),
        },
        ExprKind::For {
            binding,
            is_mut,
            iterable,
            body,
        } => ExprKind::For {
            binding,
            is_mut,
            iterable: sb(iterable),
            body: sb(body),
        },
        ExprKind::Return(v) => ExprKind::Return(v.map(|e| sb(e))),
        ExprKind::Match {
            value,
            arms,
            else_arm,
        } => ExprKind::Match {
            value: sb(value),
            arms: arms
                .into_iter()
                .map(|arm| MatchArm {
                    pattern: st(arm.pattern),
                    base: arm.base.map(|t| st(t)),
                    binding: arm.binding,
                    guard: arm.guard.map(|g| sb(g)),
                    body: se(arm.body),
                    span: arm.span,
                })
                .collect(),
            else_arm: else_arm.map(|e| sb(e)),
        },
        ExprKind::StructLit {
            name,
            type_params,
            fields,
        } => ExprKind::StructLit {
            name,
            type_params: type_params.into_iter().map(|tp| st(tp)).collect(),
            fields: fields.into_iter().map(|(l, v)| (l, se(v))).collect(),
        },
        ExprKind::DuckLit(fields) => {
            ExprKind::DuckLit(fields.into_iter().map(|(l, v)| (l, se(v))).collect())
        }
        ExprKind::Array(elems) => ExprKind::Array(elems.into_iter().map(se).collect()),
        ExprKind::Tuple(elems) => ExprKind::Tuple(elems.into_iter().map(se).collect()),
        ExprKind::Lambda {
            is_mut,
            params,
            return_type,
            body,
        } => ExprKind::Lambda {
            is_mut,
            params: params
                .into_iter()
                .map(|p| Param {
                    name: p.name,
                    type_expr: st(p.type_expr),
                    is_mut: p.is_mut,
                })
                .collect(),
            return_type: return_type.map(|t| st(t)),
            body: sb(body),
        },
        ExprKind::Async(e) => ExprKind::Async(sb(e)),
        ExprKind::Defer(e) => ExprKind::Defer(sb(e)),
        ExprKind::FmtString(parts) => ExprKind::FmtString(
            parts
                .into_iter()
                .map(|p| match p {
                    FmtPart::Literal(s) => FmtPart::Literal(s),
                    FmtPart::Expr(e) => FmtPart::Expr(se(e)),
                })
                .collect(),
        ),
        leaf => leaf,
    }
}

/// Collects all unique `DefKind::Local` DefIds reachable via `Ident` in `e`.
fn collect_local_idents(e: &Expr<Typed>, symbols: &SymbolTable, out: &mut Vec<DefId>) {
    match &e.kind {
        ExprKind::Ident(id) => {
            if matches!(symbols.get(*id).kind, DefKind::Local { .. }) && !out.contains(id) {
                out.push(*id);
            }
        }
        ExprKind::Block(stmts) => {
            for s in stmts {
                collect_local_idents(s, symbols, out);
            }
        }
        ExprKind::Let { value, .. } | ExprKind::Const { value, .. } => {
            collect_local_idents(value, symbols, out);
        }
        ExprKind::Assign { target, value }
        | ExprKind::AddAssign { target, value }
        | ExprKind::SubAssign { target, value }
        | ExprKind::MulAssign { target, value }
        | ExprKind::DivAssign { target, value }
        | ExprKind::ModAssign { target, value }
        | ExprKind::ShrAssign { target, value }
        | ExprKind::ShlAssign { target, value } => {
            collect_local_idents(target, symbols, out);
            collect_local_idents(value, symbols, out);
        }
        ExprKind::Add(a, b)
        | ExprKind::Sub(a, b)
        | ExprKind::Mul(a, b)
        | ExprKind::Div(a, b)
        | ExprKind::Mod(a, b)
        | ExprKind::BitAnd(a, b)
        | ExprKind::BitOr(a, b)
        | ExprKind::BitXor(a, b)
        | ExprKind::Shl(a, b)
        | ExprKind::Shr(a, b)
        | ExprKind::Eq(a, b)
        | ExprKind::Neq(a, b)
        | ExprKind::Lt(a, b)
        | ExprKind::Lte(a, b)
        | ExprKind::Gt(a, b)
        | ExprKind::Gte(a, b)
        | ExprKind::And(a, b)
        | ExprKind::Or(a, b)
        | ExprKind::Index { base: a, index: b } => {
            collect_local_idents(a, symbols, out);
            collect_local_idents(b, symbols, out);
        }
        ExprKind::Not(a)
        | ExprKind::Neg(a)
        | ExprKind::Ref(a)
        | ExprKind::RefMut(a)
        | ExprKind::Deref(a)
        | ExprKind::BitNot(a)
        | ExprKind::Field { base: a, .. }
        | ExprKind::ScopeRes { base: a, .. }
        | ExprKind::Async(a)
        | ExprKind::Defer(a) => {
            collect_local_idents(a, symbols, out);
        }
        ExprKind::Call { callee, args, .. } => {
            collect_local_idents(callee, symbols, out);
            for a in args {
                collect_local_idents(a, symbols, out);
            }
        }
        ExprKind::As { value, .. } => collect_local_idents(value, symbols, out),
        ExprKind::Return(v) => {
            if let Some(e) = v {
                collect_local_idents(e, symbols, out);
            }
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_local_idents(condition, symbols, out);
            collect_local_idents(then_branch, symbols, out);
            if let Some(e) = else_branch {
                collect_local_idents(e, symbols, out);
            }
        }
        ExprKind::While { condition, body } => {
            collect_local_idents(condition, symbols, out);
            collect_local_idents(body, symbols, out);
        }
        ExprKind::For { iterable, body, .. } => {
            collect_local_idents(iterable, symbols, out);
            collect_local_idents(body, symbols, out);
        }
        ExprKind::Match {
            value,
            arms,
            else_arm,
        } => {
            collect_local_idents(value, symbols, out);
            for arm in arms {
                if let Some(g) = &arm.guard {
                    collect_local_idents(g, symbols, out);
                }
                collect_local_idents(&arm.body, symbols, out);
            }
            if let Some(e) = else_arm {
                collect_local_idents(e, symbols, out);
            }
        }
        ExprKind::StructLit { fields, .. } | ExprKind::DuckLit(fields) => {
            for (_, v) in fields {
                collect_local_idents(v, symbols, out);
            }
        }
        ExprKind::Array(elems) | ExprKind::Tuple(elems) => {
            for e in elems {
                collect_local_idents(e, symbols, out);
            }
        }
        ExprKind::Lambda { body, .. } => collect_local_idents(body, symbols, out),
        ExprKind::FmtString(parts) => {
            for p in parts {
                if let FmtPart::Expr(e) = p {
                    collect_local_idents(e, symbols, out);
                }
            }
        }
        _ => {}
    }
}

/// Replaces every `Ident(old)` with `Ident(new)` for all `old` keys in `remap`.
fn remap_idents(e: Expr<Typed>, remap: &HashMap<DefId, DefId>) -> Expr<Typed> {
    let span = e.span;
    let ty = e.ty.clone();
    let kind = match e.kind {
        ExprKind::Ident(id) => ExprKind::Ident(*remap.get(&id).unwrap_or(&id)),
        ExprKind::Block(stmts) => {
            ExprKind::Block(stmts.into_iter().map(|s| remap_idents(s, remap)).collect())
        }
        ExprKind::Let {
            is_mut,
            name,
            type_ann,
            value,
        } => ExprKind::Let {
            is_mut,
            name,
            type_ann,
            value: Box::new(remap_idents(*value, remap)),
        },
        ExprKind::Const {
            name,
            type_ann,
            value,
        } => ExprKind::Const {
            name,
            type_ann,
            value: Box::new(remap_idents(*value, remap)),
        },
        ExprKind::Assign { target, value } => ExprKind::Assign {
            target: Box::new(remap_idents(*target, remap)),
            value: Box::new(remap_idents(*value, remap)),
        },
        ExprKind::AddAssign { target, value } => ExprKind::AddAssign {
            target: Box::new(remap_idents(*target, remap)),
            value: Box::new(remap_idents(*value, remap)),
        },
        ExprKind::SubAssign { target, value } => ExprKind::SubAssign {
            target: Box::new(remap_idents(*target, remap)),
            value: Box::new(remap_idents(*value, remap)),
        },
        ExprKind::MulAssign { target, value } => ExprKind::MulAssign {
            target: Box::new(remap_idents(*target, remap)),
            value: Box::new(remap_idents(*value, remap)),
        },
        ExprKind::DivAssign { target, value } => ExprKind::DivAssign {
            target: Box::new(remap_idents(*target, remap)),
            value: Box::new(remap_idents(*value, remap)),
        },
        ExprKind::ModAssign { target, value } => ExprKind::ModAssign {
            target: Box::new(remap_idents(*target, remap)),
            value: Box::new(remap_idents(*value, remap)),
        },
        ExprKind::ShrAssign { target, value } => ExprKind::ShrAssign {
            target: Box::new(remap_idents(*target, remap)),
            value: Box::new(remap_idents(*value, remap)),
        },
        ExprKind::ShlAssign { target, value } => ExprKind::ShlAssign {
            target: Box::new(remap_idents(*target, remap)),
            value: Box::new(remap_idents(*value, remap)),
        },
        ExprKind::Add(a, b) => ExprKind::Add(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::Sub(a, b) => ExprKind::Sub(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::Mul(a, b) => ExprKind::Mul(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::Div(a, b) => ExprKind::Div(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::Mod(a, b) => ExprKind::Mod(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::BitAnd(a, b) => ExprKind::BitAnd(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::BitOr(a, b) => ExprKind::BitOr(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::BitXor(a, b) => ExprKind::BitXor(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::BitNot(a) => ExprKind::BitNot(Box::new(remap_idents(*a, remap))),
        ExprKind::Shl(a, b) => ExprKind::Shl(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::Shr(a, b) => ExprKind::Shr(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::Eq(a, b) => ExprKind::Eq(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::Neq(a, b) => ExprKind::Neq(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::Lt(a, b) => ExprKind::Lt(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::Lte(a, b) => ExprKind::Lte(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::Gt(a, b) => ExprKind::Gt(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::Gte(a, b) => ExprKind::Gte(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::And(a, b) => ExprKind::And(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::Or(a, b) => ExprKind::Or(
            Box::new(remap_idents(*a, remap)),
            Box::new(remap_idents(*b, remap)),
        ),
        ExprKind::Not(a) => ExprKind::Not(Box::new(remap_idents(*a, remap))),
        ExprKind::Neg(a) => ExprKind::Neg(Box::new(remap_idents(*a, remap))),
        ExprKind::Ref(a) => ExprKind::Ref(Box::new(remap_idents(*a, remap))),
        ExprKind::RefMut(a) => ExprKind::RefMut(Box::new(remap_idents(*a, remap))),
        ExprKind::Deref(a) => ExprKind::Deref(Box::new(remap_idents(*a, remap))),
        ExprKind::Field { base, field } => ExprKind::Field {
            base: Box::new(remap_idents(*base, remap)),
            field,
        },
        ExprKind::Index { base, index } => ExprKind::Index {
            base: Box::new(remap_idents(*base, remap)),
            index: Box::new(remap_idents(*index, remap)),
        },
        ExprKind::ScopeRes { base, member } => ExprKind::ScopeRes {
            base: Box::new(remap_idents(*base, remap)),
            member,
        },
        ExprKind::Call {
            callee,
            type_params,
            args,
        } => ExprKind::Call {
            callee: Box::new(remap_idents(*callee, remap)),
            type_params,
            args: args.into_iter().map(|a| remap_idents(a, remap)).collect(),
        },
        ExprKind::As { value, type_expr } => ExprKind::As {
            value: Box::new(remap_idents(*value, remap)),
            type_expr,
        },
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => ExprKind::If {
            condition: Box::new(remap_idents(*condition, remap)),
            then_branch: Box::new(remap_idents(*then_branch, remap)),
            else_branch: else_branch.map(|e| Box::new(remap_idents(*e, remap))),
        },
        ExprKind::While { condition, body } => ExprKind::While {
            condition: Box::new(remap_idents(*condition, remap)),
            body: Box::new(remap_idents(*body, remap)),
        },
        ExprKind::For {
            binding,
            is_mut,
            iterable,
            body,
        } => ExprKind::For {
            binding,
            is_mut,
            iterable: Box::new(remap_idents(*iterable, remap)),
            body: Box::new(remap_idents(*body, remap)),
        },
        ExprKind::Return(v) => ExprKind::Return(v.map(|e| Box::new(remap_idents(*e, remap)))),
        ExprKind::Match {
            value,
            arms,
            else_arm,
        } => ExprKind::Match {
            value: Box::new(remap_idents(*value, remap)),
            arms: arms
                .into_iter()
                .map(|arm| MatchArm {
                    guard: arm.guard.map(|g| Box::new(remap_idents(*g, remap))),
                    body: remap_idents(arm.body, remap),
                    ..arm
                })
                .collect(),
            else_arm: else_arm.map(|e| Box::new(remap_idents(*e, remap))),
        },
        ExprKind::StructLit {
            name,
            type_params,
            fields,
        } => ExprKind::StructLit {
            name,
            type_params,
            fields: fields
                .into_iter()
                .map(|(l, v)| (l, remap_idents(v, remap)))
                .collect(),
        },
        ExprKind::DuckLit(fields) => ExprKind::DuckLit(
            fields
                .into_iter()
                .map(|(l, v)| (l, remap_idents(v, remap)))
                .collect(),
        ),
        ExprKind::Array(elems) => {
            ExprKind::Array(elems.into_iter().map(|e| remap_idents(e, remap)).collect())
        }
        ExprKind::Tuple(elems) => {
            ExprKind::Tuple(elems.into_iter().map(|e| remap_idents(e, remap)).collect())
        }
        ExprKind::Lambda {
            is_mut,
            params,
            return_type,
            body,
        } => ExprKind::Lambda {
            is_mut,
            params,
            return_type,
            body: Box::new(remap_idents(*body, remap)),
        },
        ExprKind::Async(e) => ExprKind::Async(Box::new(remap_idents(*e, remap))),
        ExprKind::Defer(e) => ExprKind::Defer(Box::new(remap_idents(*e, remap))),
        ExprKind::FmtString(parts) => ExprKind::FmtString(
            parts
                .into_iter()
                .map(|p| match p {
                    FmtPart::Literal(s) => FmtPart::Literal(s),
                    FmtPart::Expr(e) => FmtPart::Expr(remap_idents(e, remap)),
                })
                .collect(),
        ),
        ExprKind::Jsx(node) => ExprKind::Jsx(Box::new(remap_jsx_idents(*node, remap))),
        leaf => leaf,
    };
    Expr::typed(kind, ty, span)
}

fn remap_jsx_idents(node: JsxNode<Typed>, remap: &HashMap<DefId, DefId>) -> JsxNode<Typed> {
    match node {
        JsxNode::Text(s) => JsxNode::Text(s),
        JsxNode::Expr(e) => JsxNode::Expr(Box::new(remap_idents(*e, remap))),
        JsxNode::Element {
            tag,
            attrs,
            children,
        } => JsxNode::Element {
            tag,
            attrs: attrs
                .into_iter()
                .map(|a| JsxAttr {
                    name: a.name,
                    value: match a.value {
                        JsxAttrValue::Bool => JsxAttrValue::Bool,
                        JsxAttrValue::Str(s) => JsxAttrValue::Str(s),
                        JsxAttrValue::Expr(e) => {
                            JsxAttrValue::Expr(Box::new(remap_idents(*e, remap)))
                        }
                    },
                })
                .collect(),
            children: children
                .into_iter()
                .map(|c| remap_jsx_idents(c, remap))
                .collect(),
        },
    }
}
