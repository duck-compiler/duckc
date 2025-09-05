use std::collections::{HashMap, HashSet};

use chumsky::container::Container;

use crate::{
    parse::{
        SS, Spanned,
        function_parser::{FunctionDefintion, LambdaFunctionExpr},
        source_file_parser::SourceFile,
        struct_parser::StructDefinition,
        type_parser::{Duck, TypeDefinition, TypeExpr},
        value_parser::{Assignment, Declaration, ValFmtStringContents, ValueExpr},
    },
    semantics::ident_mangler::mangle,
    tags::Tag,
};

#[derive(Debug, Clone)]
pub enum GenericDefinition {
    Function(FunctionDefintion),
    Type(TypeDefinition),
    Struct(StructDefinition),
}

impl GenericDefinition {
    pub fn generics_names(&self) -> Vec<String> {
        return match self {
            Self::Function(function_def) => function_def
                .generics
                .as_ref()
                .expect("FunctionDefinition shouldn't be wrapped inside a GenericDefinition, when it doesn't have any generics.")
                .iter()
                .map(|generic| generic.0.name.clone())
                .collect::<Vec<_>>(),
            Self::Type(type_def) => type_def
                .generics
                .as_ref()
                .expect("TypeDefinition shouldn't be wrapped inside a GenericDefinition, when it doesn't have any generics.")
                .iter()
                .map(|generic| generic.0.name.clone())
                .collect::<Vec<_>>(),
            Self::Struct(struct_def) => struct_def
                .generics
                .as_ref()
                .expect("StructDefinition shouldn't be wrapped inside a GenericDefinition, when it doesn't have any generics.")
                .iter()
                .map(|generic| generic.0.name.clone())
                .collect::<Vec<_>>(),
        };
    }
}

#[derive(Debug, Clone)]
pub struct TypeEnv {
    pub identifier_types: Vec<HashMap<String, TypeExpr>>,
    pub type_aliases: Vec<HashMap<String, TypeExpr>>,
    pub all_types: Vec<TypeExpr>,

    pub function_definitions: Vec<FunctionDefintion>,
    pub struct_definitions: Vec<StructDefinition>,
    pub generic_fns_generated: Vec<FunctionDefintion>,
    pub generic_structs_generated: Vec<StructDefinition>,
    pub generic_methods_generated: HashMap<String, Vec<FunctionDefintion>>,
    pub prevent_struct_generation: HashSet<String>,
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self {
            identifier_types: vec![HashMap::new()],
            type_aliases: vec![HashMap::new()],
            all_types: vec![],
            function_definitions: Vec::new(),
            struct_definitions: Vec::new(),
            generic_fns_generated: Vec::new(),
            generic_structs_generated: Vec::new(),
            generic_methods_generated: HashMap::new(),
            prevent_struct_generation: HashSet::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypesSummary {
    pub types_used: Vec<TypeExpr>,
    pub param_names_used: Vec<String>,
}

impl TypeEnv {
    pub fn get_struct_def<'a>(&'a self, name: &str) -> &'a StructDefinition {
        self.struct_definitions
            .iter()
            .chain(self.generic_structs_generated.iter())
            .find(|x| x.name.as_str() == name)
            .unwrap_or_else(|| panic!("Could not find struct {name}"))
    }

    pub fn get_struct_def_mut<'a>(&'a mut self, name: &str) -> &'a mut StructDefinition {
        self.struct_definitions
            .iter_mut()
            .find(|x| x.name.as_str() == name)
            .unwrap_or_else(|| panic!("Could not find struct {name}"))
    }

    pub fn get_generic_methods(&mut self, type_name: String) -> &mut Vec<FunctionDefintion> {
        self.generic_methods_generated.entry(type_name).or_default()
    }

    pub fn has_generic_method(&self, type_name: &str, method_name: &str) -> bool {
        self.generic_methods_generated
            .get(type_name)
            .is_some_and(|x| x.iter().any(|x| x.name == method_name))
    }

    pub fn push_type_aliases(&mut self) {
        let cloned_hash_map = self
            .type_aliases
            .last()
            .expect("Expect at least one env.")
            .clone();
        self.type_aliases.push(cloned_hash_map);
    }

    pub fn pop_type_aliases(&mut self) {
        self.type_aliases.pop();
    }

    pub fn push_identifier_types(&mut self) {
        let cloned_hash_map = self
            .identifier_types
            .last()
            .expect("Expect at least one env.")
            .clone();
        self.identifier_types.push(cloned_hash_map);
    }

    pub fn pop_identifier_types(&mut self) {
        self.identifier_types.pop();
    }

    pub fn insert_identifier_type(&mut self, identifier: String, type_expr: TypeExpr) {
        self.insert_type(type_expr.clone());
        self.identifier_types
            .last_mut()
            .expect("At least one env should exist. :(")
            .insert(identifier, type_expr);
    }

    pub fn insert_type(&mut self, type_expr: TypeExpr) -> TypeExpr {
        self.all_types.push(type_expr.clone());
        return type_expr;
    }

    pub fn get_identifier_type(&self, identifier: String) -> Option<TypeExpr> {
        self.identifier_types
            .last()
            .expect("At least one env should exist. :(")
            .get(&identifier)
            .cloned()
    }

    pub fn insert_type_alias(&mut self, alias: String, type_expr: TypeExpr) {
        self.type_aliases
            .last_mut()
            .expect("At least one type aliases hashmap should exist. :(")
            .insert(alias, type_expr);
    }

    pub fn try_resolve_type_expr(&self, expr: &TypeExpr) -> TypeExpr {
        match expr {
            TypeExpr::TypeName(_, name, _) | TypeExpr::TypeNameInternal(name) => self
                .try_resolve_type_alias(name)
                .unwrap_or_else(|| expr.clone()),
            _ => expr.clone(),
        }
    }

    pub fn try_resolve_type_alias(&self, alias: &String) -> Option<TypeExpr> {
        for scope in self.type_aliases.iter().rev() {
            if let Some(type_expr) = scope.get(alias) {
                let mut res = type_expr.clone();
                if let TypeExpr::Or(types) = &mut res {
                    for ty in types {
                        if let TypeExpr::TypeName(_, name, _type_params) = &ty.0 {
                            ty.0 = self.resolve_type_alias(name);
                        }
                    }
                }

                return Some(res);
            }
        }

        if let Some(def) = self
            .generic_structs_generated
            .iter()
            .find(|x| x.name.as_str() == alias.as_str())
        {
            return Some(TypeExpr::Struct(def.name.clone()));
        }

        None
    }

    pub fn resolve_type_alias(&self, alias: &String) -> TypeExpr {
        self.try_resolve_type_alias(alias).unwrap_or_else(|| {
            panic!(
                "Couldn't resolve type alias {alias} on stack #{}",
                self.type_aliases.len()
            )
        })
    }

    fn flatten_types(
        &mut self,
        type_expr: &mut TypeExpr,
        param_names_used: &mut Vec<String>,
    ) -> Vec<TypeExpr> {
        let mut found = vec![];

        match type_expr {
            TypeExpr::Duck(duck) => duck.fields.iter_mut().for_each(|field| {
                param_names_used.push(field.name.clone());

                if !field.type_expr.0.has_subtypes() {
                    found.push(field.type_expr.0.clone());
                    return;
                }

                let mut type_expr = field.type_expr.0.clone();
                let mut flattened_types_from_type_expr =
                    self.flatten_types(&mut type_expr, param_names_used);

                found.push(type_expr.clone());
                found.append(&mut flattened_types_from_type_expr);

                if type_expr.is_object_like() {
                    let type_name = type_expr.as_clean_go_type_name(self);
                    let is_duck = matches!(type_expr, TypeExpr::Duck(_));
                    field.type_expr = (
                        if is_duck {
                            type_expr
                        } else {
                            TypeExpr::TypeNameInternal(type_name)
                        },
                        field.type_expr.1,
                    );
                }
            }),
            TypeExpr::Tuple(types) => types.iter_mut().for_each(|type_expr| {
                found.extend(self.flatten_types(&mut type_expr.0, param_names_used));
            }),
            TypeExpr::Fun(params, return_type) => {
                params.iter_mut().for_each(|param| {
                    found.extend(self.flatten_types(&mut param.1.0, param_names_used))
                });

                if let Some(type_expr) = return_type.as_mut() {
                    found.extend(self.flatten_types(&mut type_expr.0, param_names_used));
                }
            }
            TypeExpr::Or(types) => types.iter_mut().for_each(|(type_expr, ..)| {
                found.push(type_expr.clone());
                found.extend(self.flatten_types(type_expr, param_names_used));
            }),
            _ => {
                found.push(type_expr.clone());
            }
        }

        found
    }

    pub fn summarize(&mut self) -> TypesSummary {
        let mut all_types = self.all_types.clone();
        // dbg!(all_types.iter().filter(|x| x.is_struct() && x.type_id(self).contains("Xyz")).collect::<Vec<_>>());
        all_types.extend(TypeExpr::primitives());
        all_types.push(TypeExpr::Tuple(vec![]));
        let mut param_names_used = Vec::new();

        let mut to_push = Vec::new();
        all_types.iter_mut().for_each(|type_expr| {
            to_push.append(&mut self.flatten_types(type_expr, &mut param_names_used));
        });

        all_types.append(&mut to_push);

        // todo: fix this panic workaround
        let org = std::panic::take_hook();
        std::panic::set_hook(Box::new(|_| {}));
        all_types.retain(|e| {
            let mut cloned = self.clone();
            std::panic::catch_unwind(move || {
                e.type_id(&mut cloned);
                e.as_clean_go_type_name(&mut cloned);
                e.as_go_type_annotation(&mut cloned);
                e.as_clean_user_faced_type_name();
            })
            .is_ok()
        });
        std::panic::set_hook(org);

        all_types.sort_by_key(|type_expr| type_expr.type_id(self));
        all_types.dedup_by_key(|type_expr| type_expr.type_id(self));

        param_names_used.dedup();

        return TypesSummary {
            types_used: all_types,
            param_names_used,
        };
    }
}

fn resolve_all_aliases_type_expr(expr: &mut TypeExpr, env: &mut TypeEnv) {
    match expr {
        TypeExpr::RawTypeName(_, typename, generic_params) => {
            if typename.len() != 1 {
                panic!()
            }

            if let Some(generic_params) = generic_params {
                for (g, _) in generic_params {
                    resolve_all_aliases_type_expr(g, env);
                }
            }

            *expr = env.resolve_type_alias(typename.first().unwrap());
        }
        TypeExpr::Duck(Duck { fields }) => {
            fields.sort_by_key(|x| x.name.clone());
            for field in fields {
                resolve_all_aliases_type_expr(&mut field.type_expr.0, env);
            }
        }
        TypeExpr::Array(d) => resolve_all_aliases_type_expr(&mut d.0, env),
        TypeExpr::Fun(params, return_type) => {
            if let Some(r) = return_type {
                resolve_all_aliases_type_expr(&mut r.0, env);
            }

            params.iter_mut().for_each(|(_, x)| {
                resolve_all_aliases_type_expr(&mut x.0, env);
            });
        }
        TypeExpr::Or(exprs) => {
            for expr in exprs {
                resolve_all_aliases_type_expr(&mut expr.0, env);
            }
        }
        TypeExpr::Tuple(fields) => {
            for field in fields.iter_mut() {
                resolve_all_aliases_type_expr(&mut field.0, env);
            }
        }
        TypeExpr::TypeName(_, name, _) => {
            *expr = env.resolve_type_alias(name);
        }
        _ => {}
    }
}

fn replace_generics_in_struct_definition(
    def: &mut StructDefinition,
    generics: &HashMap<String, TypeExpr>,
) {
    for f in def.fields.iter_mut() {
        replace_generics_in_type_expr(&mut f.type_expr.0, generics);
    }

    for m in def.methods.iter_mut() {
        for t in m.return_type.iter_mut().map(|x| &mut x.0).chain(
            m.params
                .iter_mut()
                .flat_map(|x| x.iter_mut().map(|x| &mut x.1.0)),
        ) {
            replace_generics_in_type_expr(t, generics);
        }
        replace_generics_in_value_expr(&mut m.value_expr.0, generics);
    }
}

fn instantiate_generics_type_expr(expr: &mut TypeExpr, type_env: &mut TypeEnv) {
    match expr {
        TypeExpr::Alias(alias) => {
            instantiate_generics_type_expr(&mut alias.type_expression.0, type_env);
        }
        TypeExpr::Array(t) => {
            instantiate_generics_type_expr(&mut t.0, type_env);
        }
        TypeExpr::Duck(d) => {
            for f in &mut d.fields {
                instantiate_generics_type_expr(&mut f.type_expr.0, type_env);
            }
        }
        TypeExpr::Fun(params, ret) => {
            for p in params {
                instantiate_generics_type_expr(&mut p.1.0, type_env);
            }
            if let Some(ret) = ret {
                instantiate_generics_type_expr(&mut ret.0, type_env);
            }
        }
        TypeExpr::Or(contents) => {
            for c in contents {
                instantiate_generics_type_expr(&mut c.0, type_env);
            }
        }
        TypeExpr::Tuple(fields) => {
            for f in fields {
                instantiate_generics_type_expr(&mut f.0, type_env);
            }
        }
        TypeExpr::TypeName(_, name, type_params) => {
            let (mut new_type, mut new_type_params) =
                resolve_by_string(name.as_str(), type_params.as_ref().cloned(), type_env);
            if let Some(e) = new_type.as_mut() {
                if let TypeExpr::Struct(def) = e {
                    if let Some(new_type_params) = new_type_params.as_mut() {
                        for (new_param, _) in new_type_params.iter_mut() {
                            instantiate_generics_type_expr(new_param, type_env);
                        }
                    } else {
                        new_type_params = Some(vec![]);
                    }

                    let def = type_env.get_struct_def(def.as_str()).clone();

                    let mangled_name = mangle_generics_name(
                        def.name.as_str(),
                        new_type_params.as_ref().unwrap_or(&vec![]).as_slice(),
                        type_env,
                    );

                    if !type_env.prevent_struct_generation.contains(&mangled_name) {
                        type_env
                            .prevent_struct_generation
                            .push(mangled_name.clone());
                        let generics_instance = def
                            .generics
                            .as_ref()
                            .unwrap_or(&vec![])
                            .iter()
                            .map(|x| &x.0.name)
                            .zip(
                                new_type_params
                                    .as_ref()
                                    .unwrap_or(&vec![])
                                    .iter()
                                    .map(|x| &x.0),
                            )
                            .fold(HashMap::new(), |mut acc, (param_name, param_inst)| {
                                acc.insert(param_name.clone(), param_inst.clone());
                                acc
                            });

                        let mut cloned_def = def.clone();
                        cloned_def.name = mangled_name.clone();
                        cloned_def.generics = None;
                        replace_generics_in_struct_definition(&mut cloned_def, &generics_instance);

                        // println!("AAAAA replaced: {cloned_def:?}");
                        type_env.generic_structs_generated.push(cloned_def.clone());

                        for f in &mut cloned_def.fields {
                            instantiate_generics_type_expr(&mut f.type_expr.0, type_env);
                        }

                        for m in &mut cloned_def.methods {
                            if m.generics.is_some() {
                                continue;
                            }
                            for ty in m.return_type.iter_mut().chain(
                                m.params
                                    .iter_mut()
                                    .flat_map(|x| x.iter_mut())
                                    .map(|x| &mut x.1),
                            ) {
                                instantiate_generics_type_expr(&mut ty.0, type_env);
                            }
                            instantiate_generics_value_expr(&mut m.value_expr.0, type_env);
                        }

                        // println!("AAAA  pushing {cloned_def:?}");
                        type_env
                            .generic_structs_generated
                            .retain(|x| x.name != mangled_name);
                        type_env.generic_structs_generated.push(cloned_def.clone());
                        type_env.struct_definitions.push(cloned_def);
                        *e = TypeExpr::Struct(mangled_name);
                    } else {
                        *e = TypeExpr::Struct(mangled_name);
                    }
                }
                *expr = e.clone();
                instantiate_generics_type_expr(expr, type_env);
            }
        }
        TypeExpr::Any
        | TypeExpr::Bool
        | TypeExpr::Char
        | TypeExpr::ConstBool(..)
        | TypeExpr::ConstInt(..)
        | TypeExpr::ConstString(..)
        | TypeExpr::Float
        | TypeExpr::Go(..)
        | TypeExpr::RawTypeName(..)
        | TypeExpr::String
        | TypeExpr::Int
        | TypeExpr::TypeNameInternal(..)
        | TypeExpr::Struct(..)
        | TypeExpr::InlineGo => {}
    }
}

fn replace_generics_in_value_expr(expr: &mut ValueExpr, set_params: &HashMap<String, TypeExpr>) {
    match expr {
        ValueExpr::Add(lhs, rhs) | ValueExpr::Mul(lhs, rhs) | ValueExpr::Equals(lhs, rhs) => {
            replace_generics_in_value_expr(&mut lhs.0, set_params);
            replace_generics_in_value_expr(&mut rhs.0, set_params);
        }
        ValueExpr::BoolNegate(e) | ValueExpr::Return(Some(e)) => {
            replace_generics_in_value_expr(&mut e.0, set_params)
        }
        ValueExpr::Array(t, exprs) => {
            if let Some(t) = t {
                replace_generics_in_type_expr(&mut t.0, set_params);
            }

            for e in exprs {
                replace_generics_in_value_expr(&mut e.0, set_params);
            }
        }
        ValueExpr::ArrayAccess(target, index) => {
            replace_generics_in_value_expr(&mut target.0, set_params);
            replace_generics_in_value_expr(&mut index.0, set_params);
        }
        ValueExpr::Block(exprs) => {
            for e in exprs {
                replace_generics_in_value_expr(&mut e.0, set_params);
            }
        }
        ValueExpr::Duck(def) => {
            for (_, expr) in def {
                replace_generics_in_value_expr(&mut expr.0, set_params);
            }
        }
        ValueExpr::FieldAccess {
            target_obj,
            field_name: _,
        } => {
            replace_generics_in_value_expr(&mut target_obj.0, set_params);
        }
        ValueExpr::FormattedString(contents) => {
            for c in contents {
                if let ValFmtStringContents::Expr(e) = c {
                    replace_generics_in_value_expr(&mut e.0, set_params);
                }
            }
        }
        ValueExpr::FunctionCall {
            target,
            params,
            type_params,
        } => {
            for v in [&mut target.0]
                .into_iter()
                .chain(params.iter_mut().map(|x| &mut x.0))
            {
                replace_generics_in_value_expr(v, set_params);
            }
            for t in type_params.iter_mut().flat_map(|x| x.iter_mut()) {
                replace_generics_in_type_expr(&mut t.0, set_params);
            }
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            replace_generics_in_value_expr(&mut condition.0, set_params);
            replace_generics_in_value_expr(&mut then.0, set_params);
            if let Some(r#else) = r#else {
                replace_generics_in_value_expr(&mut r#else.0, set_params);
            }
        }
        ValueExpr::Lambda(def) => {
            for p in &mut def.params {
                replace_generics_in_type_expr(&mut p.1.0, set_params);
            }
            if let Some(return_type) = def.return_type.as_mut() {
                replace_generics_in_type_expr(&mut return_type.0, set_params);
            }
            replace_generics_in_value_expr(&mut def.value_expr.0, set_params);
        }
        ValueExpr::Struct {
            name,
            fields,
            type_params,
        } => {
            if let Some(replacement) = set_params.get(name) {
                match replacement {
                    TypeExpr::TypeName(_, new_name, _new_params) => {
                        *name = new_name.clone();
                        // *type_params = new_params.as_ref().cloned();
                    }
                    _ => panic!("invalid"),
                }
            }

            for f in fields {
                replace_generics_in_value_expr(&mut f.1.0, set_params);
            }

            for t in type_params.as_mut().unwrap_or(&mut vec![]).iter_mut() {
                replace_generics_in_type_expr(&mut t.0, set_params);
            }
        }
        ValueExpr::Tuple(fields) => {
            for f in fields {
                replace_generics_in_value_expr(&mut f.0, set_params);
            }
        }
        ValueExpr::While { condition, body } => {
            replace_generics_in_value_expr(&mut condition.0, set_params);
            replace_generics_in_value_expr(&mut body.0, set_params);
        }
        ValueExpr::VarDecl(decl) => {
            replace_generics_in_type_expr(&mut decl.0.type_expr.0, set_params);
            if let Some(init) = decl.0.initializer.as_mut() {
                replace_generics_in_value_expr(&mut init.0, set_params);
            }
        }
        ValueExpr::VarAssign(a) => {
            replace_generics_in_value_expr(&mut a.0.target.0, set_params);
            replace_generics_in_value_expr(&mut a.0.value_expr.0, set_params);
        }
        ValueExpr::Match { value_expr, arms } => {
            replace_generics_in_value_expr(&mut value_expr.0, set_params);
            for arm in arms {
                replace_generics_in_type_expr(&mut arm.type_case.0, set_params);
                replace_generics_in_value_expr(&mut arm.value_expr.0, set_params);
            }
        }
        ValueExpr::Bool(..)
        | ValueExpr::Break
        | ValueExpr::Char(..)
        | ValueExpr::String(..)
        | ValueExpr::Continue
        | ValueExpr::Float(..)
        | ValueExpr::InlineGo(..)
        | ValueExpr::Int(..)
        | ValueExpr::RawVariable(..)
        | ValueExpr::Return(..)
        | ValueExpr::Variable(..) => {}
    }
}

fn replace_generics_in_type_expr(expr: &mut TypeExpr, set_params: &HashMap<String, TypeExpr>) {
    match expr {
        TypeExpr::Alias(alias) => {
            replace_generics_in_type_expr(&mut alias.type_expression.0, set_params);
        }
        TypeExpr::Array(t) => {
            replace_generics_in_type_expr(&mut t.0, set_params);
        }
        TypeExpr::Duck(d) => {
            for f in &mut d.fields {
                replace_generics_in_type_expr(&mut f.type_expr.0, set_params);
            }
        }
        TypeExpr::Fun(params, ret) => {
            for p in params {
                replace_generics_in_type_expr(&mut p.1.0, set_params);
            }
            if let Some(ret) = ret {
                replace_generics_in_type_expr(&mut ret.0, set_params);
            }
        }
        TypeExpr::Or(contents) => {
            for c in contents {
                replace_generics_in_type_expr(&mut c.0, set_params);
            }
        }
        TypeExpr::Tuple(fields) => {
            for f in fields {
                replace_generics_in_type_expr(&mut f.0, set_params);
            }
        }
        TypeExpr::TypeName(_, name, generics) => {
            if let Some(generics) = generics {
                for (g, _) in generics {
                    replace_generics_in_type_expr(g, set_params);
                }
            }
            if let Some(replacement) = set_params.get(name) {
                *expr = replacement.clone();
            }
        }
        TypeExpr::Any
        | TypeExpr::Bool
        | TypeExpr::Char
        | TypeExpr::ConstBool(..)
        | TypeExpr::ConstInt(..)
        | TypeExpr::ConstString(..)
        | TypeExpr::Float
        | TypeExpr::Go(..)
        | TypeExpr::RawTypeName(..)
        | TypeExpr::String
        | TypeExpr::Int
        | TypeExpr::Struct(..)
        | TypeExpr::TypeNameInternal(..)
        | TypeExpr::InlineGo => {}
    }
}

fn resolve_by_string(
    s: &str,
    mut type_params: Option<Vec<(TypeExpr, SS)>>,
    type_env: &mut TypeEnv,
) -> (Option<TypeExpr>, Option<Vec<(TypeExpr, SS)>>) {
    let mut resolved = type_env.try_resolve_type_alias(&s.to_string());
    while let Some(TypeExpr::Alias(ref def)) = resolved {
        let mut gen_instance_map = HashMap::new();
        if let Some(generics) = &def.generics
            && let Some(ref user_generics) = type_params
        {
            for (generic_name, val_to_set) in generics
                .iter()
                .map(|x| &x.0.name)
                .zip(user_generics.iter().map(|x| &x.0))
            {
                gen_instance_map.insert(generic_name.to_owned(), val_to_set.clone());
            }
        }
        let mut rhs = def.type_expression.0.clone();
        // println!("def {rhs:?} {gen_instance_map:?}");
        replace_generics_in_type_expr(&mut rhs, &gen_instance_map);
        // println!("replaced def {rhs:?}");
        match &rhs {
            TypeExpr::TypeName(_, typename_name, typename_generics) => {
                let next = type_env.try_resolve_type_alias(typename_name);
                if let Some(next) = next {
                    match next {
                        TypeExpr::Alias(..) => {
                            type_params = typename_generics.as_ref().cloned();
                            resolved = Some(next);
                            println!("case alias");
                        }
                        _ => {
                            return (Some(next), typename_generics.as_ref().cloned());
                        }
                    }
                } else {
                    break;
                }
            }
            _ => {
                return (Some(rhs), type_params);
            }
        }
    }
    (resolved, type_params)
}

fn mangle_generics_name(
    base: &str,
    params: &[Spanned<TypeExpr>],
    type_env: &mut TypeEnv,
) -> String {
    format!("{base}{}", {
        let r = params
            .iter()
            .map(|x| x.0.as_clean_go_type_name(type_env))
            .collect::<Vec<_>>()
            .join("_");
        if r.is_empty() { r } else { format!("_{r}") }
    })
}

fn instantiate_generics_value_expr(expr: &mut ValueExpr, type_env: &mut TypeEnv) {
    match expr {
        ValueExpr::Add(lhs, rhs) | ValueExpr::Mul(lhs, rhs) | ValueExpr::Equals(lhs, rhs) => {
            instantiate_generics_value_expr(&mut lhs.0, type_env);
            instantiate_generics_value_expr(&mut rhs.0, type_env);
        }
        ValueExpr::BoolNegate(e) | ValueExpr::Return(Some(e)) => {
            instantiate_generics_value_expr(&mut e.0, type_env)
        }
        ValueExpr::Array(t, exprs) => {
            if let Some(t) = t {
                instantiate_generics_type_expr(&mut t.0, type_env);
            }

            for e in exprs {
                instantiate_generics_value_expr(&mut e.0, type_env);
            }
        }
        ValueExpr::ArrayAccess(target, index) => {
            instantiate_generics_value_expr(&mut target.0, type_env);
            instantiate_generics_value_expr(&mut index.0, type_env);
        }
        ValueExpr::Block(exprs) => {
            for e in exprs {
                instantiate_generics_value_expr(&mut e.0, type_env);
            }
        }
        ValueExpr::Duck(def) => {
            for (_, expr) in def {
                instantiate_generics_value_expr(&mut expr.0, type_env);
            }
        }
        ValueExpr::FieldAccess {
            target_obj,
            field_name: _,
        } => {
            instantiate_generics_value_expr(&mut target_obj.0, type_env);
        }
        ValueExpr::FormattedString(contents) => {
            for c in contents {
                if let ValFmtStringContents::Expr(e) = c {
                    instantiate_generics_value_expr(&mut e.0, type_env);
                }
            }
        }
        ValueExpr::FunctionCall {
            target,
            params,
            type_params,
        } => {
            instantiate_generics_value_expr(&mut target.0, type_env);
            for t in type_params.iter_mut().flat_map(|x| x.iter_mut()) {
                instantiate_generics_type_expr(&mut t.0, type_env);
            }
            for p in params {
                instantiate_generics_value_expr(&mut p.0, type_env);
            }

            if let ValueExpr::Variable(_, var_name, _) = &mut target.0 {
                let mangled_name = mangle_generics_name(
                    var_name.as_str(),
                    type_params.as_ref().unwrap_or(&vec![]).as_slice(),
                    type_env,
                );
                let fn_def = type_env
                    .function_definitions
                    .iter()
                    .find(|x| x.name.as_str() == var_name.as_str());
                if let Some(fn_def) = fn_def {
                    if !type_env.prevent_struct_generation.contains(&mangled_name) {
                        type_env
                            .prevent_struct_generation
                            .push(mangled_name.clone());

                        let generics_instance = fn_def
                            .generics
                            .as_ref()
                            .unwrap_or(&vec![])
                            .iter()
                            .map(|x| &x.0.name)
                            .zip(type_params.as_ref().unwrap_or(&vec![]).iter().map(|x| &x.0))
                            .fold(HashMap::new(), |mut acc, (param_name, param_inst)| {
                                acc.insert(param_name.clone(), param_inst.clone());
                                acc
                            });

                        let mut cloned_def = fn_def.clone();
                        cloned_def.name = mangled_name.clone();
                        cloned_def.generics = None;

                        for t in cloned_def
                            .params
                            .iter_mut()
                            .flat_map(|x| x.iter_mut().map(|x| &mut x.1.0))
                            .chain(cloned_def.return_type.iter_mut().map(|x| &mut x.0))
                        {
                            replace_generics_in_type_expr(t, &generics_instance);
                            instantiate_generics_type_expr(t, type_env);
                        }

                        replace_generics_in_value_expr(
                            &mut cloned_def.value_expr.0,
                            &generics_instance,
                        );
                        instantiate_generics_value_expr(&mut cloned_def.value_expr.0, type_env);

                        for m in type_env.identifier_types.iter_mut() {
                            m.insert(
                                cloned_def.name.clone(),
                                TypeExpr::Fun(
                                    cloned_def
                                        .params
                                        .clone()
                                        .map(|x| {
                                            x.iter()
                                                .map(|x| (Some(x.0.clone()), x.1.clone()))
                                                .collect()
                                        })
                                        .unwrap_or_default(),
                                    cloned_def.return_type.clone().map(Box::new),
                                ),
                            );
                        }

                        type_env.generic_fns_generated.push(cloned_def);
                        *var_name = mangled_name;
                        *type_params = None;
                    } else {
                        *var_name = mangled_name;
                        *type_params = None;
                    }
                }
            }
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            instantiate_generics_value_expr(&mut condition.0, type_env);
            instantiate_generics_value_expr(&mut then.0, type_env);
            if let Some(r#else) = r#else {
                instantiate_generics_value_expr(&mut r#else.0, type_env);
            }
        }
        ValueExpr::Lambda(def) => {
            for p in &mut def.params {
                instantiate_generics_type_expr(&mut p.1.0, type_env);
            }
            if let Some(return_type) = def.return_type.as_mut() {
                instantiate_generics_type_expr(&mut return_type.0, type_env);
            }
            instantiate_generics_value_expr(&mut def.value_expr.0, type_env);
        }
        ValueExpr::Struct {
            name,
            fields,
            type_params,
        } => {
            let (new_type, mut new_type_params) =
                resolve_by_string(name.as_str(), type_params.clone(), type_env);

            if let Some(new_type_params) = new_type_params.as_mut() {
                for (new_param, _) in new_type_params.iter_mut() {
                    instantiate_generics_type_expr(new_param, type_env);
                }
                *type_params = Some(new_type_params.clone());
            }

            for field in fields {
                instantiate_generics_value_expr(&mut field.1.0, type_env);
            }

            if let Some(TypeExpr::Struct(ref def)) = new_type {
                let def = type_env.get_struct_def(def.as_str()).clone();

                let mangled_name = mangle_generics_name(
                    def.name.as_str(),
                    new_type_params.as_ref().unwrap_or(&vec![]).as_slice(),
                    type_env,
                );
                if !type_env.prevent_struct_generation.contains(&mangled_name) {
                    type_env
                        .prevent_struct_generation
                        .push(mangled_name.clone());
                    let generics_instance = def
                        .generics
                        .as_ref()
                        .unwrap_or(&vec![])
                        .iter()
                        .map(|x| &x.0.name)
                        .zip(type_params.as_ref().unwrap_or(&vec![]).iter().map(|x| &x.0))
                        .fold(HashMap::new(), |mut acc, (param_name, param_inst)| {
                            acc.insert(param_name.clone(), param_inst.clone());
                            acc
                        });

                    let mut cloned_def = def.clone();
                    cloned_def.name = mangled_name.clone();
                    cloned_def.generics = None;
                    replace_generics_in_struct_definition(&mut cloned_def, &generics_instance);

                    // println!("replaced: {cloned_def:?}");

                    type_env.generic_structs_generated.push(cloned_def.clone());
                    for f in &mut cloned_def.fields {
                        instantiate_generics_type_expr(&mut f.type_expr.0, type_env);
                    }

                    for m in &mut cloned_def.methods {
                        if m.generics.is_some() {
                            continue;
                        }
                        for ty in m.return_type.iter_mut().chain(
                            m.params
                                .iter_mut()
                                .flat_map(|x| x.iter_mut())
                                .map(|x| &mut x.1),
                        ) {
                            instantiate_generics_type_expr(&mut ty.0, type_env);
                        }
                        instantiate_generics_value_expr(&mut m.value_expr.0, type_env);
                    }

                    // println!("pushing {cloned_def:?}");
                    type_env
                        .generic_structs_generated
                        .retain(|x| x.name != mangled_name);
                    type_env.generic_structs_generated.push(cloned_def.clone());
                    type_env.struct_definitions.push(cloned_def);
                    *name = mangled_name.clone();
                    *type_params = None;
                } else {
                    *name = mangled_name;
                    *type_params = None;
                }
            }

            println!("got {name:?} and {type_params:?}");
            // std::process::exit(0);
        }
        ValueExpr::Tuple(fields) => {
            for f in fields {
                instantiate_generics_value_expr(&mut f.0, type_env);
            }
        }
        ValueExpr::While { condition, body } => {
            instantiate_generics_value_expr(&mut condition.0, type_env);
            instantiate_generics_value_expr(&mut body.0, type_env);
        }
        ValueExpr::VarDecl(decl) => {
            instantiate_generics_type_expr(&mut decl.0.type_expr.0, type_env);

            if let Some(init) = decl.0.initializer.as_mut() {
                instantiate_generics_value_expr(&mut init.0, type_env);
            }
        }
        ValueExpr::VarAssign(a) => {
            instantiate_generics_value_expr(&mut a.0.target.0, type_env);
            instantiate_generics_value_expr(&mut a.0.value_expr.0, type_env);
        }
        ValueExpr::Match { value_expr, arms } => {
            instantiate_generics_value_expr(&mut value_expr.0, type_env);
            for arm in arms {
                instantiate_generics_type_expr(&mut arm.type_case.0, type_env);
                instantiate_generics_value_expr(&mut arm.value_expr.0, type_env);
            }
        }
        ValueExpr::Bool(..)
        | ValueExpr::Break
        | ValueExpr::Char(..)
        | ValueExpr::String(..)
        | ValueExpr::Continue
        | ValueExpr::Float(..)
        | ValueExpr::InlineGo(..)
        | ValueExpr::Int(..)
        | ValueExpr::RawVariable(..)
        | ValueExpr::Return(..)
        | ValueExpr::Variable(..) => {}
    }
}

fn sort_fields_value_expr(expr: &mut ValueExpr) {
    match expr {
        ValueExpr::Array(ty, exprs) => {
            if let Some(ty) = ty {
                sort_fields_type_expr(&mut ty.0);
            }
            for expr in exprs {
                sort_fields_value_expr(&mut expr.0);
            }
        }
        ValueExpr::VarDecl(d) => {
            let Declaration {
                name: _,
                type_expr,
                initializer,
            } = &mut d.0;
            sort_fields_type_expr(&mut type_expr.0);
            if let Some(e) = initializer {
                sort_fields_value_expr(&mut e.0);
            }
        }
        ValueExpr::Lambda(l) => {
            let LambdaFunctionExpr {
                params,
                return_type,
                value_expr,
            } = &mut **l;
            if let Some(return_type) = return_type {
                sort_fields_type_expr(&mut return_type.0);
            }
            for (_, p) in params {
                sort_fields_type_expr(&mut p.0);
            }
            sort_fields_value_expr(&mut value_expr.0);
        }
        ValueExpr::Add(l, r) | ValueExpr::Mul(l, r) | ValueExpr::Equals(l, r) => {
            sort_fields_value_expr(&mut l.0);
            sort_fields_value_expr(&mut r.0);
        }
        ValueExpr::ArrayAccess(target, idx) => {
            sort_fields_value_expr(&mut target.0);
            sort_fields_value_expr(&mut idx.0);
        }
        ValueExpr::Block(exprs) => {
            for expr in exprs {
                sort_fields_value_expr(&mut expr.0);
            }
        }
        ValueExpr::BoolNegate(e) => sort_fields_value_expr(&mut e.0),
        ValueExpr::Duck(init) => {
            for i in init {
                sort_fields_value_expr(&mut i.1.0);
            }
        }
        ValueExpr::FieldAccess {
            target_obj,
            field_name: _,
        } => {
            sort_fields_value_expr(&mut target_obj.0);
        }
        ValueExpr::FormattedString(content) => {
            for c in content {
                if let ValFmtStringContents::Expr(e) = c {
                    sort_fields_value_expr(&mut e.0);
                }
            }
        }
        ValueExpr::FunctionCall {
            target,
            params,
            type_params: _,
        } => {
            // todo: type_params
            sort_fields_value_expr(&mut target.0);
            for p in params {
                sort_fields_value_expr(&mut p.0);
            }
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            sort_fields_value_expr(&mut condition.0);
            sort_fields_value_expr(&mut then.0);
            if let Some(r#else) = r#else {
                sort_fields_value_expr(&mut r#else.0);
            }
        }
        ValueExpr::Match { value_expr, arms } => {
            sort_fields_value_expr(&mut value_expr.0);
            for arm in arms {
                sort_fields_type_expr(&mut arm.type_case.0);
                sort_fields_value_expr(&mut arm.value_expr.0);
            }
        }
        ValueExpr::Return(r) => {
            if let Some(r) = r {
                sort_fields_value_expr(&mut r.0);
            }
        }
        ValueExpr::Struct { fields, .. } => {
            for field in fields {
                sort_fields_value_expr(&mut field.1.0);
            }
        }
        ValueExpr::Tuple(fields) => {
            for field in fields {
                sort_fields_value_expr(&mut field.0);
            }
        }
        ValueExpr::VarAssign(a) => {
            let Assignment { target, value_expr } = &mut a.0;
            sort_fields_value_expr(&mut target.0);
            sort_fields_value_expr(&mut value_expr.0);
        }
        ValueExpr::While { condition, body } => {
            sort_fields_value_expr(&mut condition.0);
            sort_fields_value_expr(&mut body.0);
        }
        ValueExpr::Break
        | ValueExpr::InlineGo(..)
        | ValueExpr::Int(..)
        | ValueExpr::Variable(..)
        | ValueExpr::RawVariable(..)
        | ValueExpr::Continue
        | ValueExpr::String(..)
        | ValueExpr::Char(..)
        | ValueExpr::Float(..)
        | ValueExpr::Bool(..) => {}
    }
}

fn sort_fields_type_expr(expr: &mut TypeExpr) {
    match expr {
        TypeExpr::Alias(t) => {
            sort_fields_type_expr(&mut t.type_expression.0);
        }
        TypeExpr::RawTypeName(..) => {}
        TypeExpr::Duck(Duck { fields }) => {
            fields.sort_by_key(|x| x.name.clone());
            for field in fields {
                sort_fields_type_expr(&mut field.type_expr.0);
            }
        }
        TypeExpr::Array(d) => sort_fields_type_expr(&mut d.0),
        TypeExpr::Fun(params, r) => {
            if let Some(r) = r {
                sort_fields_type_expr(&mut r.0);
            }
            params
                .iter_mut()
                .for_each(|(_, x)| sort_fields_type_expr(&mut x.0));
        }
        TypeExpr::Or(exprs) => {
            for expr in exprs {
                sort_fields_type_expr(&mut expr.0);
            }
        }
        TypeExpr::Tuple(fields) => {
            for field in fields {
                sort_fields_type_expr(&mut field.0);
            }
        }
        TypeExpr::Any
        | TypeExpr::Bool
        | TypeExpr::ConstBool(_)
        | TypeExpr::Char
        | TypeExpr::Float
        | TypeExpr::Go(_)
        | TypeExpr::InlineGo
        | TypeExpr::Int
        | TypeExpr::ConstInt(_)
        | TypeExpr::String
        | TypeExpr::ConstString(_)
        | TypeExpr::TypeName(..)
        | TypeExpr::Struct(..)
        | TypeExpr::TypeNameInternal(..) => {}
    }
}

pub fn typeresolve_struct_def(def: &mut StructDefinition, type_env: &mut TypeEnv) {
    type_env.push_type_aliases();
    type_env.insert_type_alias("Self".to_string(), TypeExpr::Struct(def.name.clone()));

    for f in &mut def.fields {
        resolve_all_aliases_type_expr(&mut f.type_expr.0, type_env);
    }

    for m in &mut def.methods {
        if m.generics.is_some() {
            continue;
        }
        if let Some(return_type) = &mut m.return_type {
            resolve_all_aliases_type_expr(&mut return_type.0, type_env);
        }

        if let Some(params) = m.params.clone().as_mut() {
            for p in params {
                resolve_all_aliases_type_expr(&mut p.1.0, type_env);
            }
        }
    }

    for m in &mut def.methods {
        if m.generics.is_some() {
            continue;
        }
        type_env.push_identifier_types();

        if let Some(params) = m.params.clone().as_mut() {
            for p in params {
                type_env.insert_identifier_type(p.0.clone(), p.1.0.clone());
            }
        }

        type_env.insert_identifier_type("self".to_string(), TypeExpr::Struct(def.name.clone()));
        typeresolve_value_expr(&mut m.value_expr.0, type_env);
        if m.name == "fisch" {
            println!(
                "method resolve {} {} {:?}",
                def.name, m.name, m.value_expr.0
            );
        }
        type_env.pop_identifier_types();
    }

    type_env.pop_type_aliases();
    let ty_expr = TypeExpr::Struct(def.name.clone());
    type_env.insert_type_alias(def.name.clone(), ty_expr.clone());
    type_env.all_types.insert(0, ty_expr);
}

pub fn typeresolve_source_file(source_file: &mut SourceFile, type_env: &mut TypeEnv) {
    type_env.push_type_aliases();

    println!("{} sort fields", Tag::TypeResolve,);

    // Step 1: Sort fields
    source_file
        .type_definitions
        .iter_mut()
        .for_each(|type_definition| {
            sort_fields_type_expr(&mut type_definition.type_expression.0);
        });

    // todo: check if we'd rather sort after generic generation
    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_definition| {
            sort_fields_value_expr(&mut function_definition.value_expr.0);
        });

    println!("{} insert type definitions", Tag::TypeResolve,);

    println!(
        "source file struct defs: {}",
        source_file.struct_definitions.len()
    );
    // Step 2: Insert type definitions
    source_file
        .struct_definitions
        .iter()
        .for_each(|struct_definition| {
            type_env.struct_definitions.push(struct_definition.clone());
            type_env.insert_type_alias(
                struct_definition.name.clone(),
                TypeExpr::Struct(struct_definition.name.clone()),
            );
        });

    source_file
        .type_definitions
        .iter()
        .for_each(|type_definition| {
            type_env.insert_type_alias(
                type_definition.name.clone(),
                TypeExpr::Alias(Box::new(type_definition.clone())),
            );
        });

    for fn_def in &source_file.function_definitions {
        type_env.function_definitions.push(fn_def.clone());
    }

    println!(
        "{} resolve aliases in function signatures and prepare function types",
        Tag::TypeResolve,
    );

    // Step 3: Find generic instantiations
    source_file.function_definitions.iter_mut().for_each(|x| {
        if x.generics.is_some() {
            return;
        }
        for t in x
            .params
            .iter_mut()
            .flat_map(|x| x.iter_mut().map(|x| &mut x.1.0))
            .chain(x.return_type.iter_mut().map(|x| &mut x.0))
        {
            instantiate_generics_type_expr(t, type_env);
        }

        instantiate_generics_value_expr(&mut x.value_expr.0, type_env);
    });

    for func in &type_env.generic_fns_generated {
        source_file.function_definitions.push(func.clone());
    }

    // Step 3: Resolve aliases in function signatures and prepare function types
    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_definition| {
            if function_definition.generics.is_some() {
                return;
            }

            if let Some(params) = function_definition.params.as_mut() {
                for (_, p) in params {
                    instantiate_generics_type_expr(&mut p.0, type_env);
                    resolve_all_aliases_type_expr(&mut p.0, type_env);
                }
            }

            if let Some(r) = function_definition.return_type.as_mut() {
                instantiate_generics_type_expr(&mut r.0, type_env);
                resolve_all_aliases_type_expr(&mut r.0, type_env);
            }

            let fn_type_expr = TypeExpr::Fun(
                function_definition
                    .params
                    .as_ref()
                    .unwrap_or(&Vec::new())
                    .iter()
                    .map(|(identifier, type_expr)| (Some(identifier.clone()), type_expr.clone()))
                    .collect::<Vec<_>>(),
                function_definition
                    .return_type
                    .as_ref()
                    .map(|spanned_type_expr| {
                        Box::new((
                            type_env.insert_type(spanned_type_expr.0.clone()),
                            spanned_type_expr.1,
                        ))
                    }),
            );

            type_env.insert_identifier_type(function_definition.name.clone(), fn_type_expr);
        });
    println!("{} typeresolve functions", Tag::TypeResolve);
    println!("{} final resolve of all functions", Tag::TypeResolve);

    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_defintion| {
            if function_defintion.generics.is_some() {
                return;
            }
            typeresolve_function_definition(function_defintion, type_env);
        });

    for s in &type_env.generic_structs_generated {
        source_file.struct_definitions.push(s.clone());
    }

    let mut cloned = type_env.generic_methods_generated.clone();

    for (struct_name, methods) in cloned.iter_mut() {
        for m in methods.iter_mut() {
            type_env.push_identifier_types();
            type_env
                .insert_identifier_type("self".to_string(), TypeExpr::Struct(struct_name.clone()));
            typeresolve_function_definition(m, type_env);
            type_env.pop_identifier_types();
        }
    }

    type_env.generic_methods_generated = cloned;

    let mut cloned = type_env.generic_fns_generated.clone();

    for m in cloned.iter_mut() {
        typeresolve_function_definition(m, type_env);
    }

    type_env.generic_fns_generated = cloned;

    // Step 5: Final resolve of all functions
    source_file
        .struct_definitions
        .iter_mut()
        .for_each(|struct_definition| {
            if struct_definition.generics.is_some() {
                return;
            }
            typeresolve_struct_def(struct_definition, type_env);
        });

    for x in &type_env.generic_fns_generated {
        if !source_file
            .function_definitions
            .iter()
            .any(|y| x.name.as_str() == y.name.as_str())
        {
            source_file.function_definitions.push(x.clone());
        }
    }

    type_env.struct_definitions = source_file.struct_definitions.clone();
    type_env.function_definitions = source_file.function_definitions.clone();
}

fn typeresolve_function_definition(
    function_definition: &mut FunctionDefintion,
    type_env: &mut TypeEnv,
) {
    if let Some(generics) = function_definition.generics.as_mut() {
        generics.iter().for_each(|(generic, _)| {
            type_env.insert_type_alias(generic.name.clone(), TypeExpr::Any);
        });
    }

    if let Some((return_type, _)) = &mut function_definition.return_type {
        *return_type = type_env.insert_type(return_type.clone());
    }

    type_env.push_identifier_types();

    if let Some(params) = function_definition.params.clone().as_mut() {
        for p in params {
            type_env.insert_identifier_type(p.0.clone(), p.1.0.clone());
        }
    }

    typeresolve_value_expr(&mut function_definition.value_expr.0, type_env);
    type_env.pop_identifier_types();
}

fn typeresolve_value_expr(value_expr: &mut ValueExpr, type_env: &mut TypeEnv) {
    match value_expr {
        ValueExpr::RawVariable(_, path) => {
            let ident = mangle(path);
            let mut type_expr = type_env
                .get_identifier_type(ident.clone())
                .unwrap_or_else(|| panic!("Couldn't resolve type of identifier {ident}"));
            resolve_all_aliases_type_expr(&mut type_expr, type_env);
            *value_expr = ValueExpr::Variable(true, ident, Some(type_expr));
        }
        ValueExpr::VarDecl(declaration) => {
            let declaration = &mut declaration.0;

            // Resolve the type expression on the declaration
            resolve_all_aliases_type_expr(&mut declaration.type_expr.0, type_env);

            type_env
                .insert_identifier_type(declaration.name.clone(), declaration.type_expr.0.clone());

            if let Some(initializer) = &mut declaration.initializer {
                typeresolve_value_expr(&mut initializer.0, type_env);
            }
        }
        ValueExpr::FormattedString(contents) => {
            for c in contents {
                match c {
                    ValFmtStringContents::Expr(e) => typeresolve_value_expr(&mut e.0, type_env),
                    ValFmtStringContents::String(s) => {
                        type_env.insert_type(TypeExpr::ConstString(s.clone()));
                    }
                }
            }
        }
        ValueExpr::ArrayAccess(target, idx) => {
            typeresolve_value_expr(&mut target.0, type_env);
            typeresolve_value_expr(&mut idx.0, type_env);
        }
        ValueExpr::Array(ty, exprs) => {
            if let Some(ty) = ty {
                resolve_all_aliases_type_expr(&mut ty.0, type_env);
            }

            for expr in exprs {
                typeresolve_value_expr(&mut expr.0, type_env);
            }
            let ty = TypeExpr::from_value_expr(value_expr as &ValueExpr, type_env);
            type_env.insert_type(ty);
        }
        ValueExpr::InlineGo(..) => {}
        ValueExpr::Lambda(b) => {
            let LambdaFunctionExpr {
                params,
                return_type,
                value_expr,
            } = &mut **b;

            type_env.push_identifier_types();

            for (name, ty) in params {
                type_env.insert_identifier_type(name.to_owned(), ty.0.clone());
                resolve_all_aliases_type_expr(&mut ty.0, type_env);
            }

            if let Some(return_type) = return_type {
                resolve_all_aliases_type_expr(&mut return_type.0, type_env);
            }

            typeresolve_value_expr(&mut value_expr.0, type_env);
            type_env.pop_identifier_types();
        }
        ValueExpr::FunctionCall {
            target,
            params,
            type_params,
        } => {
            typeresolve_value_expr(&mut target.0, type_env);

            // generic method call
            if let Some(type_params_vec) = type_params
                && let ValueExpr::FieldAccess {
                    target_obj,
                    field_name,
                } = &mut target.0
            {
                let target_ty =
                    TypeExpr::from_value_expr_resolved_type_name(&target_obj.0, type_env);

                let TypeExpr::Struct(struct_name) = target_ty else {
                    panic!()
                };

                let StructDefinition {
                    name,
                    fields: _,
                    methods,
                    generics,
                } = type_env.get_struct_def(struct_name.as_str()).clone();

                assert!(generics.is_none());

                let mangled_name =
                    mangle_generics_name(field_name.as_str(), type_params_vec.as_slice(), type_env);

                let mangled_name_to_check = format!("{name}_{mangled_name}");

                let method_def = methods
                    .iter()
                    .find(|m| m.name.as_str() == field_name.as_str())
                    .unwrap();
                if !type_env
                    .prevent_struct_generation
                    .contains(&mangled_name_to_check)
                {
                    type_env
                        .prevent_struct_generation
                        .push(mangled_name_to_check);
                    let generics_instance = method_def
                        .generics
                        .as_ref()
                        .unwrap_or(&vec![])
                        .iter()
                        .map(|x| &x.0.name)
                        .zip(type_params.as_ref().unwrap_or(&vec![]).iter().map(|x| &x.0))
                        .fold(HashMap::new(), |mut acc, (param_name, param_inst)| {
                            acc.insert(param_name.clone(), param_inst.clone());
                            acc
                        });

                    let mut cloned_def = method_def.clone();
                    cloned_def.name = mangled_name.clone();
                    cloned_def.generics = None;

                    for t in cloned_def
                        .params
                        .iter_mut()
                        .flat_map(|x| x.iter_mut().map(|x| &mut x.1.0))
                        .chain(cloned_def.return_type.iter_mut().map(|x| &mut x.0))
                    {
                        replace_generics_in_type_expr(t, &generics_instance);
                        instantiate_generics_type_expr(t, type_env);
                    }

                    replace_generics_in_value_expr(
                        &mut cloned_def.value_expr.0,
                        &generics_instance,
                    );
                    instantiate_generics_value_expr(&mut cloned_def.value_expr.0, type_env);

                    let mut cloned = type_env.generic_structs_generated.clone();
                    for s in cloned.iter_mut() {
                        typeresolve_struct_def(s, type_env);
                    }
                    type_env.generic_structs_generated = cloned;

                    let mut cloned = type_env.generic_fns_generated.clone();
                    for s in cloned.iter_mut() {
                        typeresolve_function_definition(s, type_env);
                    }
                    type_env.generic_fns_generated = cloned;

                    let mut cloned = type_env.generic_methods_generated.clone();
                    for (s_name, values) in cloned.iter_mut() {
                        for v in values {
                            type_env.push_identifier_types();
                            type_env.insert_identifier_type(
                                "self".to_string(),
                                TypeExpr::Struct(s_name.clone()),
                            );
                            typeresolve_function_definition(v, type_env);
                            type_env.push_identifier_types();
                        }
                    }
                    type_env.generic_methods_generated = cloned;

                    type_env.push_identifier_types();
                    type_env
                        .insert_identifier_type("self".to_string(), TypeExpr::Struct(struct_name));
                    typeresolve_function_definition(&mut cloned_def, type_env);
                    type_env.pop_identifier_types();

                    type_env
                        .generic_methods_generated
                        .entry(name.clone())
                        .or_default()
                        .push(cloned_def);

                    *field_name = mangled_name;
                    *type_params = None;
                } else {
                    *field_name = mangled_name;
                    *type_params = None;
                }
            }

            assert!(
                type_params.is_none(),
                "type_params should be omitted by now"
            );

            params
                .iter_mut()
                .for_each(|param| typeresolve_value_expr(&mut param.0, type_env));
        }
        ValueExpr::Variable(_, identifier, type_expr_opt) => {
            // if let Some(type_expr) = type_expr_opt {
            //     resolve_all_aliases_type_expr(type_expr, type_env);
            //     return;
            // }
            let type_expr = type_env
                .get_identifier_type(identifier.clone())
                .unwrap_or_else(|| panic!("Couldn't resolve type of identifier {identifier}"));
            //resolve_all_aliases_type_expr(&mut type_expr, type_env, generics_to_ignore);
            *type_expr_opt = Some(type_expr)
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            typeresolve_value_expr(&mut condition.0, type_env);
            type_env.push_identifier_types();
            typeresolve_value_expr(&mut then.0, type_env);
            type_env.pop_identifier_types();
            if let Some(r#else) = r#else {
                type_env.push_identifier_types();
                typeresolve_value_expr(&mut r#else.0, type_env);
                type_env.pop_identifier_types();
            }
        }
        ValueExpr::While { condition, body } => {
            typeresolve_value_expr(&mut condition.0, type_env);
            type_env.push_identifier_types();
            typeresolve_value_expr(&mut body.0, type_env);
            type_env.pop_identifier_types();
        }
        ValueExpr::Tuple(value_exprs) => {
            value_exprs
                .iter_mut()
                .for_each(|value_expr| typeresolve_value_expr(&mut value_expr.0, type_env));
            let ty = TypeExpr::from_value_expr(value_expr as &ValueExpr, type_env);
            type_env.insert_type(ty);
        }
        ValueExpr::Block(value_exprs) => {
            type_env.push_identifier_types();
            value_exprs
                .iter_mut()
                .for_each(|value_expr| typeresolve_value_expr(&mut value_expr.0, type_env));
            type_env.pop_identifier_types();
        }
        ValueExpr::Duck(items) => {
            items
                .iter_mut()
                .for_each(|(_, value_expr)| typeresolve_value_expr(&mut value_expr.0, type_env));
            let ty = TypeExpr::from_value_expr(value_expr as &ValueExpr, type_env);
            type_env.insert_type(ty);
        }
        ValueExpr::Struct {
            name,
            fields,
            type_params: _,
        } => {
            if let Some(TypeExpr::Struct(s)) = type_env
                .try_resolve_type_alias(name)
                .map(|e| type_env.try_resolve_type_expr(&e))
            {
                *name = s;
            }

            fields.iter_mut().for_each(|(_field_name, value_expr)| {
                typeresolve_value_expr(&mut value_expr.0, type_env);
            });

            let ty = TypeExpr::from_value_expr(value_expr as &ValueExpr, type_env);
            type_env.insert_type(ty);
        }
        ValueExpr::FieldAccess { target_obj, .. } => {
            typeresolve_value_expr(&mut target_obj.0, type_env);
        }
        ValueExpr::Return(Some(value_expr)) => typeresolve_value_expr(&mut value_expr.0, type_env),
        ValueExpr::VarAssign(assignment) => {
            typeresolve_value_expr(&mut assignment.0.target.0, type_env);
            typeresolve_value_expr(&mut assignment.0.value_expr.0, type_env);
        }
        ValueExpr::Add(lhs, rhs) => {
            typeresolve_value_expr(&mut lhs.0, type_env);
            typeresolve_value_expr(&mut rhs.0, type_env);
        }
        ValueExpr::Mul(lhs, rhs) => {
            typeresolve_value_expr(&mut lhs.0, type_env);
            typeresolve_value_expr(&mut rhs.0, type_env);
        }
        ValueExpr::Equals(lhs, rhs) => {
            typeresolve_value_expr(&mut lhs.0, type_env);
            typeresolve_value_expr(&mut rhs.0, type_env);
        }
        ValueExpr::BoolNegate(value_expr) => {
            typeresolve_value_expr(&mut value_expr.0, type_env);
        }
        ValueExpr::Match { value_expr, arms } => {
            typeresolve_value_expr(&mut value_expr.0, type_env);
            arms.iter_mut().for_each(|arm| {
                type_env.push_identifier_types();
                if let Some(identifier) = &arm.identifier_binding {
                    type_env.insert_identifier_type(
                        identifier.clone(),
                        arm.type_case.0.clone(),
                    );
                }
                typeresolve_value_expr(&mut arm.value_expr.0, type_env);
                type_env.pop_identifier_types();
            });
        }
        ValueExpr::String(str) => {
            type_env.insert_type(TypeExpr::ConstString(str.clone()));
        }
        ValueExpr::Int(..)
        | ValueExpr::Bool(..)
        | ValueExpr::Char(..)
        | ValueExpr::Float(..)
        | ValueExpr::Break
        | ValueExpr::Return(None)
        | ValueExpr::Continue => {}
    }
}

// todo(@mvmo): typeinference
// resolve implicit type and attach it to variables for type inference
#[allow(dead_code)]
fn resolve_implicit_function_return_type(
    fun_def: &FunctionDefintion,
    type_env: &mut TypeEnv,
) -> Result<TypeExpr, String> {
    // check against annotated return type
    fn flatten_returns(
        value_expr: &ValueExpr,
        return_types_found: &mut Vec<TypeExpr>,
        type_env: &mut TypeEnv,
    ) {
        match value_expr {
            ValueExpr::FormattedString(contents) => {
                for c in contents {
                    if let ValFmtStringContents::Expr(e) = c {
                        flatten_returns(&e.0, return_types_found, type_env);
                    }
                }
            }
            ValueExpr::Array(_, exprs) => {
                for expr in exprs {
                    flatten_returns(&expr.0, return_types_found, type_env);
                }
            }
            ValueExpr::FunctionCall { .. }
            | ValueExpr::Int(..)
            | ValueExpr::InlineGo(..)
            | ValueExpr::String(..)
            | ValueExpr::Bool(..)
            | ValueExpr::Float(..)
            | ValueExpr::Char(..)
            | ValueExpr::Tuple(..)
            | ValueExpr::Break
            | ValueExpr::Continue
            | ValueExpr::Duck(..)
            | ValueExpr::Struct { .. }
            | ValueExpr::FieldAccess { .. }
            | ValueExpr::Lambda(..)
            | ValueExpr::Variable(..)
            | ValueExpr::RawVariable(..)
            | ValueExpr::Match { .. }
            | ValueExpr::ArrayAccess(..) => {}
            ValueExpr::If {
                condition,
                then,
                r#else,
            } => {
                flatten_returns(&condition.as_ref().0, return_types_found, type_env);
                flatten_returns(&then.as_ref().0, return_types_found, type_env);
                r#else.as_ref().inspect(|r#else| {
                    flatten_returns(&r#else.as_ref().0, return_types_found, type_env);
                });
            }
            ValueExpr::While { condition, body } => {
                flatten_returns(&condition.as_ref().0, return_types_found, type_env);
                flatten_returns(&body.as_ref().0, return_types_found, type_env)
            }
            ValueExpr::Block(items) => items
                .iter()
                .for_each(|item| flatten_returns(&item.0, return_types_found, type_env)),
            ValueExpr::Return(Some(value_expr)) => {
                return_types_found.push(TypeExpr::from_value_expr(&value_expr.0, type_env));
            }
            ValueExpr::Return(None) => {
                return_types_found.push(TypeExpr::Tuple(vec![]));
            }
            ValueExpr::VarAssign(assignment) => {
                flatten_returns(
                    &assignment.as_ref().0.value_expr.0,
                    return_types_found,
                    type_env,
                );
            }
            ValueExpr::VarDecl(declaration) => {
                declaration
                    .as_ref()
                    .0
                    .initializer
                    .as_ref()
                    .inspect(|initializer| {
                        flatten_returns(&initializer.0, return_types_found, type_env);
                    });
            }
            ValueExpr::Add(left, right) => {
                flatten_returns(&left.as_ref().0, return_types_found, type_env);
                flatten_returns(&right.as_ref().0, return_types_found, type_env);
            }
            ValueExpr::Mul(left, right) => {
                flatten_returns(&left.as_ref().0, return_types_found, type_env);
                flatten_returns(&right.as_ref().0, return_types_found, type_env);
            }
            ValueExpr::BoolNegate(value_expr) => {
                flatten_returns(&value_expr.as_ref().0, return_types_found, type_env);
            }
            ValueExpr::Equals(left, right) => {
                flatten_returns(&left.as_ref().0, return_types_found, type_env);
                flatten_returns(&right.as_ref().0, return_types_found, type_env);
            }
        }
    }

    let mut return_types_found = Vec::new();
    flatten_returns(&fun_def.value_expr.0, &mut return_types_found, type_env);

    return_types_found.sort_by_key(|type_expr| type_expr.as_clean_go_type_name(type_env));
    return_types_found.dedup();

    if return_types_found.is_empty() {
        return Ok(TypeExpr::Tuple(vec![]));
    }

    if return_types_found.len() == 1 {
        return Ok(return_types_found.first().unwrap().clone());
    }

    // TODO add spans
    return Ok(TypeExpr::Or(
        return_types_found
            .iter()
            .map(|type_expr| type_expr.clone().into_empty_span())
            .collect::<Vec<_>>(),
    ));
}
