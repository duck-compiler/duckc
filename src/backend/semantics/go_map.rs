use std::collections::HashMap;

use crate::backend::semantics::{
    context::SemanticsContext,
    go_resolve::{RawField, RawFuncType, RawType},
    r#type::{Type, TypeId},
    symbol::{Origin, SymbolData, SymbolKind},
};

fn leak<'src>(s: String) -> &'src str {
    Box::leak(s.into_boxed_str())
}

pub fn map_go_type<'src>(
    context: &mut SemanticsContext<'src>,
    package: &str,
    raw: &RawType,
    types: &HashMap<String, RawType>,
) -> Option<TypeId> {
    match raw {
        RawType::Basic { basic } => map_go_basic(basic).map(|type_| context.intern(type_)),
        RawType::Named { r#ref } => {
            let resolved = types.get(r#ref)?;
            if let RawType::Struct { fields } = resolved {
                resolve_go_struct(context, package, r#ref, fields, types)
            } else {
                map_go_type(context, package, resolved, types)
            }
        }
        RawType::Pointer { .. }
        | RawType::Slice { .. }
        | RawType::Array { .. }
        | RawType::Map { .. }
        | RawType::Struct { .. }
        | RawType::Interface { .. }
        | RawType::Any
        | RawType::Chan { .. }
        | RawType::Func { .. }
        | RawType::TypeParam { .. }
        | RawType::Unsupported => None,
    }
}

fn map_go_basic(name: &str) -> Option<Type> {
    match name {
        "bool" => Some(Type::Bool),
        "string" => Some(Type::String),
        "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16" | "uint32" | "uint64" | "uintptr" => Some(Type::Int),
        "float32" | "float64" => Some(Type::Float),
        // complex64/complex128/unsafe.Pointer
        _ => None,
    }
}

fn resolve_go_struct<'src>(
    context: &mut SemanticsContext<'src>,
    package: &str,
    qualified_name: &str,
    fields: &[RawField],
    types: &HashMap<String, RawType>,
) -> Option<TypeId> {
    if let Some(&symbol) = context.go_struct_symbols.get(qualified_name) {
        return Some(context.intern(Type::Struct(symbol)));
    }

    let symbol = context.add_symbol(SymbolData {
        name: leak(qualified_name.to_string()),
        kind: SymbolKind::Struct,
        type_: None,
        origin: Origin::GoType { package: leak(package.to_string()) },
    });

    context.go_struct_symbols.insert(qualified_name.to_string(), symbol);

    let mapped_fields = fields
        .iter()
        .filter(|field| field.exported)
        .filter_map(|field| {
            let field_type = map_go_type(context, package, &field.type_, types)?;
            Some((leak(field.name.clone()), field_type))
        })
        .collect::<Vec<_>>();

    context.struct_fields.insert(symbol, mapped_fields);
    Some(context.intern(Type::Struct(symbol)))
}

pub fn map_go_signature<'src>(
    context: &mut SemanticsContext<'src>,
    package: &str,
    raw: &RawFuncType,
    types: &HashMap<String, RawType>,
) -> Option<TypeId> {
    if !raw.type_params.is_empty() {
        return None;
    }

    let fixed_count = if raw.variadic {
        raw.params.len().checked_sub(1)?
    } else {
        raw.params.len()
    };

    let mut params = Vec::with_capacity(raw.params.len());
    for param in &raw.params[..fixed_count] {
        params.push(map_go_type(context, package, param, types)?);
    }

    if raw.variadic {
        let is_any_slice = matches!(
            raw.params.last(),
            Some(RawType::Slice { elem }) if matches!(**elem, RawType::Any)
        );

        if !is_any_slice {
            return None;
        }

        params.push(context.intern(Type::String));
    }

    let return_type = match raw.results.len() {
        0 => context.intern(Type::Unit),
        1 => map_go_type(context, package, &raw.results[0], types)?,
        _ => context.intern(Type::Unit), // todo: tuple types
    };

    Some(context.intern(Type::Fn { params, return_type }))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn empty_types() -> HashMap<String, RawType> {
        HashMap::new()
    }

    fn basic_field(name: &str, basic: &str, exported: bool) -> RawField {
        RawField {
            name: name.to_string(),
            type_: RawType::Basic { basic: basic.to_string() },
            exported,
            embedded: false,
            tag: String::new(),
        }
    }

    #[test]
    fn named_type_resolves_through_to_its_underlying_basic_kind() {
        let mut context = SemanticsContext::new();
        let types = HashMap::from([
            ("pkg.Duration".to_string(), RawType::Basic { basic: "int64".to_string() }),
        ]);

        let resolved = map_go_type(&mut context, "pkg", &RawType::Named { r#ref: "pkg.Duration".to_string() }, &types)
            .expect("should map");

        assert_eq!(context.types[resolved.0 as usize], Type::Int);
    }

    #[test]
    fn named_struct_type_synthesizes_a_duck_struct_with_only_exported_fields() {
        let mut context = SemanticsContext::new();
        let types = HashMap::from([(
            "image.Point".to_string(),
            RawType::Struct { fields: vec![basic_field("X", "int", true), basic_field("secret", "int", false)] },
        )]);

        let resolved = map_go_type(&mut context, "image", &RawType::Named { r#ref: "image.Point".to_string() }, &types)
            .expect("should map");

        let Type::Struct(sym) = context.types[resolved.0 as usize] else {
            panic!("expected Type::Struct, found {:?}", context.types[resolved.0 as usize]);
        };

        assert_eq!(context.symbols[sym.0 as usize].name, "image.Point");
        assert_eq!(context.symbols[sym.0 as usize].kind, SymbolKind::Struct);

        let fields = context.struct_fields.get(&sym).expect("struct_fields entry");
        assert_eq!(fields.len(), 1, "unexported field must not be included: {:?}", fields);
        assert_eq!(fields[0].0, "X");
        assert_eq!(context.types[fields[0].1.0 as usize], Type::Int);
    }

    #[test]
    fn same_named_struct_resolves_to_the_same_symbol_every_time() {
        let mut context = SemanticsContext::new();
        let types = HashMap::from([(
            "image.Point".to_string(),
            RawType::Struct { fields: vec![basic_field("X", "int", true)] },
        )]);
        let named = RawType::Named { r#ref: "image.Point".to_string() };

        let first = map_go_type(&mut context, "image", &named, &types).expect("should map");
        let second = map_go_type(&mut context, "image", &named, &types).expect("should map");

        assert_eq!(first, second);
        assert_eq!(context.symbols.len(), 1, "must not synthesize a second symbol for the same type");
    }

    #[test]
    fn struct_field_referencing_another_named_struct_resolves_recursively() {
        let mut context = SemanticsContext::new();
        let types = HashMap::from([
            (
                "pkg.Outer".to_string(),
                RawType::Struct {
                    fields: vec![RawField {
                        name: "Inner".to_string(),
                        type_: RawType::Named { r#ref: "pkg.Inner".to_string() },
                        exported: true,
                        embedded: false,
                        tag: String::new(),
                    }],
                },
            ),
            ("pkg.Inner".to_string(), RawType::Struct { fields: vec![basic_field("Value", "string", true)] }),
        ]);

        let resolved = map_go_type(&mut context, "pkg", &RawType::Named { r#ref: "pkg.Outer".to_string() }, &types)
            .expect("should map");

        let Type::Struct(outer_sym) = context.types[resolved.0 as usize] else { panic!("expected struct") };
        let outer_fields = context.struct_fields.get(&outer_sym).unwrap();
        assert_eq!(outer_fields[0].0, "Inner");

        let Type::Struct(inner_sym) = context.types[outer_fields[0].1.0 as usize] else { panic!("expected struct") };
        let string_type = context.intern(Type::String);
        let inner_fields = context.struct_fields.get(&inner_sym).unwrap();
        assert_eq!(inner_fields[0], ("Value", string_type));
    }

    #[test]
    fn signature_with_an_unsupported_param_type_does_not_map_at_all() {
        let mut context = SemanticsContext::new();
        let raw = RawFuncType {
            type_params: vec![],
            variadic: false,
            params: vec![RawType::Pointer { elem: Box::new(RawType::Basic { basic: "int".to_string() }) }],
            results: vec![],
        };

        assert!(map_go_signature(&mut context, "pkg", &raw, &empty_types()).is_none());
    }

    #[test]
    fn variadic_any_tail_maps_to_a_single_string_param() {
        let mut context = SemanticsContext::new();
        let raw = RawFuncType {
            type_params: vec![],
            variadic: true,
            params: vec![RawType::Slice { elem: Box::new(RawType::Any) }],
            results: vec![RawType::Basic { basic: "int".to_string() }],
        };

        let fn_type = map_go_signature(&mut context, "fmt", &raw, &empty_types()).expect("should map");

        let Type::Fn { params, return_type } = context.types[fn_type.0 as usize].clone() else { panic!("expected Fn") };
        assert_eq!(params, vec![context.intern(Type::String)]);
        assert_eq!(context.types[return_type.0 as usize], Type::Int);
    }

    #[test]
    fn generic_signature_is_unsupported() {
        let mut context = SemanticsContext::new();
        let raw = RawFuncType {
            type_params: vec![crate::backend::semantics::go_resolve::RawTypeParam {
                name: "T".to_string(),
                constraint: "any".to_string(),
            }],
            variadic: false,
            params: vec![],
            results: vec![],
        };

        assert!(map_go_signature(&mut context, "pkg", &raw, &empty_types()).is_none());
    }
}
