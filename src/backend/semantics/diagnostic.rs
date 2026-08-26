use crate::{ast::{Span, struct_definition::MethodKind}, backend::semantics::symbol::SymbolKind};

#[derive(Debug)]
pub enum DiagnosticKind {
    Warning,
    Error,
}

#[derive(Debug)]
pub struct Diagnostic<'src> {
    pub message: Box<str>,
    pub location: Span<'src>,
    pub error_code: Box<str>,
    pub kind: DiagnosticKind,
}

impl<'src> Diagnostic<'src> {
    pub fn symbol_not_found(
        kind: SymbolKind,
        name: &'src str,
        location: Span<'src>
    ) -> Diagnostic<'src> {
        let message = match kind {
            SymbolKind::Function => format!("function not found: {}", name),
            SymbolKind::Variable => format!("variable not found: {}", name),
            SymbolKind::Struct => format!("struct not found: {}", name),
            SymbolKind::Param => format!("parameter not found: {}", name),
            SymbolKind::Module => format!("package not found: {}", name),
        };

        let error_code = format!("S0001");

        Diagnostic {
            message: message.into_boxed_str(),
            location: location,
            error_code: error_code.into_boxed_str(),
            kind: DiagnosticKind::Error
        }
    }

    pub fn type_mismatch(
        expected: &str,
        found: &str,
        location: Span<'src>,
    ) -> Diagnostic<'src> {
        Diagnostic {
            message: format!("type mismatch: expected {}, found {}", expected, found).into_boxed_str(),
            location,
            error_code: "T0001".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn not_callable(location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: "expression is not callable".to_string().into_boxed_str(),
            location,
            error_code: "T0002".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn wrong_arg_count(
        expected: usize,
        found: usize,
        location: Span<'src>,
    ) -> Diagnostic<'src> {
        Diagnostic {
            message: format!("wrong number of arguments: expected {}, found {}", expected, found).into_boxed_str(),
            location,
            error_code: "T0003".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn empty_array_literal(location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: "cannot infer type of empty array literal".to_string().into_boxed_str(),
            location,
            error_code: "T0005".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn not_indexable(location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: "expression is not indexable".to_string().into_boxed_str(),
            location,
            error_code: "T0006".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn unknown_package_member(
        package: &str,
        name: &str,
        reason: &str,
        location: Span<'src>,
    ) -> Diagnostic<'src> {
        Diagnostic {
            message: format!("unknown or unsupported member `{}.{}`: {}", package, name, reason).into_boxed_str(),
            location,
            error_code: "T0004".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn not_a_struct(location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: "field access on a value that is not a struct".to_string().into_boxed_str(),
            location,
            error_code: "T0007".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn unknown_struct_field(
        struct_name: &str,
        field_name: &str,
        location: Span<'src>,
    ) -> Diagnostic<'src> {
        Diagnostic {
            message: format!("struct `{}` has no field `{}`", struct_name, field_name).into_boxed_str(),
            location,
            error_code: "T0008".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn missing_struct_fields(
        struct_name: &str,
        missing: &[&str],
        location: Span<'src>,
    ) -> Diagnostic<'src> {
        Diagnostic {
            message: format!("missing field(s) in initializer of `{}`: {}", struct_name, missing.join(", ")).into_boxed_str(),
            location,
            error_code: "T0009".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn not_yet_supported(feature: &str, location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: format!("{} is not supported yet", feature).into_boxed_str(),
            location,
            error_code: "T0010".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn not_allowed_at_top_level(location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: "only `use`, `fn`, and `struct` declarations are allowed at the top level".to_string().into_boxed_str(),
            location,
            error_code: "T0011".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn break_or_continue_outside_loop(location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: "`break`/`continue` can only be used inside a `while` loop".to_string().into_boxed_str(),
            location,
            error_code: "T0012".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn already_defined(name: &str, location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: format!("`{}` is already defined", name).into_boxed_str(),
            location,
            error_code: "T0013".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn not_a_pointer(location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: "can't dereference a value that's nt a pointer".to_string().into_boxed_str(),
            location,
            error_code: "T0015".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn not_addressable(location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: "can't take address of this".to_string().into_boxed_str(),
            location,
            error_code: "T0016".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn not_a_value(name: &str, location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: format!("`{}` is not a value", name).into_boxed_str(),
            location,
            error_code: "T0017".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn literal_out_of_range(type_name: &str, location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: format!("literal does not fit into `{}`", type_name).into_boxed_str(),
            location,
            error_code: "T0018".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn cannot_infer_type(name: &str, location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: format!("cannot infer the type of `{}`", name).into_boxed_str(),
            location,
            error_code: "T0019".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn if_without_else_as_value(location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: "`if` without a `else` branch can't produce value".to_string().into_boxed_str(),
            location,
            error_code: "T0020".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn private_member(
        struct_name: &str,
        member_name: &str,
        location: Span<'src>,
    ) -> Diagnostic<'src> {
        Diagnostic {
            message: format!(
                "`{}.{}` is private and can only be used inside the impl block of `{}`",
                struct_name, member_name, struct_name,
            ).into_boxed_str(),
            location,
            error_code: "T0021".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn cannot_assign_to_method(
        struct_name: &str,
        method_name: &str,
        location: Span<'src>,
    ) -> Diagnostic<'src> {
        Diagnostic {
            message: format!("cannot assign to method `{}.{}`", struct_name, method_name).into_boxed_str(),
            location,
            error_code: "T0023".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn field_needs_instance(
        struct_name: &str,
        field_name: &str,
        location: Span<'src>,
    ) -> Diagnostic<'src> {
        Diagnostic {
            message: format!(
                "field `{}.{}` needs a instance of `{}`, it can't be called from the type itself",
                struct_name, field_name, struct_name,
            ).into_boxed_str(),
            location,
            error_code: "T0022".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn wrong_method_receiver(
        struct_name: &str,
        method_name: &str,
        kind: MethodKind,
        location: Span<'src>,
    ) -> Diagnostic<'src> {
        let message = match kind {
            MethodKind::Instance => format!(
                "instance method `{}.{}` needs a value of `{}`, it can't be called on the type itself",
                struct_name, method_name, struct_name,
            ),
            MethodKind::Static => format!(
                "static method `{}.{}` has to be called on `{}`, not on a instance",
                struct_name, method_name, struct_name,
            ),
        };

        Diagnostic {
            message: message.into_boxed_str(),
            location,
            error_code: "T0022".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }

    pub fn nested_declaration_not_allowed(location: Span<'src>) -> Diagnostic<'src> {
        Diagnostic {
            message: "`fn`/`struct` declarations are only allowed at the top level, not nested inside a function".to_string().into_boxed_str(),
            location,
            error_code: "T0014".to_string().into_boxed_str(),
            kind: DiagnosticKind::Error,
        }
    }
}
