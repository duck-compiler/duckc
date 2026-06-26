use crate::{ast::Span, backend::semantics::symbol::SymbolKind};

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
        };

        let error_code = format!("S0001");

        Diagnostic {
            message: message.into_boxed_str(),
            location: location,
            error_code: error_code.into_boxed_str(),
            kind: DiagnosticKind::Error
        }
    }
}
