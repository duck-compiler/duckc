use std::fmt;

use super::tokenizer::{FmtStringPart, Token};

mod sealed {
    pub trait Sealed {}
}

/// Compile-time marker for which pass the AST is in.
///
/// Using an associated type per concern means the compiler enforces what's
/// available in each pass at the type level, with zero runtime cost.
pub trait Phase: sealed::Sealed + 'static {
    /// Type annotation attached to every expression node.
    ///
    /// `()` in `Parsed`
    /// `TypeExpr<Typed>` in `Typed`
    type ExprType: Clone + fmt::Debug + PartialEq;

    /// How a name *reference* is represented.
    ///
    /// `UnresolvedIdent` in `Parsed` - one or more raw path segments from source.
    /// `DefId` in `Typed` - an index into the `SymbolTable` built by the resolver.
    ///
    /// This eliminates the old `RawVariable` / `Variable` variant duality:
    /// both resolve to the same `Ident` variant; only the payload type changes.
    type Ident: Clone + fmt::Debug + PartialEq;

    /// How a *type name reference* is represented inside `TypeDescription`.
    ///
    /// `UnresolvedTypeRef` in `Parsed` - a raw string path from source.
    /// `DefId` in `Typed` - an index into the `SymbolTable` after resolution.
    type TypeRef: Clone + fmt::Debug + PartialEq;
}

/// Phase 1 - output of the parser. Raw string names, no type annotations.
#[derive(Debug, Clone, PartialEq)]
pub struct Parsed;
impl sealed::Sealed for Parsed {}
impl Phase for Parsed {
    type ExprType = ();
    type Ident    = UnresolvedIdent;
    type TypeRef  = UnresolvedTypeRef;
}

/// Phase 2 - output of the resolver + type inferencer.
/// Every expression has a type; every name reference has a `DefId`.
#[derive(Debug, Clone, PartialEq)]
pub struct Typed;
impl sealed::Sealed for Typed {}
impl Phase for Typed {
    type ExprType = TypeExpr<Typed>;
    type Ident    = DefId;
    type TypeRef  = DefId;
}

/// An opaque index into a `SymbolTable`
///
/// copyable, 4 bytes 
/// The actual name, kind, and type live in the table - looked up on demand
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(pub u32);

/// A name reference as it appears in source before resolution
#[derive(Debug, Clone, PartialEq)]
pub struct UnresolvedIdent {
    pub segments:  Vec<WithSpan<String>>,
    /// True when the path starts with `::` (rooted at the crate root)
    pub is_global: bool,
}

impl UnresolvedIdent {
    pub fn simple(name: WithSpan<String>) -> Self {
        Self { segments: vec![name], is_global: false }
    }

    pub fn path(segments: Vec<WithSpan<String>>, is_global: bool) -> Self {
        Self { segments, is_global }
    }

    pub fn span(&self) -> Span {
        let first = self.segments.first().expect("empty ident").span;
        let last  = self.segments.last().expect("empty ident").span;
        first.to(last)
    }
}

/// A type name reference as it appears in source before resolution.
///
/// `path` has one entry for a simple `Foo`, multiple for `pkg::Foo`.
/// After resolution the resolver replaces this with a `DefId` (`Typed::TypeRef`).
#[derive(Debug, Clone, PartialEq)]
pub struct UnresolvedTypeRef {
    pub path:      Vec<String>,
    pub is_global: bool,
}

/// What kind of binding a `DefId` refers to
#[derive(Debug, Clone, PartialEq)]
pub enum DefKind {
    Local   { is_mut: bool },
    Param   { is_mut: bool },
    Function { is_static: bool },
    Struct,
    TypeAlias,
    Const,
    GlobalVar,
}

/// One entry in the symbol table - the source of truth for a definition
#[derive(Debug, Clone, PartialEq)]
pub struct SymbolDef {
    pub kind: DefKind,
    /// Original name as written in source (before mangling or lowering).
    pub name: String,
    pub span: Span,
    /// Filled in by the type inferencer; `None` only during name resolution.
    pub ty: Option<TypeExpr<Typed>>,
}

/// Built incrementally during name resolution, then read by type inference and emit
///
/// Keeping this separate from the AST means the tree itself stays immutable after
/// construction - the table is the mutable side-channel for semantic information
#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    defs: Vec<SymbolDef>,
}

impl SymbolTable {
    pub fn insert(&mut self, def: SymbolDef) -> DefId {
        let id = DefId(self.defs.len() as u32);
        self.defs.push(def);
        id
    }

    pub fn get(&self, id: DefId) -> &SymbolDef {
        &self.defs[id.0 as usize]
    }

    pub fn get_mut(&mut self, id: DefId) -> &mut SymbolDef {
        &mut self.defs[id.0 as usize]
    }

    pub fn len(&self) -> usize {
        self.defs.len()
    }
}

/// Lightweight source location: start/end byte offsets + a file index.
///
/// The old system stored `file_name: &'static str` and `file_contents: &'static str`
/// inside every span - 32 extra bytes cloned with every AST node. Here only an index
/// is stored; call-sites look up file info in a table when they need it for diagnostics.
///
/// Layout: 4 + 4 + 2 = 10 bytes, padded to 12. `Copy`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start:   u32,
    pub end:     u32,
    pub file_id: u16,
}

impl Span {
    pub fn new(start: u32, end: u32, file_id: u16) -> Self {
        Self { start, end, file_id }
    }

    /// utility for synthesized nodes with no real source location.
    pub fn dummy() -> Self {
        Self { start: 0, end: 0, file_id: u16::MAX }
    }

    pub fn is_dummy(self) -> bool {
        self.file_id == u16::MAX
    }

    /// extend to cover `other`; both must be from the same file.
    pub fn to(self, other: Span) -> Span {
        debug_assert_eq!(self.file_id, other.file_id);
        Span {
            start:   self.start.min(other.start),
            end:     self.end.max(other.end),
            file_id: self.file_id,
        }
    }
}

/// A value together with its source location.
///
/// Replaces the old `(T, SS)` tuple pattern. Named fields make call-sites
/// self-documenting and eliminate the `.0` / `.1` noise that made the old
/// parser hard to follow.
#[derive(Debug, Clone, PartialEq)]
pub struct WithSpan<T> {
    pub value: T,
    pub span:  Span,
}

impl<T: Copy> Copy for WithSpan<T> {}

impl<T> WithSpan<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }

    pub fn dummy(value: T) -> Self {
        Self { value, span: Span::dummy() }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn as_ref(&self) -> WithSpan<&T> {
        WithSpan { value: &self.value, span: self.span }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WithSpan<U> {
        WithSpan { value: f(self.value), span: self.span }
    }
}

/// A single named, typed field: `<name>: <type>`.
///
/// Both pieces carry independent spans so a diagnostic can point at the name
/// or the type separately.
#[derive(Debug, Clone, PartialEq)]
pub struct Field<P: Phase> {
    pub name:      WithSpan<String>,
    pub type_expr: TypeExpr<P>,
}

/// A type parameter in a definition, e.g. the `T` in `fn foo<T: Eq>`.
///
/// Bindings (where a new name is *introduced*) stay as `WithSpan<String>` in
/// both phases - they are definitions, not references, so they do not go through
/// the resolver and do not get a `DefId`.
#[derive(Debug, Clone, PartialEq)]
pub struct Generic<P: Phase> {
    pub name:       WithSpan<String>,
    pub constraint: Option<TypeExpr<P>>,
}

/// A parameter slot inside a `fun(…)` type, e.g. `label: SomeType`.
#[derive(Debug, Clone, PartialEq)]
pub struct FunTypeParam<P: Phase> {
    pub label:     Option<WithSpan<String>>,
    pub type_expr: TypeExpr<P>,
}

/// A structural duck type: `{ field: type, ... }`.
#[derive(Debug, Clone, PartialEq)]
pub struct DuckType<P: Phase> {
    pub fields: Vec<Field<P>>,
}

/// A type expression. Generic over `P` because `TypeDescription<P>` contains
/// `TypeName` whose reference form differs between phases.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeExpr<P: Phase> {
    pub desc: TypeDescription<P>,
    pub span: Span,
}

impl<P: Phase> TypeExpr<P> {
    pub fn new(desc: TypeDescription<P>, span: Span) -> Self {
        Self { desc, span }
    }
}

/// All forms a type expression can take.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeDescription<P: Phase> {
    // Primitives
    Int,
    UInt,
    Float,
    Bool(Option<bool>),     // bare `Bool`, or a constant true/false singleton
    Char,
    Byte,
    String(Option<String>), // bare `String`, or a constant string singleton

    // Built-in special
    Statement,  // unit type for statement positions
    Never,      // bottom type `!`
    Html,       // JSX-style HTML value
    Any,        // dynamic / escape-hatch

    // tag types - nominal unit types like `.ok` or `.err`
    Tag(String),

    // introspection
    TypeOf(String),
    KeyOf(Box<TypeExpr<P>>),

    // composite
    Tuple(Vec<TypeExpr<P>>),
    Array(Box<TypeExpr<P>>),
    Indexed { base: Box<TypeExpr<P>>, index: Box<TypeExpr<P>> },

    // algebraic
    Or(Vec<TypeExpr<P>>),   // union        T1 | T2 | ..
    And(Vec<TypeExpr<P>>),  // intersection T1 & T2 & ..

    // fn type
    Fun {
        params:      Vec<FunTypeParam<P>>,
        return_type: Box<TypeExpr<P>>,
        is_mut:      bool,
    },

    // References
    Ref(Box<TypeExpr<P>>),
    RefMut(Box<TypeExpr<P>>),

    // Named type reference.
    // `Parsed`:  `type_ref` is an `UnresolvedTypeRef` (raw string path from source).
    // `Typed`:   `type_ref` is a `DefId` pointing into the `SymbolTable`.
    TypeName {
        type_ref:    P::TypeRef,
        type_params: Vec<TypeExpr<P>>,
    },

    /// A generic parameter introduced at a definition site, e.g. `T` in `fn foo<T>`.
    /// Stays as a string in both phases; the resolver creates a scoped `DefId` for it
    /// separately and does not rewrite this variant.
    TemplParam(String),

    // Structural duck types
    Duck(DuckType<P>),
    NamedDuck { name: String, type_params: Vec<TypeExpr<P>> },
    Struct     { name: String, type_params: Vec<TypeExpr<P>> },

    // Inline Go - escape hatch
    Go(String),
}

/// `type <name>[<generics>] = <type_expr>`
#[derive(Debug, Clone, PartialEq)]
pub struct TypeAliasDecl<P: Phase> {
    pub name:      WithSpan<String>,
    pub generics:  Vec<WithSpan<Generic<P>>>,
    pub type_expr: TypeExpr<P>,
    pub span:      Span,
}

/// `struct <name>[<generics>] { <fields> }`
#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl<P: Phase> {
    pub name:     WithSpan<String>,
    pub generics: Vec<WithSpan<Generic<P>>>,
    pub fields:   Vec<Field<P>>,
    pub span:     Span,
}

/// A named, typed function parameter - used in both definitions and lambdas.
///
/// The parameter name is a *binding* (new name introduced here), so it stays
/// `WithSpan<String>` in both phases. A `DefId` is created for it in the
/// `SymbolTable` during name resolution, but the AST node itself doesn't change.
#[derive(Debug, Clone, PartialEq)]
pub struct Param<P: Phase> {
    pub name:      WithSpan<String>,
    pub type_expr: TypeExpr<P>,
    pub is_mut:    bool,
}

/// `fn <name>[<generics>](<params>) [--> <return_type>] { <body> }`
///
/// `P` propagates through the body expression, so
/// `FunctionDecl<Parsed>` has an untyped body and `FunctionDecl<Typed>` has a
/// fully-typed body where every sub-expression carries a `TypeExpr<Typed>`.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl<P: Phase> {
    pub name:        WithSpan<String>,
    pub generics:    Vec<WithSpan<Generic<P>>>,
    pub params:      Vec<Param<P>>,
    pub return_type: Option<TypeExpr<P>>,
    pub body:        Expr<P>,
    pub is_static:   bool,
    pub span:        Span,
}

/// An expression node. Carries its phase-dependent payload and its source span.
///
/// In `Parsed`: `ty` is `()` - zero-sized, the field is optimized away by the
/// compiler, so `Expr<Parsed>` is no larger than if the field were absent.
///
/// In `Typed`: `ty` is a `TypeExpr` - guaranteed present, so the emit pass can
/// read it without any `unwrap` or match on an `Option`.
#[derive(Debug, Clone, PartialEq)]
pub struct Expr<P: Phase> {
    pub kind: ExprKind<P>,
    pub ty:   P::ExprType,
    pub span: Span,
}

impl Expr<Parsed> {
    /// construct a parsed expression. `ty` is inferred as `()`.
    pub fn parsed(kind: ExprKind<Parsed>, span: Span) -> Self {
        Self { kind, ty: (), span }
    }
}

impl Expr<Typed> {
    /// Construct a typed expression with an explicit type annotation.
    pub fn typed(kind: ExprKind<Typed>, ty: TypeExpr<Typed>, span: Span) -> Self {
        Self { kind, ty, span }
    }
}

/// All expression forms. Generic over `P` only because it contains `Expr<P>` children.
///
/// The `Ident` variant uses `P::Ident` to encode the pre/post-resolution distinction
/// without duplicate variants. In `Parsed` it holds an `UnresolvedIdent` (one or more raw path segments); in `Typed` it holds a `DefId`. 
/// This replaces the old `RawVariable(bool, Vec<String>)` + `Variable(…)` pair and the separate `Path` variant.
#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind<P: Phase> {
    // literals
    Int(u64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    FmtString(Vec<FmtPart<P>>),
    Tag(String),

    // name reference
    /// covers both simple `foo` and qualified `pkg::Foo` in `Parsed`
    /// becomes a bare `DefId` after name resolution in `Typed`.
    Ident(P::Ident),

    // block 
    /// A sequence of expression/ the value of the block is the last element.
    Block(Vec<Expr<P>>),

    // Let / const declarations (expression-level)
    Let {
        is_mut:   bool,
        name:     WithSpan<String>,
        type_ann: Option<TypeExpr<P>>,
        value:    Box<Expr<P>>,
    },
    Const {
        name:     WithSpan<String>,
        type_ann: Option<TypeExpr<P>>,
        value:    Box<Expr<P>>,
    },

    // Assignment operators 
    Assign    { target: Box<Expr<P>>, value: Box<Expr<P>> },
    AddAssign { target: Box<Expr<P>>, value: Box<Expr<P>> },
    SubAssign { target: Box<Expr<P>>, value: Box<Expr<P>> },
    MulAssign { target: Box<Expr<P>>, value: Box<Expr<P>> },
    DivAssign { target: Box<Expr<P>>, value: Box<Expr<P>> },
    ModAssign { target: Box<Expr<P>>, value: Box<Expr<P>> },
    ShrAssign { target: Box<Expr<P>>, value: Box<Expr<P>> },
    ShlAssign { target: Box<Expr<P>>, value: Box<Expr<P>> },

    // aritmetic 
    Add(Box<Expr<P>>, Box<Expr<P>>),
    Sub(Box<Expr<P>>, Box<Expr<P>>),
    Mul(Box<Expr<P>>, Box<Expr<P>>),
    Div(Box<Expr<P>>, Box<Expr<P>>),
    Mod(Box<Expr<P>>, Box<Expr<P>>),

    // bitwise 
    BitAnd(Box<Expr<P>>, Box<Expr<P>>),
    BitOr (Box<Expr<P>>, Box<Expr<P>>),
    BitXor(Box<Expr<P>>, Box<Expr<P>>),
    BitNot(Box<Expr<P>>),
    Shl(Box<Expr<P>>, Box<Expr<P>>),
    Shr(Box<Expr<P>>, Box<Expr<P>>),

    // comparison
    Eq (Box<Expr<P>>, Box<Expr<P>>),
    Neq(Box<Expr<P>>, Box<Expr<P>>),
    Lt (Box<Expr<P>>, Box<Expr<P>>),
    Lte(Box<Expr<P>>, Box<Expr<P>>),
    Gt (Box<Expr<P>>, Box<Expr<P>>),
    Gte(Box<Expr<P>>, Box<Expr<P>>),

    // logical 
    And(Box<Expr<P>>, Box<Expr<P>>),
    Or (Box<Expr<P>>, Box<Expr<P>>),
    Not(Box<Expr<P>>),

    // unary
    Neg   (Box<Expr<P>>),
    Ref   (Box<Expr<P>>),
    RefMut(Box<Expr<P>>),
    Deref (Box<Expr<P>>),

    // access
    Field {
        base:  Box<Expr<P>>,
        /// Field names are structural labels, not binding references, so they
        /// stay as strings in both phases and do not go through the resolver.
        field: WithSpan<String>,
    },
    Index {
        base:  Box<Expr<P>>,
        index: Box<Expr<P>>,
    },
    /// `base::member` - scope resolution. Only valid in `Parsed`; the resolver
    /// either rewrites it into `Ident(DefId)` or keeps it as a qualified access.
    ScopeRes {
        base:   Box<Expr<P>>,
        member: WithSpan<String>,
    },

    // Function calls
    Call {
        callee:      Box<Expr<P>>,
        type_params: Vec<TypeExpr<P>>,
        args:        Vec<Expr<P>>,
    },

    // Type cast
    As {
        value:     Box<Expr<P>>,
        type_expr: TypeExpr<P>,
    },

    // Control flow 
    If {
        condition:   Box<Expr<P>>,
        then_branch: Box<Expr<P>>,
        else_branch: Option<Box<Expr<P>>>,
    },
    While {
        condition: Box<Expr<P>>,
        body:      Box<Expr<P>>,
    },
    For {
        binding:  WithSpan<String>,
        is_mut:   bool,
        iterable: Box<Expr<P>>,
        body:     Box<Expr<P>>,
    },
    Return(Option<Box<Expr<P>>>),
    Break,
    Continue,

    // Match
    Match {
        value:    Box<Expr<P>>,
        arms:     Vec<MatchArm<P>>,
        else_arm: Option<Box<Expr<P>>>,
    },

    // "constructors"
    StructLit {
        name:        P::Ident,
        type_params: Vec<TypeExpr<P>>,
        /// Field labels are structural, not resolved - `WithSpan<String>` in both phases.
        fields:      Vec<(WithSpan<String>, Expr<P>)>,
    },
    DuckLit(Vec<(WithSpan<String>, Expr<P>)>),
    Array(Vec<Expr<P>>),
    Tuple(Vec<Expr<P>>),

    // lambda
    Lambda {
        is_mut:      bool,
        params:      Vec<Param<P>>,
        return_type: Option<TypeExpr<P>>,
        body:        Box<Expr<P>>,
    },

    // async / defer
    Async(Box<Expr<P>>),
    Defer(Box<Expr<P>>),

    // escape hatches
    InlineGo(String),
}

/// A piece of an interpolated format string.
#[derive(Debug, Clone, PartialEq)]
pub enum FmtPart<P: Phase> {
    Literal(String),
    Expr(Expr<P>),
}

/// One arm of a match expression.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm<P: Phase> {
    pub pattern: TypeExpr<P>,
    pub base:    Option<TypeExpr<P>>,
    pub binding: Option<WithSpan<String>>,
    pub guard:   Option<Box<Expr<P>>>,
    pub body:    Expr<P>,
    pub span:    Span,
}

/// `use duck::path::to::item` or `use go "pkg/path"`.
#[derive(Debug, Clone, PartialEq)]
pub enum UseDecl {
    Duck(Vec<WithSpan<String>>, Span),
    Go  (Vec<WithSpan<String>>, Span),
}

impl UseDecl {
    pub fn span(&self) -> Span {
        match self {
            UseDecl::Duck(_, s) | UseDecl::Go(_, s) => *s,
        }
    }
}

/// `extend <type_expr> with impl { <methods> }`
#[derive(Debug, Clone, PartialEq)]
pub struct ExtensionDecl<P: Phase> {
    pub target:  TypeExpr<P>,
    pub methods: Vec<FunctionDecl<P>>,
    pub span:    Span,
}

// top level items
#[derive(Debug, Clone, PartialEq)]
pub enum Item<P: Phase> {
    Function (FunctionDecl<P>),
    TypeAlias(TypeAliasDecl<P>),
    Struct   (StructDecl<P>),
    Use      (UseDecl),
    Extension(ExtensionDecl<P>),
}

impl<P: Phase> Item<P> {
    pub fn span(&self) -> Span {
        match self {
            Item::Function(f)  => f.span,
            Item::TypeAlias(t) => t.span,
            Item::Struct(s)    => s.span,
            Item::Use(u)       => u.span(),
            Item::Extension(e) => e.span,
        }
    }
}

/// a complete parsed (or resolved/typed) source file.
#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile<P: Phase> {
    pub items: Vec<Item<P>>,
}

impl<P: Phase> SourceFile<P> {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn push(&mut self, item: Item<P>) {
        self.items.push(item);
    }
}

impl<P: Phase> Default for SourceFile<P> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub msg:  String,
    pub span: Span,
}

pub fn parse(
    tokens: Vec<WithSpan<Token>>,
    file_id: u16,
) -> (SourceFile<Parsed>, Vec<ParseError>) {
    let mut p = Parser::new(tokens, file_id);
    let sf = p.parse_source_file();
    (sf, p.errors)
}

/// Parse a single expression from `tokens`. Used by tests in `parser_tests.rs`.
pub(crate) fn parse_single_expr(
    tokens: Vec<WithSpan<Token>>,
    file_id: u16,
) -> (Option<Expr<Parsed>>, Vec<ParseError>) {
    let mut p = Parser::new(tokens, file_id);
    let expr = p.parse_expr();
    (expr, p.errors)
}

struct Parser {
    tokens:  Vec<WithSpan<Token>>,
    pos:     usize,
    file_id: u16,
    errors:  Vec<ParseError>,
}

impl Parser {
    fn new(tokens: Vec<WithSpan<Token>>, file_id: u16) -> Self {
        Self { tokens, pos: 0, file_id, errors: Vec::new() }
    }

    fn at_end(&self) -> bool {
         self.pos >= self.tokens.len() 
    }

    fn peek(&self) -> Option<&WithSpan<Token>> {
        self.tokens.get(self.pos) 
    }

    fn peek2(&self) -> Option<&WithSpan<Token>> {
        self.tokens.get(self.pos + 1) 
    }

    fn peek_at(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.pos + offset)
            .map(|t| &t.value)
    }

    fn peek_kind(&self) -> Option<&Token> {
        self.peek().map(|t| &t.value) 
    }

    fn peek2_kind(&self) -> Option<&Token> {
        self.peek2().map(|t| &t.value) 
    }

    fn current_span(&self) -> Span {
        self.peek().map(|t| t.span).unwrap_or(Span::new(0, 0, self.file_id))
    }

    fn prev_span(&self) -> Span {
        if self.pos > 0 { self.tokens[self.pos - 1].span }
        else { Span::new(0, 0, self.file_id) }
    }

    fn advance(&mut self) -> Option<WithSpan<Token>> {
        if self.pos < self.tokens.len() {
            let tok = self.tokens[self.pos].clone();
            self.pos += 1;
            Some(tok)
        } else {
            None
        }
    }

    fn eat_ctrl(&mut self, c: char) -> Option<Span> {
        if matches!(self.peek_kind(), Some(Token::Ctrl(x)) if *x == c) {
            Some(self.advance().unwrap().span)
        } else {
            None
        }
    }

    fn expect_ctrl(&mut self, c: char) -> Span {
        if self.eat_ctrl(c).is_none() {
            self.error(format!("expected '{c}'"));
        }
        self.prev_span()
    }

    // match exact keyword/operator tokens (no payload).
    fn eat_kw(&mut self, kind: &Token) -> Option<Span> {
        if self.peek_kind() == Some(kind) {
            Some(self.advance().unwrap().span)
        } else {
            None
        }
    }

    fn eat_ident(&mut self) -> Option<WithSpan<String>> {
        match self.peek_kind() {
            Some(Token::Ident(_)) => {
                let tok = self.advance().unwrap();
                if let Token::Ident(s) = tok.value {
                    Some(WithSpan::new(s, tok.span))
                } else { unreachable!() }
            }
            _ => None,
        }
    }

    fn expect_ident(&mut self) -> WithSpan<String> {
        if let Some(id) = self.eat_ident() { return id; }
        self.error("expected identifier");
        WithSpan::new(String::new(), self.prev_span())
    }

    fn error(&mut self, msg: impl Into<String>) {
        let span = self.current_span();
        self.errors.push(ParseError { msg: msg.into(), span });
    }

    // skip doc-comment tokens (kept by tokenize_no_comments).
    fn skip_doc_comments(&mut self) {
        while matches!(self.peek_kind(), Some(Token::DocComment(_))) {
            self.advance();
        }
    }

    fn parse_source_file(&mut self) -> SourceFile<Parsed> {
        let mut sf = SourceFile::new();
        while !self.at_end() {
            self.skip_doc_comments();
            if self.at_end() { break; }
            if let Some(item) = self.parse_item() {
                sf.push(item);
            } else {
                self.error(format!("unexpected token at top level: {:?}", self.peek_kind()));
                self.advance();
            }
        }
        sf
    }

    fn parse_item(&mut self) -> Option<Item<Parsed>> {
        match self.peek_kind()? {
            Token::Fn     => self.parse_function_decl(false).map(Item::Function),
            Token::Static => {
                self.advance();
                if matches!(self.peek_kind(), Some(Token::Fn)) {
                    self.parse_function_decl(true).map(Item::Function)
                } else {
                    self.error("expected 'fn' after 'static'");
                    None
                }
            }
            Token::Type   => self.parse_type_alias_decl().map(Item::TypeAlias),
            Token::Struct => self.parse_struct_decl().map(Item::Struct),
            Token::Use    => self.parse_use_decl().map(Item::Use),
            Token::Extend => self.parse_extension_decl().map(Item::Extension),
            _ => None,
        }
    }

    fn parse_function_decl(&mut self, is_static: bool) -> Option<FunctionDecl<Parsed>> {
        let start = self.current_span();
        self.advance(); // eat 'fn'
        let name = self.expect_ident();
        let generics = self.parse_generics();
        self.expect_ctrl('(');
        let params = self.parse_params();
        self.expect_ctrl(')');
        let return_type = if self.eat_kw(&Token::ThinArrow).is_some() {
            self.parse_type_expr()
        } else {
            None
        };
        let body = self.parse_block()?;
        let span = start.to(body.span);
        Some(FunctionDecl { name, generics, params, return_type, body, is_static, span })
    }

    fn parse_generics(&mut self) -> Vec<WithSpan<Generic<Parsed>>> {
        if !matches!(self.peek_kind(), Some(Token::Ctrl('<'))) { return Vec::new(); }
        self.advance(); // eat '<'
        let mut out = Vec::new();
        loop {
            if matches!(self.peek_kind(), Some(Token::Ctrl('>'))) || self.at_end() { break; }
            let name = self.expect_ident();
            let gs = name.span;
            let constraint = if self.eat_ctrl(':').is_some() { self.parse_type_expr() } else { None };
            let end = constraint.as_ref().map(|t| t.span).unwrap_or(gs);
            out.push(WithSpan::new(Generic { name, constraint }, gs.to(end)));
            if self.eat_ctrl(',').is_none() { break; }
        }
        self.expect_ctrl('>');
        out
    }

    fn parse_params(&mut self) -> Vec<Param<Parsed>> {
        let mut out = Vec::new();
        loop {
            if matches!(self.peek_kind(), Some(Token::Ctrl(')'))) || self.at_end() { break; }
            let is_mut = self.eat_kw(&Token::Mut).is_some();
            let name = self.expect_ident();
            self.expect_ctrl(':');
            let type_expr = match self.parse_type_expr() {
                Some(t) => t,
                None => break,
            };
            out.push(Param { name, type_expr, is_mut });
            if self.eat_ctrl(',').is_none() { break; }
        }
        out
    }

    fn parse_type_alias_decl(&mut self) -> Option<TypeAliasDecl<Parsed>> {
        let start = self.current_span();
        self.advance(); // eat 'type'
        let name = self.expect_ident();
        let generics = self.parse_generics();
        self.expect_ctrl('=');
        let type_expr = self.parse_type_expr()?;
        let end = type_expr.span;
        self.eat_ctrl(';');
        Some(TypeAliasDecl { name, generics, type_expr, span: start.to(end) })
    }


    fn parse_struct_decl(&mut self) -> Option<StructDecl<Parsed>> {
        let start = self.current_span();
        self.advance(); // eat 'struct'
        let name = self.expect_ident();
        let generics = self.parse_generics();
        self.expect_ctrl('{');
        let fields = self.parse_named_fields();
        self.expect_ctrl('}');
        let span = start.to(self.prev_span());
        Some(StructDecl { name, generics, fields, span })
    }

    fn parse_named_fields(&mut self) -> Vec<Field<Parsed>> {
        let mut out = Vec::new();
        loop {
            if matches!(self.peek_kind(), Some(Token::Ctrl('}'))) || self.at_end() { break; }
            let name = self.expect_ident();
            self.expect_ctrl(':');
            let type_expr = match self.parse_type_expr() {
                Some(t) => t,
                None => break,
            };
            out.push(Field { name, type_expr });
            if self.eat_ctrl(',').is_none() { break; }
        }
        out
    }

    fn parse_use_decl(&mut self) -> Option<UseDecl> {
        let start = self.current_span();
        self.advance(); // eat 'use'

        // use go "pkg/path" as alias;
        // 'go' is Token::Ident("go") unless followed by '{' (which makes it InlineGo).
        let peek_is_go = matches!(self.peek_kind(), Some(Token::Go))
            || matches!(self.peek_kind(), Some(Token::Ident(s)) if s == "go");
        if peek_is_go {
            self.advance();
            let path_tok = self.advance()?;
            if let Token::String(path) = path_tok.value {
                let alias = if self.eat_kw(&Token::As).is_some() {
                    Some(self.expect_ident())
                } else {
                    None
                };
                let end = alias.as_ref().map(|a| a.span).unwrap_or(path_tok.span);
                self.eat_ctrl(';');
                let mut segs = vec![WithSpan::new(path, path_tok.span)];
                if let Some(a) = alias { segs.push(a); }
                return Some(UseDecl::Go(segs, start.to(end)));
            }
            self.error("expected string literal after 'use go'");
            return None;
        }

        // use ::? ident :: ident :: ... ;
        let _is_global = self.eat_kw(&Token::ScopeRes).is_some();
        let mut segs: Vec<WithSpan<String>> = Vec::new();

        // handle brace groups: use std::{a, b};
        loop {
            if matches!(self.peek_kind(), Some(Token::Ctrl('{'))) {
                self.advance();
                let mut group = Vec::new();
                loop {
                    if matches!(self.peek_kind(), Some(Token::Ctrl('}'))) || self.at_end() { break; }
                    group.push(self.expect_ident());
                    if self.eat_ctrl(',').is_none() { break; }
                }
                self.expect_ctrl('}');
                // Each item gets its own UseDecl — return first and leave rest for caller.
                // For simplicity, flatten into one UseDecl with the path prefix + each item.
                // We return only the first item here; caller loops for more.
                // Better: treat the whole path as the UseDecl (simplified approach).
                segs.extend(group);
                break;
            }
            match self.eat_ident() {
                Some(seg) => segs.push(seg),
                None => break,
            }
            if self.eat_kw(&Token::ScopeRes).is_none() { break; }
        }

        if segs.is_empty() {
            self.error("expected identifier in use statement");
            return None;
        }
        let end = segs.last().unwrap().span;
        self.eat_ctrl(';');
        Some(UseDecl::Duck(segs, start.to(end)))
    }

    // Extension declaration 

    fn parse_extension_decl(&mut self) -> Option<ExtensionDecl<Parsed>> {
        let start = self.current_span();
        self.advance(); // eat 'extend'
        let target = self.parse_type_expr()?;
        self.eat_kw(&Token::With);
        self.eat_kw(&Token::Impl);
        self.expect_ctrl('{');
        let mut methods = Vec::new();
        loop {
            self.skip_doc_comments();
            if matches!(self.peek_kind(), Some(Token::Ctrl('}'))) || self.at_end() { break; }
            let is_static = self.eat_kw(&Token::Static).is_some();
            if matches!(self.peek_kind(), Some(Token::Fn)) {
                if let Some(f) = self.parse_function_decl(is_static) {
                    methods.push(f);
                }
            } else {
                self.error("expected 'fn' in extension block");
                self.advance();
            }
        }
        self.expect_ctrl('}');
        let span = start.to(self.prev_span());
        Some(ExtensionDecl { target, methods, span })
    }

    // type expressions

    fn parse_type_expr(&mut self) -> Option<TypeExpr<Parsed>> {
        self.parse_type_or()
    }

    fn parse_type_or(&mut self) -> Option<TypeExpr<Parsed>> {
        let first = self.parse_type_and()?;
        if !matches!(self.peek_kind(), Some(Token::Ctrl('|'))) {
            return Some(first);
        }
        let mut variants = vec![first];
        while self.eat_ctrl('|').is_some() {
            if let Some(t) = self.parse_type_and() { variants.push(t); } else { break; }
        }
        let span = variants.first().unwrap().span.to(variants.last().unwrap().span);
        Some(TypeExpr::new(TypeDescription::Or(variants), span))
    }

    fn parse_type_and(&mut self) -> Option<TypeExpr<Parsed>> {
        let first = self.parse_type_postfix()?;
        if !matches!(self.peek_kind(), Some(Token::Ctrl('&'))) {
            return Some(first);
        }
        let mut variants = vec![first];
        while self.eat_ctrl('&').is_some() {
            if let Some(t) = self.parse_type_postfix() { variants.push(t); } else { break; }
        }
        let span = variants.first().unwrap().span.to(variants.last().unwrap().span);
        Some(TypeExpr::new(TypeDescription::And(variants), span))
    }

    fn parse_type_postfix(&mut self) -> Option<TypeExpr<Parsed>> {
        let mut ty = self.parse_type_atom()?;
        // T[IndexType] — indexed type
        while matches!(self.peek_kind(), Some(Token::Ctrl('['))) {
            let open = self.current_span();
            self.advance();
            if let Some(index) = self.parse_type_expr() {
                self.expect_ctrl(']');
                let span = ty.span.to(self.prev_span());
                ty = TypeExpr::new(TypeDescription::Indexed {
                    base:  Box::new(ty),
                    index: Box::new(index),
                }, span);
            } else {
                self.expect_ctrl(']');
                let span = ty.span.to(open);
                ty = TypeExpr::new(TypeDescription::Array(Box::new(ty)), span);
            }
        }
        Some(ty)
    }

    fn parse_type_atom(&mut self) -> Option<TypeExpr<Parsed>> {
        let start = self.current_span();

        // &mut T
        if let Some(sp) = self.eat_kw(&Token::RefMut) {
            let inner = self.parse_type_atom()?;
            let span = sp.to(inner.span);
            return Some(TypeExpr::new(TypeDescription::RefMut(Box::new(inner)), span));
        }
        // &T
        if self.eat_ctrl('&').is_some() {
            let inner = self.parse_type_atom()?;
            let end = inner.span;
            return Some(TypeExpr::new(TypeDescription::Ref(Box::new(inner)), start.to(end)));
        }
        // ! → Never
        if self.eat_ctrl('!').is_some() {
            return Some(TypeExpr::new(TypeDescription::Never, start));
        }
        // [T] → Array
        if self.eat_ctrl('[').is_some() {
            let inner = self.parse_type_expr()?;
            self.expect_ctrl(']');
            return Some(TypeExpr::new(TypeDescription::Array(Box::new(inner)), start.to(self.prev_span())));
        }
        // (T, U) → Tuple, or () → unit
        if self.eat_ctrl('(').is_some() {
            if self.eat_ctrl(')').is_some() {
                return Some(TypeExpr::new(TypeDescription::Tuple(vec![]), start.to(self.prev_span())));
            }
            let first = self.parse_type_expr()?;
            if self.eat_ctrl(')').is_some() {
                return Some(first); // parenthesized type
            }
            self.expect_ctrl(',');
            let mut elems = vec![first];
            loop {
                if matches!(self.peek_kind(), Some(Token::Ctrl(')'))) || self.at_end() { break; }
                if let Some(t) = self.parse_type_expr() { elems.push(t); } else { break; }
                if self.eat_ctrl(',').is_none() { break; }
            }
            self.expect_ctrl(')');
            return Some(TypeExpr::new(TypeDescription::Tuple(elems), start.to(self.prev_span())));
        }
        // typeof name
        if self.eat_kw(&Token::Typeof).is_some() {
            let name = self.expect_ident();
            return Some(TypeExpr::new(TypeDescription::TypeOf(name.value), start.to(name.span)));
        }
        // keyof T
        if self.eat_kw(&Token::Keyof).is_some() {
            let inner = self.parse_type_atom()?;
            let end = inner.span;
            return Some(TypeExpr::new(TypeDescription::KeyOf(Box::new(inner)), start.to(end)));
        }
        // duck { fields } or { fields } (duck type, not block)
        if matches!(self.peek_kind(), Some(Token::Duck))
            && matches!(self.peek2_kind(), Some(Token::Ctrl('{'))) {
            self.advance(); // eat 'duck'
            self.advance(); // eat '{'
            let fields = self.parse_named_fields();
            self.expect_ctrl('}');
            let span = start.to(self.prev_span());
            return Some(TypeExpr::new(TypeDescription::Duck(DuckType { fields }), span));
        }
        // { fields } as duck type (anonymous structural type) - only if followed by ident:
        if matches!(self.peek_kind(), Some(Token::Ctrl('{'))) {
            let is_duck_type = matches!(self.peek_at(1), Some(Token::Ident(_)))
                && matches!(self.peek_at(2), Some(Token::Ctrl(':')));
            let is_empty_any = matches!(self.peek_at(1), Some(Token::Ctrl('}')));
            if is_duck_type || is_empty_any {
                self.advance(); // eat '{'
                if is_empty_any {
                    self.advance(); // eat '}'
                    return Some(TypeExpr::new(TypeDescription::Any, start.to(self.prev_span())));
                }
                let fields = self.parse_named_fields();
                self.expect_ctrl('}');
                let span = start.to(self.prev_span());
                return Some(TypeExpr::new(TypeDescription::Duck(DuckType { fields }), span));
            }
        }
        // mut? fn(params) -> RetType — function type
        if matches!(self.peek_kind(), Some(Token::Fn))
            || (matches!(self.peek_kind(), Some(Token::Mut))
                && matches!(self.peek2_kind(), Some(Token::Fn))) {
            let is_mut = self.eat_kw(&Token::Mut).is_some();
            self.advance(); // eat 'fn'
            self.expect_ctrl('(');
            let params = self.parse_fun_type_params();
            self.expect_ctrl(')');
            let return_type = if self.eat_kw(&Token::ThinArrow).is_some() {
                self.parse_type_expr()
                    .unwrap_or_else(|| TypeExpr::new(TypeDescription::Tuple(vec![]), self.current_span()))
            } else {
                TypeExpr::new(TypeDescription::Tuple(vec![]), self.current_span())
            };
            let span = start.to(return_type.span);
            return Some(TypeExpr::new(TypeDescription::Fun {
                params, return_type: Box::new(return_type), is_mut,
            }, span));
        }
        // go "GoTypeName" - 'go' is Ident("go") when not followed by '{'
        let is_go_type = matches!(self.peek_kind(), Some(Token::Go))
            || matches!(self.peek_kind(), Some(Token::Ident(s)) if s == "go");
        if is_go_type {
            self.advance();
            if let Some(Token::String(_)) = self.peek_kind() {
                let tok = self.advance().unwrap();
                if let Token::String(s) = tok.value {
                    return Some(TypeExpr::new(TypeDescription::Go(s), start.to(tok.span)));
                }
            }
            self.error("expected string literal after 'go' in type");
            return None;
        }

        // .name - tag type
        if self.eat_ctrl('.').is_some() {
            let name = self.expect_ident();
            return Some(TypeExpr::new(TypeDescription::Tag(name.value), start.to(name.span)));
        }

        // ::? ident (:: ident)* (<T>)? - named type or path
        let is_global = self.eat_kw(&Token::ScopeRes).is_some();
        if let Some(first) = self.eat_ident() {
            let mut path = vec![first.value.clone()];
            // Check for :: ident continuation
            while matches!(self.peek_kind(), Some(Token::ScopeRes))
                && matches!(self.peek2_kind(), Some(Token::Ident(_))) {
                self.advance(); // eat '::'
                path.push(self.eat_ident().unwrap().value);
            }
            let type_params = self.try_parse_type_angle_params();
            let span = start.to(self.prev_span());

            let desc = if path.len() == 1 && !is_global {
                match path[0].as_str() {
                    "Int"       => TypeDescription::Int,
                    "UInt"      => TypeDescription::UInt,
                    "Float"     => TypeDescription::Float,
                    "Bool"      => TypeDescription::Bool(None),
                    "Char"      => TypeDescription::Char,
                    "Byte"      => TypeDescription::Byte,
                    "String"    => TypeDescription::String(None),
                    "Any" | "any"             => TypeDescription::Any,
                    "Html" | "html"           => TypeDescription::Html,
                    "Statement" | "statement" => TypeDescription::Statement,
                    _ => TypeDescription::TypeName {
                        type_ref: UnresolvedTypeRef { path, is_global: false },
                        type_params,
                    },
                }
            } else {
                TypeDescription::TypeName {
                    type_ref: UnresolvedTypeRef { path, is_global },
                    type_params,
                }
            };
            return Some(TypeExpr::new(desc, span));
        }

        None
    }

    fn parse_fun_type_params(&mut self) -> Vec<FunTypeParam<Parsed>> {
        let mut out = Vec::new();
        loop {
            if matches!(self.peek_kind(), Some(Token::Ctrl(')'))) || self.at_end() { break; }
            let label = if matches!(self.peek_kind(), Some(Token::Ident(_)))
                && matches!(self.peek2_kind(), Some(Token::Ctrl(':'))) {
                let id = self.eat_ident().unwrap();
                self.advance(); // eat ':'
                Some(id)
            } else {
                None
            };
            let type_expr = match self.parse_type_expr() { Some(t) => t, None => break };
            out.push(FunTypeParam { label, type_expr });
            if self.eat_ctrl(',').is_none() { break; }
        }
        out
    }

    // Try to parse <T, U> as type params, restoring position on failure.
    fn try_parse_type_angle_params(&mut self) -> Vec<TypeExpr<Parsed>> {
        if !matches!(self.peek_kind(), Some(Token::Ctrl('<'))) { return Vec::new(); }
        let saved = self.pos;
        self.advance(); // eat '<'
        let mut params = Vec::new();
        loop {
            if matches!(self.peek_kind(), Some(Token::Ctrl('>'))) || self.at_end() { break; }
            if let Some(t) = self.parse_type_expr() { params.push(t); } else {
                self.pos = saved;
                return Vec::new();
            }
            if self.eat_ctrl(',').is_none() { break; }
        }
        if self.eat_ctrl('>').is_none() {
            self.pos = saved;
            return Vec::new();
        }
        params
    }

    fn parse_expr(&mut self) -> Option<Expr<Parsed>> {
        let start = self.current_span();
        match self.peek_kind()? {
            Token::Defer => {
                self.advance();
                let inner = self.parse_expr()?;
                let span = start.to(inner.span);
                return Some(Expr::parsed(ExprKind::Defer(Box::new(inner)), span));
            }
            Token::Async => {
                self.advance();
                let inner = self.parse_expr()?;
                let span = start.to(inner.span);
                return Some(Expr::parsed(ExprKind::Async(Box::new(inner)), span));
            }
            Token::For => return self.parse_for_expr(),
            _ => {}
        }
        self.parse_cast_expr()
    }

    fn parse_cast_expr(&mut self) -> Option<Expr<Parsed>> {
        let expr = self.parse_assign_expr()?;
        if self.eat_kw(&Token::As).is_some() {
            let start = expr.span;
            let type_expr = self.parse_type_expr()?;
            let span = start.to(type_expr.span);
            return Some(Expr::parsed(ExprKind::As { value: Box::new(expr), type_expr }, span));
        }
        Some(expr)
    }

    fn parse_assign_expr(&mut self) -> Option<Expr<Parsed>> {
        let start = self.current_span();

        if matches!(self.peek_kind(), Some(Token::Let)) {
            self.advance();
            let is_mut = self.eat_kw(&Token::Mut).is_some();
            let name = self.expect_ident();
            let type_ann = if self.eat_ctrl(':').is_some() { self.parse_type_expr() } else { None };
            self.expect_ctrl('=');
            let value = self.parse_expr()?;
            let span = start.to(value.span);
            return Some(Expr::parsed(ExprKind::Let { is_mut, name, type_ann, value: Box::new(value) }, span));
        }
        if matches!(self.peek_kind(), Some(Token::Const)) {
            self.advance();
            let name = self.expect_ident();
            let type_ann = if self.eat_ctrl(':').is_some() { self.parse_type_expr() } else { None };
            self.expect_ctrl('=');
            let value = self.parse_expr()?;
            let span = start.to(value.span);
            return Some(Expr::parsed(ExprKind::Const { name, type_ann, value: Box::new(value) }, span));
        }

        let lhs = self.parse_or_expr()?;

        let kind: u8 = match self.peek_kind() {
            Some(Token::Ctrl('=')) => 0,
            Some(Token::PlusEq)   => 1,
            Some(Token::SubEq)    => 2,
            Some(Token::MulEq)    => 3,
            Some(Token::DivEq)    => 4,
            Some(Token::ModEq)    => 5,
            Some(Token::ShrEq)    => 6,
            Some(Token::ShlEq)    => 7,
            _ => return Some(lhs),
        };
        self.advance();
        let rhs = self.parse_expr()?;
        let span = lhs.span.to(rhs.span);
        let target = Box::new(lhs);
        let value  = Box::new(rhs);
        Some(Expr::parsed(match kind {
            0 => ExprKind::Assign    { target, value },
            1 => ExprKind::AddAssign { target, value },
            2 => ExprKind::SubAssign { target, value },
            3 => ExprKind::MulAssign { target, value },
            4 => ExprKind::DivAssign { target, value },
            5 => ExprKind::ModAssign { target, value },
            6 => ExprKind::ShrAssign { target, value },
            7 => ExprKind::ShlAssign { target, value },
            _ => unreachable!(),
        }, span))
    }

    fn parse_or_expr(&mut self) -> Option<Expr<Parsed>> {
        let mut lhs = self.parse_and_expr()?;
        while matches!(self.peek_kind(), Some(Token::Or)) {
            self.advance();
            let rhs = self.parse_and_expr()?;
            let span = lhs.span.to(rhs.span);
            lhs = Expr::parsed(ExprKind::Or(Box::new(lhs), Box::new(rhs)), span);
        }
        Some(lhs)
    }

    fn parse_and_expr(&mut self) -> Option<Expr<Parsed>> {
        let mut lhs = self.parse_equality_expr()?;
        while matches!(self.peek_kind(), Some(Token::And)) {
            self.advance();
            let rhs = self.parse_equality_expr()?;
            let span = lhs.span.to(rhs.span);
            lhs = Expr::parsed(ExprKind::And(Box::new(lhs), Box::new(rhs)), span);
        }
        Some(lhs)
    }

    fn parse_equality_expr(&mut self) -> Option<Expr<Parsed>> {
        let mut lhs = self.parse_relation_expr()?;
        loop {
            match self.peek_kind() {
                Some(Token::EqEq) => {
                    self.advance();
                    let rhs = self.parse_relation_expr()?;
                    let span = lhs.span.to(rhs.span);
                    lhs = Expr::parsed(ExprKind::Eq(Box::new(lhs), Box::new(rhs)), span);
                }
                Some(Token::BangEq) => {
                    self.advance();
                    let rhs = self.parse_relation_expr()?;
                    let span = lhs.span.to(rhs.span);
                    lhs = Expr::parsed(ExprKind::Neq(Box::new(lhs), Box::new(rhs)), span);
                }
                _ => break,
            }
        }
        Some(lhs)
    }

    fn parse_relation_expr(&mut self) -> Option<Expr<Parsed>> {
        let mut lhs = self.parse_add_expr()?;
        loop {
            match self.peek_kind() {
                // < not followed by < (would be Shl)
                Some(Token::Ctrl('<')) if !matches!(self.peek2_kind(), Some(Token::Ctrl('<'))) => {
                    self.advance();
                    let rhs = self.parse_add_expr()?;
                    let span = lhs.span.to(rhs.span);
                    lhs = Expr::parsed(ExprKind::Lt(Box::new(lhs), Box::new(rhs)), span);
                }
                Some(Token::LtEq) => {
                    self.advance();
                    let rhs = self.parse_add_expr()?;
                    let span = lhs.span.to(rhs.span);
                    lhs = Expr::parsed(ExprKind::Lte(Box::new(lhs), Box::new(rhs)), span);
                }
                // > not followed by > (would be Shr)
                Some(Token::Ctrl('>')) if !matches!(self.peek2_kind(), Some(Token::Ctrl('>'))) => {
                    self.advance();
                    let rhs = self.parse_add_expr()?;
                    let span = lhs.span.to(rhs.span);
                    lhs = Expr::parsed(ExprKind::Gt(Box::new(lhs), Box::new(rhs)), span);
                }
                Some(Token::GtEq) => {
                    self.advance();
                    let rhs = self.parse_add_expr()?;
                    let span = lhs.span.to(rhs.span);
                    lhs = Expr::parsed(ExprKind::Gte(Box::new(lhs), Box::new(rhs)), span);
                }
                _ => break,
            }
        }
        Some(lhs)
    }

    fn parse_add_expr(&mut self) -> Option<Expr<Parsed>> {
        let mut lhs = self.parse_prod_expr()?;
        loop {
            match self.peek_kind() {
                Some(Token::Ctrl('+')) => {
                    self.advance();
                    let rhs = self.parse_prod_expr()?;
                    let span = lhs.span.to(rhs.span);
                    lhs = Expr::parsed(ExprKind::Add(Box::new(lhs), Box::new(rhs)), span);
                }
                Some(Token::Ctrl('-')) => {
                    self.advance();
                    let rhs = self.parse_prod_expr()?;
                    let span = lhs.span.to(rhs.span);
                    lhs = Expr::parsed(ExprKind::Sub(Box::new(lhs), Box::new(rhs)), span);
                }
                _ => break,
            }
        }
        Some(lhs)
    }

    fn parse_prod_expr(&mut self) -> Option<Expr<Parsed>> {
        let mut lhs = self.parse_pipeline_expr()?;
        loop {
            match self.peek_kind() {
                Some(Token::Ctrl('*')) => { self.advance(); let r = self.parse_pipeline_expr()?; let s = lhs.span.to(r.span); lhs = Expr::parsed(ExprKind::Mul   (Box::new(lhs), Box::new(r)), s); }
                Some(Token::Ctrl('/')) => { self.advance(); let r = self.parse_pipeline_expr()?; let s = lhs.span.to(r.span); lhs = Expr::parsed(ExprKind::Div   (Box::new(lhs), Box::new(r)), s); }
                Some(Token::Ctrl('%')) => { self.advance(); let r = self.parse_pipeline_expr()?; let s = lhs.span.to(r.span); lhs = Expr::parsed(ExprKind::Mod   (Box::new(lhs), Box::new(r)), s); }
                Some(Token::Ctrl('&')) => { self.advance(); let r = self.parse_pipeline_expr()?; let s = lhs.span.to(r.span); lhs = Expr::parsed(ExprKind::BitAnd(Box::new(lhs), Box::new(r)), s); }
                Some(Token::Ctrl('|')) => { self.advance(); let r = self.parse_pipeline_expr()?; let s = lhs.span.to(r.span); lhs = Expr::parsed(ExprKind::BitOr (Box::new(lhs), Box::new(r)), s); }
                Some(Token::Ctrl('^')) => { self.advance(); let r = self.parse_pipeline_expr()?; let s = lhs.span.to(r.span); lhs = Expr::parsed(ExprKind::BitXor(Box::new(lhs), Box::new(r)), s); }
                Some(Token::Ctrl('<')) if matches!(self.peek2_kind(), Some(Token::Ctrl('<'))) => {
                    self.advance(); self.advance();
                    let r = self.parse_pipeline_expr()?; let s = lhs.span.to(r.span);
                    lhs = Expr::parsed(ExprKind::Shl(Box::new(lhs), Box::new(r)), s);
                }
                Some(Token::Ctrl('>')) if matches!(self.peek2_kind(), Some(Token::Ctrl('>'))) => {
                    self.advance(); self.advance();
                    let r = self.parse_pipeline_expr()?; let s = lhs.span.to(r.span);
                    lhs = Expr::parsed(ExprKind::Shr(Box::new(lhs), Box::new(r)), s);
                }
                _ => break,
            }
        }
        Some(lhs)
    }

    // a -> f(b) = f(a, b)
    fn parse_pipeline_expr(&mut self) -> Option<Expr<Parsed>> {
        let mut lhs = self.parse_unary_expr()?;
        while matches!(self.peek_kind(), Some(Token::ThinArrow)) {
            self.advance();
            let rhs = self.parse_postfix_expr()?;
            let span = lhs.span.to(rhs.span);
            lhs = match rhs.kind {
                ExprKind::Call { callee, type_params, mut args } => {
                    args.insert(0, lhs);
                    Expr::parsed(ExprKind::Call { callee, type_params, args }, span)
                }
                _ => Expr::parsed(ExprKind::Call {
                    callee: Box::new(rhs), type_params: Vec::new(), args: vec![lhs],
                }, span),
            };
        }
        Some(lhs)
    }

    fn parse_unary_expr(&mut self) -> Option<Expr<Parsed>> {
        let start = self.current_span();
        match self.peek_kind() {
            Some(Token::Ctrl('!')) => { self.advance(); let i = self.parse_unary_expr()?; let s = start.to(i.span); return Some(Expr::parsed(ExprKind::Not   (Box::new(i)), s)); }
            Some(Token::Ctrl('-')) => { self.advance(); let i = self.parse_unary_expr()?; let s = start.to(i.span); return Some(Expr::parsed(ExprKind::Neg   (Box::new(i)), s)); }
            Some(Token::Ctrl('~')) => { self.advance(); let i = self.parse_unary_expr()?; let s = start.to(i.span); return Some(Expr::parsed(ExprKind::BitNot(Box::new(i)), s)); }
            Some(Token::Ctrl('*')) => { self.advance(); let i = self.parse_unary_expr()?; let s = start.to(i.span); return Some(Expr::parsed(ExprKind::Deref (Box::new(i)), s)); }
            Some(Token::RefMut)    => { self.advance(); let i = self.parse_unary_expr()?; let s = start.to(i.span); return Some(Expr::parsed(ExprKind::RefMut(Box::new(i)), s)); }
            Some(Token::Ctrl('&')) => { self.advance(); let i = self.parse_unary_expr()?; let s = start.to(i.span); return Some(Expr::parsed(ExprKind::Ref   (Box::new(i)), s)); }
            _ => {}
        }
        self.parse_postfix_expr()
    }

    fn parse_postfix_expr(&mut self) -> Option<Expr<Parsed>> {
        let mut expr = self.parse_primary_expr()?;
        loop {
            match self.peek_kind() {
                Some(Token::Ctrl('.')) => {
                    self.advance();
                    // Numeric field on tuple: expr.0
                    let field = if let Some(Token::Int(_)) = self.peek_kind() {
                        let tok = self.advance().unwrap();
                        if let Token::Int(n) = tok.value {
                            WithSpan::new(n.to_string(), tok.span)
                        } else { unreachable!() }
                    } else {
                        self.expect_ident()
                    };
                    let span = expr.span.to(field.span);
                    expr = Expr::parsed(ExprKind::Field { base: Box::new(expr), field }, span);
                }
                Some(Token::Ctrl('[')) => {
                    self.advance();
                    let index = self.parse_expr()?;
                    self.expect_ctrl(']');
                    let span = expr.span.to(self.prev_span());
                    expr = Expr::parsed(ExprKind::Index { base: Box::new(expr), index: Box::new(index) }, span);
                }
                Some(Token::Ctrl('(')) | Some(Token::Ctrl('<')) => {
                    // Try type params first if '<'
                    let saved = self.pos;
                    let type_params = if matches!(self.peek_kind(), Some(Token::Ctrl('<'))) {
                        self.advance(); // eat '<'
                        let mut tp = Vec::new();
                        let mut ok = true;
                        loop {
                            if matches!(self.peek_kind(), Some(Token::Ctrl('>'))) || self.at_end() { break; }
                            if let Some(t) = self.parse_type_expr() { tp.push(t); } else { ok = false; break; }
                            if self.eat_ctrl(',').is_none() { break; }
                        }
                        if ok && self.eat_ctrl('>').is_some() && matches!(self.peek_kind(), Some(Token::Ctrl('('))) {
                            tp
                        } else {
                            self.pos = saved;
                            Vec::new()
                        }
                    } else {
                        Vec::new()
                    };
                    if !matches!(self.peek_kind(), Some(Token::Ctrl('('))) { break; }
                    self.advance(); // eat '('
                    let mut args = Vec::new();
                    loop {
                        if matches!(self.peek_kind(), Some(Token::Ctrl(')'))) || self.at_end() { break; }
                        if let Some(a) = self.parse_expr() { args.push(a); } else { break; }
                        if self.eat_ctrl(',').is_none() { break; }
                    }
                    self.expect_ctrl(')');
                    let span = expr.span.to(self.prev_span());
                    expr = Expr::parsed(ExprKind::Call { callee: Box::new(expr), type_params, args }, span);
                }
                Some(Token::ScopeRes) => {
                    self.advance();
                    let member = self.expect_ident();
                    let span = expr.span.to(member.span);
                    expr = Expr::parsed(ExprKind::ScopeRes { base: Box::new(expr), member }, span);
                }
                _ => break,
            }
        }
        Some(expr)
    }

    fn parse_primary_expr(&mut self) -> Option<Expr<Parsed>> {
        let start = self.current_span();
        match self.peek_kind()? {
            Token::Int(_) => {
                let tok = self.advance().unwrap();
                let Token::Int(n) = tok.value else { unreachable!() };
                // Float: int.int
                if matches!(self.peek_kind(), Some(Token::Ctrl('.')))
                    && matches!(self.peek2_kind(), Some(Token::Int(_))) {
                    self.advance(); // eat '.'
                    let frac = self.advance().unwrap();
                    let Token::Int(f) = frac.value else { unreachable!() };
                    let v = format!("{n}.{f}").parse::<f64>().unwrap_or(0.0);
                    return Some(Expr::parsed(ExprKind::Float(v), start.to(frac.span)));
                }
                Some(Expr::parsed(ExprKind::Int(n), tok.span))
            }
            Token::Bool(_) => {
                let tok = self.advance().unwrap();
                let Token::Bool(b) = tok.value else { unreachable!() };
                Some(Expr::parsed(ExprKind::Bool(b), tok.span))
            }
            Token::Char(_) => {
                let tok = self.advance().unwrap();
                let Token::Char(c) = tok.value else { unreachable!() };
                Some(Expr::parsed(ExprKind::Char(c), tok.span))
            }
            Token::String(_) => {
                let tok = self.advance().unwrap();
                let Token::String(s) = tok.value else { unreachable!() };
                Some(Expr::parsed(ExprKind::String(s), tok.span))
            }
            Token::FmtString(_) => {
                let tok = self.advance().unwrap();
                let Token::FmtString(parts) = tok.value else { unreachable!() };
                let fmt_parts = self.convert_fmt_string(parts);
                Some(Expr::parsed(ExprKind::FmtString(fmt_parts), tok.span))
            }
            Token::InlineGo(_) => {
                let tok = self.advance().unwrap();
                let Token::InlineGo(s) = tok.value else { unreachable!() };
                Some(Expr::parsed(ExprKind::InlineGo(s), tok.span))
            }
            // .name — tag literal
            Token::Ctrl('.') => {
                self.advance();
                let name = self.expect_ident();
                Some(Expr::parsed(ExprKind::Tag(name.value.clone()), start.to(name.span)))
            }
            Token::Return => {
                self.advance();
                let value = if matches!(self.peek_kind(), Some(Token::Ctrl(';') | Token::Ctrl('}'))) || self.at_end() {
                    None
                } else {
                    self.parse_expr().map(Box::new)
                };
                let span = value.as_ref().map(|e| start.to(e.span)).unwrap_or(start);
                Some(Expr::parsed(ExprKind::Return(value), span))
            }
            Token::Break    => { self.advance(); Some(Expr::parsed(ExprKind::Break,    start)) }
            Token::Continue => { self.advance(); Some(Expr::parsed(ExprKind::Continue, start)) }
            Token::Ctrl('{') => self.parse_block_or_duck_lit(),
            Token::Ctrl('[') => {
                self.advance();
                let mut elems = Vec::new();
                loop {
                    if matches!(self.peek_kind(), Some(Token::Ctrl(']'))) || self.at_end() { break; }
                    if let Some(e) = self.parse_expr() { elems.push(e); } else { break; }
                    if self.eat_ctrl(',').is_none() { break; }
                }
                self.expect_ctrl(']');
                Some(Expr::parsed(ExprKind::Array(elems), start.to(self.prev_span())))
            }
            Token::Ctrl('(') => self.parse_paren_or_tuple(start),
            Token::If       => self.parse_if_expr(),
            Token::While    => self.parse_while_expr(),
            Token::For      => self.parse_for_expr(),
            Token::Match    => self.parse_match_expr(),
            Token::Fn | Token::Mut => self.parse_lambda_expr(),
            Token::Ident(_) | Token::ScopeRes => self.parse_ident_or_struct_lit(),
            _ => {
                self.error(format!("unexpected token in expression: {:?}", self.peek_kind()));
                None
            }
        }
    }

    fn parse_block_or_duck_lit(&mut self) -> Option<Expr<Parsed>> {
        // Duck lit if { ident : ... } (non-empty, starts with ident:)
        let is_duck = matches!(self.peek_at(1), Some(Token::Ident(_)))
            && matches!(self.peek_at(2), Some(Token::Ctrl(':')));
        if is_duck {
            let start = self.current_span();
            self.advance(); // eat '{'
            let mut fields = Vec::new();
            loop {
                if matches!(self.peek_kind(), Some(Token::Ctrl('}'))) || self.at_end() { break; }
                let name = self.expect_ident();
                self.expect_ctrl(':');
                let val = self.parse_expr()?;
                fields.push((name, val));
                if self.eat_ctrl(',').is_none() { break; }
            }
            self.expect_ctrl('}');
            Some(Expr::parsed(ExprKind::DuckLit(fields), start.to(self.prev_span())))
        } else {
            self.parse_block()
        }
    }

    fn parse_block(&mut self) -> Option<Expr<Parsed>> {
        let start = self.current_span();
        self.expect_ctrl('{');
        let mut stmts: Vec<Expr<Parsed>> = Vec::new();
        loop {
            if matches!(self.peek_kind(), Some(Token::Ctrl('}'))) || self.at_end() { break; }
            let expr = self.parse_expr()?;
            stmts.push(expr);
            let has_semi = self.eat_ctrl(';').is_some();
            if matches!(self.peek_kind(), Some(Token::Ctrl('}'))) {
                if has_semi {
                    // trailing `;` → block value is unit
                    let unit_span = self.current_span();
                    stmts.push(Expr::parsed(ExprKind::Tuple(vec![]), unit_span));
                }
                break;
            }
            if !has_semi {
                self.error("expected ';' between statements in block");
            }
        }
        self.expect_ctrl('}');
        Some(Expr::parsed(ExprKind::Block(stmts), start.to(self.prev_span())))
    }

    fn parse_if_expr(&mut self) -> Option<Expr<Parsed>> {
        let start = self.current_span();
        self.advance(); // eat 'if'
        self.expect_ctrl('(');
        let condition = self.parse_expr()?;
        self.expect_ctrl(')');
        let then_branch = self.parse_block()?;
        let else_branch = if self.eat_kw(&Token::Else).is_some() {
            if matches!(self.peek_kind(), Some(Token::If)) {
                Some(Box::new(self.parse_if_expr()?))
            } else {
                Some(Box::new(self.parse_block()?))
            }
        } else { None };
        let end = else_branch.as_ref().map(|e| e.span).unwrap_or(then_branch.span);
        Some(Expr::parsed(ExprKind::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch,
        }, start.to(end)))
    }

    fn parse_while_expr(&mut self) -> Option<Expr<Parsed>> {
        let start = self.current_span();
        self.advance(); // eat 'while'
        self.expect_ctrl('(');
        let condition = self.parse_expr()?;
        self.expect_ctrl(')');
        let body = self.parse_block()?;
        let body_span = body.span;
        Some(Expr::parsed(ExprKind::While {
            condition: Box::new(condition), body: Box::new(body),
        }, start.to(body_span)))
    }

    fn parse_for_expr(&mut self) -> Option<Expr<Parsed>> {
        let start = self.current_span();
        self.advance(); // eat 'for'
        self.expect_ctrl('(');
        let is_mut = self.eat_kw(&Token::Mut).is_some();
        let binding = self.expect_ident();
        self.eat_kw(&Token::In);
        let iterable = self.parse_expr()?;
        self.expect_ctrl(')');
        let body = self.parse_block()?;
        let body_span = body.span;
        Some(Expr::parsed(ExprKind::For {
            binding, is_mut, iterable: Box::new(iterable), body: Box::new(body),
        }, start.to(body_span)))
    }

    fn parse_match_expr(&mut self) -> Option<Expr<Parsed>> {
        let start = self.current_span();
        self.advance(); // eat 'match'
        let value = self.parse_expr()?;
        self.expect_ctrl('{');
        let mut arms = Vec::new();
        let mut else_arm: Option<Box<Expr<Parsed>>> = None;
        loop {
            if matches!(self.peek_kind(), Some(Token::Ctrl('}'))) || self.at_end() { break; }
            if matches!(self.peek_kind(), Some(Token::Else)) {
                self.advance();
                // Consume optional @ binding and if guard on else arm (binding unused at this level).
                if self.eat_ctrl('@').is_some() { self.expect_ident(); }
                if self.eat_kw(&Token::If).is_some() { self.parse_expr()?; }
                self.eat_kw(&Token::ThickArrow);
                let body = self.parse_expr()?;
                self.eat_ctrl(',');
                else_arm = Some(Box::new(body));
                break;
            }
            let arm_start = self.current_span();
            let pattern = self.parse_type_expr()?;
            let base = if matches!(self.peek_kind(), Some(Token::Ident(_)))
                && matches!(self.peek2_kind(), Some(Token::Ctrl('@') | Token::If | Token::ThickArrow)) {
                // 'base' keyword followed by binding or guard or arrow
                // Old parser: TypeExpr base? @ binding if guard => body
                // 'base' is actually another TypeExpr for the narrowing base
                None // skip for now; base is rarely used
            } else { None };
            let binding = if self.eat_ctrl('@').is_some() { Some(self.expect_ident()) } else { None };
            let guard = if self.eat_kw(&Token::If).is_some() {
                Some(Box::new(self.parse_expr()?))
            } else { None };
            self.eat_kw(&Token::ThickArrow);
            let body = self.parse_expr()?;
            let arm_span = arm_start.to(body.span);
            self.eat_ctrl(',');
            arms.push(MatchArm { pattern, base, binding, guard, body, span: arm_span });
        }
        self.expect_ctrl('}');
        Some(Expr::parsed(ExprKind::Match {
            value: Box::new(value), arms, else_arm,
        }, start.to(self.prev_span())))
    }

    fn parse_paren_or_tuple(&mut self, start: Span) -> Option<Expr<Parsed>> {
        self.advance(); // eat '('
        if self.eat_ctrl(')').is_some() {
            return Some(Expr::parsed(ExprKind::Tuple(vec![]), start.to(self.prev_span())));
        }
        let first = self.parse_expr()?;
        if self.eat_ctrl(')').is_some() {
            return Some(first); // parenthesized expression
        }
        self.expect_ctrl(',');
        let mut elems = vec![first];
        loop {
            if matches!(self.peek_kind(), Some(Token::Ctrl(')'))) || self.at_end() { break; }
            if let Some(e) = self.parse_expr() { elems.push(e); } else { break; }
            if self.eat_ctrl(',').is_none() { break; }
        }
        self.expect_ctrl(')');
        Some(Expr::parsed(ExprKind::Tuple(elems), start.to(self.prev_span())))
    }

    fn parse_lambda_expr(&mut self) -> Option<Expr<Parsed>> {
        let start = self.current_span();
        let is_mut = self.eat_kw(&Token::Mut).is_some();
        self.advance(); // eat 'fn'
        self.expect_ctrl('(');
        let mut params = Vec::new();
        loop {
            if matches!(self.peek_kind(), Some(Token::Ctrl(')'))) || self.at_end() { break; }
            let name = self.expect_ident();
            let type_expr = if self.eat_ctrl(':').is_some() {
                self.parse_type_expr()
                    .unwrap_or_else(|| TypeExpr::new(TypeDescription::Any, name.span))
            } else {
                TypeExpr::new(TypeDescription::Any, name.span)
            };
            params.push(Param { name, type_expr, is_mut: false });
            if self.eat_ctrl(',').is_none() { break; }
        }
        self.expect_ctrl(')');
        let return_type = if self.eat_kw(&Token::ThinArrow).is_some() {
            self.parse_type_expr()
        } else { None };
        let body = self.parse_block()?;
        let body_span = body.span;
        Some(Expr::parsed(ExprKind::Lambda {
            is_mut, params, return_type, body: Box::new(body),
        }, start.to(body_span)))
    }

    fn parse_ident_or_struct_lit(&mut self) -> Option<Expr<Parsed>> {
        let start = self.current_span();
        let is_global = self.eat_kw(&Token::ScopeRes).is_some();
        let first = if is_global { self.expect_ident() } else { self.eat_ident()? };
        let mut segments = vec![first];
        // Collect path: ident :: ident :: ...
        while matches!(self.peek_kind(), Some(Token::ScopeRes))
            && matches!(self.peek2_kind(), Some(Token::Ident(_))) {
            self.advance(); // eat '::'
            segments.push(self.eat_ident().unwrap());
        }

        // Try struct literal: name<T>? { field: val, ... }
        let saved = self.pos;
        let type_params = self.try_parse_type_angle_params();
        let is_struct = matches!(self.peek_kind(), Some(Token::Ctrl('{')))
            && matches!(self.peek_at(1), Some(Token::Ident(_)))
            && matches!(self.peek_at(2), Some(Token::Ctrl(':')));
        if !is_struct {
            // Restore type params tokens if no struct follows
            if !type_params.is_empty() { self.pos = saved; }
            let span = start.to(segments.last().unwrap().span);
            return Some(Expr::parsed(ExprKind::Ident(UnresolvedIdent { segments, is_global }), span));
        }

        // Struct literal
        self.advance(); // eat '{'
        let mut fields = Vec::new();
        loop {
            if matches!(self.peek_kind(), Some(Token::Ctrl('}'))) || self.at_end() { break; }
            let fname = self.expect_ident();
            self.expect_ctrl(':');
            let fval = self.parse_expr()?;
            fields.push((fname, fval));
            if self.eat_ctrl(',').is_none() { break; }
        }
        self.expect_ctrl('}');
        let name = UnresolvedIdent { segments, is_global };
        Some(Expr::parsed(ExprKind::StructLit { name, type_params, fields }, start.to(self.prev_span())))
    }

    fn convert_fmt_string(&mut self, parts: Vec<FmtStringPart>) -> Vec<FmtPart<Parsed>> {
        parts.into_iter().map(|p| match p {
            FmtStringPart::Literal(s) => FmtPart::Literal(s),
            FmtStringPart::Tokens(toks) => {
                // Strip surrounding { and } delimiters, then parse as expression.
                let inner: Vec<WithSpan<Token>> = toks.into_iter()
                    .skip(1)
                    .rev().skip(1).rev()
                    .collect();
                let mut sub = Parser::new(inner, self.file_id);
                let expr = sub.parse_expr()
                    .unwrap_or_else(|| Expr::parsed(ExprKind::Tuple(vec![]), Span::dummy()));
                self.errors.extend(sub.errors);
                FmtPart::Expr(expr)
            }
        }).collect()
    }
}

