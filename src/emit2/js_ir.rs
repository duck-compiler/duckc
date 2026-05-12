/// JavaScript IR - the target representation for Duck->JS compilation.
/// Types are erased; this models the runtime JavaScript semantics.
#[derive(Debug, Clone)]
pub struct JsFile {
    pub decls: Vec<JsDecl>,
    /// TypeScript packages declared with `use ts "pkg"`: (pkg_name, binding_name).
    pub ts_packages: Vec<(String, String)>,
    /// Names of all top-level `client fn` functions.
    pub client_fn_names: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum JsDecl {
    Function {
        name: String,
        params: Vec<String>,
        is_async: bool,
        body: Vec<JsStmt>,
    },
    Const {
        name: String,
        value: JsExpr,
    },
    Class {
        name: String,
        methods: Vec<JsMethod>,
    },
    Export(Box<JsDecl>),
}

#[derive(Debug, Clone)]
pub struct JsMethod {
    pub name: String,
    pub params: Vec<String>,
    pub is_static: bool,
    pub is_async: bool,
    pub body: Vec<JsStmt>,
}

#[derive(Debug, Clone)]
pub enum JsStmt {
    Const {
        name: String,
        value: JsExpr,
    },
    Let {
        name: String,
        value: Option<JsExpr>,
    },
    /// `const [a, b] = expr` - destructuring assignment
    MultiConst {
        names: Vec<String>,
        value: JsExpr,
    },
    Assign {
        target: JsExpr,
        value: JsExpr,
    },
    Return(Option<JsExpr>),
    If {
        cond: JsExpr,
        then: Vec<JsStmt>,
        else_: Option<Vec<JsStmt>>,
    },
    ForOf {
        val: String,
        iter: JsExpr,
        body: Vec<JsStmt>,
    },
    While {
        cond: JsExpr,
        body: Vec<JsStmt>,
    },
    Loop(Vec<JsStmt>),
    Break,
    Continue,
    Expr(JsExpr),
    Block(Vec<JsStmt>),
    Throw(JsExpr),
}

#[derive(Debug, Clone)]
pub enum JsExpr {
    Ident(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Null,
    Undefined,
    /// Verbatim JavaScript source (JSX, inline JS, etc.)
    Raw(String),
    BinOp {
        op: JsBinOp,
        lhs: Box<JsExpr>,
        rhs: Box<JsExpr>,
    },
    UnaryOp {
        op: JsUnaryOp,
        operand: Box<JsExpr>,
    },
    Call {
        callee: Box<JsExpr>,
        args: Vec<JsExpr>,
    },
    New {
        callee: Box<JsExpr>,
        args: Vec<JsExpr>,
    },
    Field {
        base: Box<JsExpr>,
        field: String,
    },
    OptionalField {
        base: Box<JsExpr>,
        field: String,
    },
    Index {
        base: Box<JsExpr>,
        index: Box<JsExpr>,
    },
    /// `{ field: expr, ... }` - object literal
    Object {
        fields: Vec<(String, JsExpr)>,
    },
    /// `[elem, ...]` - array literal
    Array {
        elems: Vec<JsExpr>,
    },
    Arrow {
        params: Vec<String>,
        body: JsArrowBody,
        is_async: bool,
    },
    /// Template literal: `` `${expr}text` ``
    Template {
        parts: Vec<JsTemplatePart>,
    },
    Instanceof {
        value: Box<JsExpr>,
        class: String,
    },
    Spread(Box<JsExpr>),
    Await(Box<JsExpr>),
    Typeof(Box<JsExpr>),
    Ternary {
        cond: Box<JsExpr>,
        then: Box<JsExpr>,
        else_: Box<JsExpr>,
    },
}

#[derive(Debug, Clone)]
pub enum JsArrowBody {
    Expr(Box<JsExpr>),
    Block(Vec<JsStmt>),
}

#[derive(Debug, Clone)]
pub enum JsTemplatePart {
    Str(String),
    Expr(JsExpr),
}

#[derive(Debug, Clone)]
pub enum JsBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Eq,
    Neq,
    StrictEq,
    StrictNeq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
    NullCoalesce,
}

#[derive(Debug, Clone)]
pub enum JsUnaryOp {
    Neg,
    Not,
    BitNot,
}
