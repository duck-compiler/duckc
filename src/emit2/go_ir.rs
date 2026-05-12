#[derive(Debug, Clone)]
pub struct GoFile {
    pub package: String,
    pub imports: Vec<String>,
    pub decls: Vec<GoDecl>,
}

#[derive(Debug, Clone)]
pub enum GoDecl {
    Func {
        name: String,
        receiver: Option<GoParam>,
        params: Vec<GoParam>,
        ret: Option<GoType>,
        body: Vec<GoStmt>,
    },
    Struct {
        name: String,
        fields: Vec<GoField>,
    },
    TypeAlias {
        name: String,
        ty: GoType,
    },
    Var {
        name: String,
        ty: GoType,
        value: GoExpr,
    },
    /// `type HasFieldName[T any] interface { GetFieldName() T; ... }`
    Interface {
        name: String,
        methods: Vec<GoInterfaceMethod>,
    },
    /// Verbatim Go source - emitted as-is, used for generated island helpers.
    Raw(String),
}

/// A method signature inside a `[T any]` interface declaration.
/// Uses `GoType::Named("T")` / `GoType::Ptr(Named("T"))` to represent the type parameter.
#[derive(Debug, Clone)]
pub struct GoInterfaceMethod {
    pub name: String,
    pub params: Vec<GoParam>,
    pub ret: Option<GoType>,
}

#[derive(Debug, Clone)]
pub struct GoParam {
    pub name: String,
    pub ty: GoType,
}

#[derive(Debug, Clone)]
pub struct GoField {
    pub name: String,
    pub ty: GoType,
}

#[derive(Debug, Clone)]
pub enum GoStmt {
    Declare {
        name: String,
        ty: GoType,
    },
    DeclareAssign {
        name: String,
        value: GoExpr,
    },
    /// `a, b := expr` - multi-return / tuple destructure
    MultiDeclareAssign {
        names: Vec<String>,
        value: GoExpr,
    },
    Assign {
        target: GoExpr,
        value: GoExpr,
    },
    Return(Option<GoExpr>),
    If {
        cond: GoExpr,
        then: Vec<GoStmt>,
        else_: Option<Vec<GoStmt>>,
    },
    ForCond {
        cond: GoExpr,
        body: Vec<GoStmt>,
    },
    ForRange {
        val: String,
        iter: GoExpr,
        body: Vec<GoStmt>,
    },
    Loop(Vec<GoStmt>),
    Break,
    Continue,
    Defer(GoExpr),
    Expr(GoExpr),
    Block(Vec<GoStmt>),
    TypeSwitch {
        value: GoExpr,
        arms: Vec<TypeSwitchArm>,
        default: Option<Vec<GoStmt>>,
    },
}

#[derive(Debug, Clone)]
pub struct TypeSwitchArm {
    pub binding: String,
    pub ty: GoType,
    pub body: Vec<GoStmt>,
}

#[derive(Debug, Clone)]
pub enum GoExpr {
    Ident(String),
    Int(i64),
    UInt(u64),
    Float(f64),
    Bool(bool),
    Char(char),
    Str(String),
    Nil,
    Raw(String),
    BinOp {
        op: GoBinOp,
        lhs: Box<GoExpr>,
        rhs: Box<GoExpr>,
    },
    UnaryOp {
        op: GoUnaryOp,
        operand: Box<GoExpr>,
    },
    Call {
        callee: Box<GoExpr>,
        args: Vec<GoExpr>,
    },
    Field {
        base: Box<GoExpr>,
        field: String,
    },
    Index {
        base: Box<GoExpr>,
        index: Box<GoExpr>,
    },
    StructLit {
        ty: String,
        fields: Vec<(String, GoExpr)>,
    },
    SliceLit {
        ty: GoType,
        elems: Vec<GoExpr>,
    },
    Closure {
        params: Vec<GoParam>,
        ret: Option<GoType>,
        body: Vec<GoStmt>,
    },
    Cast {
        ty: GoType,
        value: Box<GoExpr>,
    },
    Ref(Box<GoExpr>),
    Deref(Box<GoExpr>),
    TypeAssert {
        value: Box<GoExpr>,
        ty: GoType,
    },
}

#[derive(Debug, Clone)]
pub enum GoBinOp {
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
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
}

#[derive(Debug, Clone)]
pub enum GoUnaryOp {
    Neg,
    Not,
    BitNot,
}

#[derive(Debug, Clone)]
pub enum GoType {
    Int64,
    Uint64,
    Float64,
    Bool,
    String,
    Rune,
    Byte,
    Slice(Box<GoType>),
    Func {
        params: Vec<GoType>,
        ret: Option<Box<GoType>>,
    },
    Ptr(Box<GoType>),
    Named(String),
    Any,
    /// Inline duck usage: `interface { HasName[string]; HasAge[int64] }`
    /// Vec of (HasFieldName, concrete_go_type). Fields stored sorted alphabetically.
    DuckInterface(Vec<(String, GoType)>),
}

impl GoType {
    pub fn zero_expr(&self) -> GoExpr {
        match self {
            GoType::Int64 | GoType::Uint64 => GoExpr::Int(0),
            GoType::Float64 => GoExpr::Float(0.0),
            GoType::Bool => GoExpr::Bool(false),
            GoType::String => GoExpr::Str(String::new()),
            GoType::Rune => GoExpr::Char('\0'),
            GoType::Byte => GoExpr::Int(0),
            _ => GoExpr::Nil,
        }
    }
}
