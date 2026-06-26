#[derive(Debug)]
pub struct GostRoot<'src> {
    pub body: Vec<GoStatement<'src>>,
}

#[derive(Debug)]
pub enum GoExpression<'src> {
    String(&'src str),
    Int(&'src i64),
    Int8(&'src i8),
    Int32(&'src i32),
    Int64(&'src i64),
    Uint(&'src u64),
    Uint8(&'src u8),
    Uint32(&'src u32),
    Uint64(&'src u64),
    Float32(&'src f32),
    Float64(&'src f64),
    FuncCall {
        name: &'src str,
        args: Vec<GoExpression<'src>>,
    },
    Immediate(&'src str)
}

#[derive(Debug)]
pub struct StructField<'src> {
    pub name: &'src str,
    pub type_: GoType<'src>,
    pub tag: Option<&'src str>,
}

#[derive(Debug)]
pub enum GoType<'src> {
    Int,
    Int8,
    Int32,
    Int64,
    Uint,
    Uint8,
    Uint32,
    Uint64,
    Float32,
    Float64,
    String,
    Bool,
    Array(Box<GoType<'src>>),
    Struct {
        fields: Vec<StructField<'src>>,
    },
    TypeName(&'src str)
}

#[derive(Debug)]
pub enum GoStatement<'src> {
    GoImport {
        alias: Option<&'src str>, // _ is a side effect import; . is a dot import, allows package access without package name
        path: &'src str,
    },
    FuncDef {
        name: &'src str,
        params: Vec<(&'src str, GoType<'src>)>,
        return_type: Option<GoType<'src>>,
        body: Vec<GoStatement<'src>>,
    },
    VarDecl {
        name: &'src str,
        type_: Option<GoType<'src>>,
        init_expression: Option<GoExpression<'src>>,
    },
    Assign {
        target: &'src str,
        expr: GoExpression<'src>,
    },
    Expr {
        expr: GoExpression<'src>,
    },
}
