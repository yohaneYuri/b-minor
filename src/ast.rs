use std::fmt::Display;

#[derive(Debug)]
pub struct Program(pub Vec<Decl>);

#[derive(Debug)]
pub enum Decl {
    Var {
        id: String,
        ty: Ty,
        init: Option<Init>,
    },
    ExternFunc {
        id: String,
        sig: FuncSig,
    },
    Func {
        id: String,
        sig: FuncSig,
        body: Block,
    },
}

#[derive(Debug)]
pub enum Ty {
    Int,
    Bool,
    Char,
    Str,
    Arr { arr_size: Option<i32>, ty: Box<Ty> },
    Void,
}

#[derive(Debug)]
pub enum Init {
    Arr(Vec<Expr>),
    Expr(Expr),
}

#[derive(Debug)]
pub struct FuncSig {
    pub ret_ty: Ty,
    pub params: Vec<Param>,
}

#[derive(Debug)]
pub struct Param {
    pub id: String,
    pub ty: Ty,
}

#[derive(Debug)]
pub struct Block(pub Vec<Stmt>);

#[derive(Debug)]
pub enum Expr {
    Group(Box<Expr>),
    Id(String),
    Int(i32),
    Bool(Bool),
    Str(String),
    Char(char),
    ArrSubscript {
        id: String,
        index: Box<Expr>,
    },
    FuncCall {
        id: String,
        args: Vec<Expr>,
    },
    Unary {
        expr: Box<Expr>,
        op: UnaryOp,
    },
    Binary {
        le: Box<Expr>,
        op: BinaryOp,
        re: Box<Expr>,
    },
    Assign {
        lv: String,
        expr: Box<Expr>,
    },
}

#[derive(Debug)]
pub enum UnaryOp {
    Incre,
    Decre,
    Not,
    Neg,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use UnaryOp::*;
        write!(
            f,
            "{}",
            match self {
                Incre => "++",
                Decre => "--",
                Not => "!",
                Neg => "-",
            }
        )
    }
}

#[derive(Debug)]
pub enum BinaryOp {
    Exp,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    NotEq,
    And,
    Or,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BinaryOp::*;
        write!(
            f,
            "{}",
            match self {
                Exp => "^",
                Mul => "*",
                Div => "/",
                Mod => "%",
                Add => "+",
                Sub => "-",
                Gt => ">",
                Ge => ">=",
                Lt => "<",
                Le => "<=",
                Eq => "==",
                NotEq => "!=",
                And => "&&",
                Or => "||",
            }
        )
    }
}

#[derive(Debug)]
pub enum Stmt {
    Decl(Decl),
    Expr(Expr),
    Ret(Expr),
    If {
        cond: Expr,
        t_branch: Block,
        f_branch: Option<Block>,
    },
    For {
        init: Option<Expr>,
        cond: Option<Expr>,
        update: Option<Expr>,
        body: Block,
    },
    Block(Block),
}

#[derive(Debug)]
pub enum Bool {
    True,
    False,
}
