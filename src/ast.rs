use std::{fmt::Display, ops::Deref};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct NodeId(pub u32);

pub struct NodeIdGenerator(u32);

impl NodeIdGenerator {
    pub fn new() -> Self {
        Self(0)
    }

    pub fn get_id(&mut self) -> NodeId {
        let id = self.0;
        self.0 += 1;
        NodeId(id)
    }
}

#[derive(Debug)]
pub struct SpannedNode<T> {
    pub id: NodeId,
    pub node: T,
    pub start: usize,
    pub end: usize,
}

impl<T> SpannedNode<T> {
    pub fn new(id_gen: &mut NodeIdGenerator, node: T, start: usize, end: usize) -> Self {
        Self { id: id_gen.get_id(), node, start, end }
    }
}

#[derive(Debug)]
pub struct Program(pub Vec<SpannedNode<Decl>>);

#[derive(Debug)]
pub struct Ident(pub String);

#[derive(Debug)]
pub enum Decl {
    Var {
        ident: SpannedNode<Ident>,
        ty: SpannedNode<Ty>,
        init: Option<SpannedNode<Init>>,
    },
    ExternFunc {
        ident: SpannedNode<Ident>,
        sig: SpannedNode<FuncSig>,
    },
    Func {
        ident: SpannedNode<Ident>,
        sig: SpannedNode<FuncSig>,
        body: SpannedNode<Block>,
    },
}

#[derive(Debug)]
pub enum Ty {
    Int,
    Bool,
    Char,
    Str,
    Arr { arr_size: Option<i32>, ty: Box<SpannedNode<Ty>> },
    Void,
}

#[derive(Debug)]
pub enum Init {
    Arr(Vec<SpannedNode<Expr>>),
    Expr(SpannedNode<Expr>),
}

#[derive(Debug)]
pub struct FuncSig {
    pub ret_ty: SpannedNode<Ty>,
    pub params: Vec<SpannedNode<Param>>,
}

#[derive(Debug)]
pub struct Param {
    pub ident: SpannedNode<Ident>,
    pub ty: SpannedNode<Ty>,
}

#[derive(Debug)]
pub struct Block(pub Vec<SpannedNode<Stmt>>);

#[derive(Debug)]
pub enum Expr {
    Group(Box<SpannedNode<Expr>>),
    Id(SpannedNode<Ident>),
    Int(i32),
    Bool(Bool),
    Str(String),
    Char(char),
    ArrSubscript {
        ident: SpannedNode<Ident>,
        index: Box<SpannedNode<Expr>>,
    },
    FuncCall {
        ident: SpannedNode<Ident>,
        args: Vec<SpannedNode<Expr>>,
    },
    Unary {
        expr: Box<SpannedNode<Expr>>,
        op: UnaryOp,
    },
    Binary {
        le: Box<SpannedNode<Expr>>,
        op: BinaryOp,
        re: Box<SpannedNode<Expr>>,
    },
    Assign {
        lv: SpannedNode<Ident>,
        expr: Box<SpannedNode<Expr>>,
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
    Decl(SpannedNode<Decl>),
    Expr(SpannedNode<Expr>),
    Ret(SpannedNode<Expr>),
    If {
        cond: SpannedNode<Expr>,
        t_branch: SpannedNode<Block>,
        f_branch: Option<SpannedNode<Block>>,
    },
    For {
        init: Option<SpannedNode<Expr>>,
        cond: Option<SpannedNode<Expr>>,
        update: Option<SpannedNode<Expr>>,
        body: SpannedNode<Block>,
    },
    Block(SpannedNode<Block>),
}

#[derive(Debug)]
pub enum Bool {
    True,
    False,
}

impl<T> Deref for SpannedNode<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}