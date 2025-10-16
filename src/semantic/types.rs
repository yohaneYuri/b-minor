use std::{fmt::Display, ops::Deref};

use crate::ast;

#[derive(Debug, PartialEq, Clone)]
pub enum Ty {
    Int,
    Bool,
    Char,
    Str,
    Arr { arr_size: Option<i32>, ty: Box<Ty> },
    Func { params: Vec<Ty>, ret_ty: Box<Ty> },
    Void,
    Unknown,
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Ty::*;
        match self {
            Int => write!(f, "integer"),
            Bool => write!(f, "boolean"),
            Char => write!(f, "char"),
            Str => write!(f, "string"),
            Arr { arr_size, ty } => {
                let arr_size = if let Some(n) = arr_size {
                    format!("{n}")
                } else {
                    "unknown".to_string()
                };
                write!(f, "{}", format!("array [{arr_size}] {ty}"))
            }
            Func { .. } => write!(f, "function"),
            Void => write!(f, "void"),
            Unknown => write!(f, "unknown"),
        }
    }
}

impl From<&ast::Ty> for Ty {
    fn from(value: &ast::Ty) -> Self {
        match value {
            ast::Ty::Int => Self::Int,
            ast::Ty::Bool => Self::Bool,
            ast::Ty::Char => Self::Char,
            ast::Ty::Str => Self::Str,
            ast::Ty::Arr { arr_size, ty } => Self::Arr {
                arr_size: *arr_size,
                ty: Box::new((**ty).deref().into()),
            },
            ast::Ty::Void => Self::Void,
        }
    }
}

impl From<&ast::FuncSig> for Ty {
    fn from(value: &ast::FuncSig) -> Self {
        Self::Func {
            params: value.params.iter().map(|p| p.ty.deref().into()).collect(),
            ret_ty: Box::new(value.ret_ty.deref().into()),
        }
    }
}
