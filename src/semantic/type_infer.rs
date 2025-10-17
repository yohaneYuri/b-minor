use std::ops::Deref;

use crate::{
    ast::{BinaryOp, Expr, Ident, Init, SpannedNode, UnaryOp},
    semantic::{environment::Environement, types::Ty},
};

pub trait TypeInfer {
    fn infer_type(&self, env: &mut Environement, errs: &mut Vec<String>) -> Ty;
}

impl TypeInfer for SpannedNode<Ident> {
    fn infer_type(&self, env: &mut Environement, errs: &mut Vec<String>) -> Ty {
        if let Some(ty) = env.lookup(&self.0) {
            ty.clone()
        } else {
            errs.push(format!("undefined identifier: {}", self.0));
            Ty::Unknown
        }
    }
}

impl TypeInfer for SpannedNode<Expr> {
    fn infer_type(&self, env: &mut Environement, errs: &mut Vec<String>) -> Ty {
        use Expr::*;
        let ty = match self.deref() {
            Group(expr) => expr.infer_type(env, errs),
            Id(id) => id.infer_type(env, errs),
            Int(_) => Ty::Int,
            Bool(_) => Ty::Bool,
            Str(_) => Ty::Str,
            Char(_) => Ty::Char,
            ArrSubscript { ident, index } => {
                let index_ty = index.infer_type(env, errs);
                if index_ty != Ty::Unknown && index_ty != Ty::Int {
                    errs.push(format!("array index must be integer, found {index_ty}"));
                }
                ident.infer_type(env, errs)
            }
            FuncCall { ident, .. } => {
                let ty = ident.infer_type(env, errs);
                if let Ty::Func { ret_ty, .. } = ty {
                    *ret_ty
                } else {
                    errs.push(format!("{} is not a function", ident.0));
                    Ty::Unknown
                }
            }
            Unary { expr, op } => {
                let expr_ty = expr.infer_type(env, errs);
                let (expected_ty, err_msg) = match op {
                    UnaryOp::Incre | UnaryOp::Neg | UnaryOp::Decre => {
                        (Ty::Int, format!("target must be integer, found {expr_ty}"))
                    }
                    UnaryOp::Not => (
                        Ty::Bool,
                        format!("logical operation {op} must be applied boolean, found {expr_ty}"),
                    ),
                };
                if expr_ty != Ty::Unknown && expr_ty != expected_ty {
                    errs.push(err_msg);
                }
                expected_ty
            }
            Binary { le, op, re } => {
                use BinaryOp::*;
                let l_ty = le.infer_type(env, errs);
                let r_ty = re.infer_type(env, errs);
                let unified_ty = match (l_ty, r_ty) {
                    (l, Ty::Unknown) => l,
                    (Ty::Unknown, r) => r,
                    (l, r) if l == r => l,
                    _ => Ty::Unknown,
                };
                match op {
                    Exp | Mul | Div | Mod | Add | Sub => {
                        if unified_ty != Ty::Int || unified_ty == Ty::Unknown {
                            errs.push(format!(
                                "arithmetic operation {op} can only be applied to integers"
                            ));
                        }
                        Ty::Int
                    }
                    Gt | Ge | Lt | Le | Eq | NotEq => {
                        if unified_ty == Ty::Unknown {
                            errs.push(format!(
                                "comparison operation {op} can only be applied to the same types"
                            ));
                        } else if matches!(unified_ty, Ty::Void | Ty::Func { .. } | Ty::Arr { .. })
                        {
                            errs.push(format!(
                                "comparison operation {op} cannot be applied to {unified_ty}"
                            ));
                        }
                        Ty::Bool
                    }
                    And | Or => {
                        if unified_ty != Ty::Bool || unified_ty == Ty::Unknown {
                            errs.push(format!("logical operation {op} must be applied booleans"));
                        }
                        Ty::Bool
                    }
                }
            }
            Assign { lv, expr } => {
                let lv_ty = lv.infer_type(env, errs);
                if lv_ty != Ty::Unknown {
                    let expr_ty = expr.infer_type(env, errs);
                    if lv_ty != expr_ty {
                        errs.push(format!("cannot assign {expr_ty} to {lv_ty}"));
                    }
                }
                Ty::Void
            }
        };
        env.record_node_ty(self.id, ty.clone());
        ty
    }
}

impl TypeInfer for SpannedNode<Init> {
    fn infer_type(&self, env: &mut Environement, errs: &mut Vec<String>) -> Ty {
        use Init::*;
        let ty = match self.deref() {
            Arr(exprs) => {
                let types: Vec<_> = exprs.iter().map(|e| e.infer_type(env, errs)).collect();
                let pred = types
                    .iter()
                    .find(|&ty| ty != &Ty::Unknown)
                    .cloned()
                    .unwrap_or(Ty::Unknown);
                if !types.iter().all(|ty| ty == &pred) {
                    errs.push(format!("elements in an array must have the same type"));
                }
                Ty::Arr {
                    arr_size: Some(if pred != Ty::Unknown {
                        types.len() as i32
                    } else {
                        0
                    }),
                    ty: Box::new(pred),
                }
            }
            Expr(expr) => expr.infer_type(env, errs),
        };
        env.record_node_ty(self.id, ty.clone());
        ty
    }
}
