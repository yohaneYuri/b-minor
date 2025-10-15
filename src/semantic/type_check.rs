use crate::{
    ast::{BinaryOp, Block, Decl, Expr, Init, Stmt, UnaryOp},
    semantic::{
        environment::{Environement, FunctionContext},
        types::Ty,
    },
};

pub trait TypeCheck {
    fn check_type(&self, env: &mut Environement, errs: &mut Vec<String>) -> Ty;
}

impl TypeCheck for String {
    fn check_type(&self, env: &mut Environement, errs: &mut Vec<String>) -> Ty {
        if let Some(ty) = env.lookup(&self) {
            ty.clone()
        } else {
            errs.push(format!("undefined identifier: {self}"));
            Ty::Unknown
        }
    }
}

impl TypeCheck for Expr {
    fn check_type(&self, env: &mut Environement, errs: &mut Vec<String>) -> Ty {
        use Expr::*;
        match self {
            Group(expr) => expr.check_type(env, errs),
            Id(id) => id.check_type(env, errs),
            Int(_) => Ty::Int,
            Bool(_) => Ty::Bool,
            Str(_) => Ty::Str,
            Char(_) => Ty::Char,
            ArrSubscript { id, index } => {
                let index_ty = index.check_type(env, errs);
                if index_ty != Ty::Unknown && index_ty != Ty::Int {
                    errs.push(format!("array index must be integer, found {index_ty}"));
                }
                id.check_type(env, errs)
            }
            FuncCall { id, .. } => {
                let ty = id.check_type(env, errs);
                if let Ty::Func { ret_ty, .. } = ty {
                    *ret_ty
                } else {
                    errs.push(format!("{id} is not a function"));
                    Ty::Unknown
                }
            }
            Unary { expr, op } => {
                let expr_ty = expr.check_type(env, errs);
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
                let l_ty = le.check_type(env, errs);
                let r_ty = re.check_type(env, errs);
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
                let lv_ty = lv.check_type(env, errs);
                if lv_ty != Ty::Unknown {
                    let expr_ty = expr.check_type(env, errs);
                    if lv_ty != expr_ty {
                        errs.push(format!("cannot assign {expr_ty} to {lv_ty}"));
                    }
                }
                Ty::Void
            }
        }
    }
}

impl TypeCheck for Decl {
    fn check_type(&self, env: &mut Environement, errs: &mut Vec<String>) -> Ty {
        use Decl::*;
        match self {
            Var { id, ty, init } => {
                if let Some(init) = init {
                    let anno_ty = ty.into();
                    if anno_ty == Ty::Void {
                        errs.push(format!("variable's type cannot be void"));
                    }
                    let init_ty = init.check_type(env, errs);
                    if init_ty != anno_ty && init_ty != Ty::Unknown {
                        errs.push(format!(
                            "mismatched initializer type, expected {anno_ty}, found {init_ty}"
                        ));
                    }
                    env.bind(id, ty.into());
                }
            }
            Func { id, sig, body } => {
                // treat function as special and do not call <Block as TypeCheck>::check_type
                env.enter();
                let ret_ty = (&sig.ret_ty).into();
                env.func_ctx = Some(FunctionContext { ret_ty });
                // inject parameters into the environment
                sig.params
                    .iter()
                    .for_each(|p| env.bind(&p.id, (&p.ty).into()));

                for stmt in body.0.iter() {
                    stmt.check_type(env, errs);
                }

                env.func_ctx = None;
                env.leave();
                env.bind(id, sig.into());
            }
            _ => {}
        }
        Ty::Void
    }
}

impl TypeCheck for Init {
    fn check_type(&self, env: &mut Environement, errs: &mut Vec<String>) -> Ty {
        use Init::*;
        match self {
            Arr(exprs) => {
                let types: Vec<_> = exprs.iter().map(|e| e.check_type(env, errs)).collect();
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
            Expr(expr) => expr.check_type(env, errs),
        }
    }
}

impl TypeCheck for Block {
    fn check_type(&self, env: &mut Environement, errs: &mut Vec<String>) -> Ty {
        env.enter();
        for stmt in self.0.iter() {
            stmt.check_type(env, errs);
        }
        env.leave();
        Ty::Void
    }
}

impl TypeCheck for Stmt {
    fn check_type(&self, env: &mut Environement, errs: &mut Vec<String>) -> Ty {
        use Stmt::*;
        match self {
            Decl(decl) => {
                decl.check_type(env, errs);
            }
            Expr(expr) => {
                expr.check_type(env, errs);
            }
            Ret(expr) => {
                let ty = expr.check_type(env, errs);
                let ret_ty = &env.func_ctx.as_ref().unwrap().ret_ty;
                if &ty != ret_ty {
                    errs.push(format!(
                        "return type doesn't match function return type, expect {ret_ty}, found {ty}"
                    ));
                }
            }
            If {
                cond,
                t_branch,
                f_branch,
            } => {
                let cond_ty = cond.check_type(env, errs);
                if cond_ty != Ty::Bool && cond_ty != Ty::Unknown {
                    errs.push(format!(
                        "condition of if statement must be boolean type, found {cond_ty}"
                    ));
                }

                let t_ty = t_branch.check_type(env, errs);
                if t_ty != Ty::Void {
                    return t_ty;
                }
                if let Some(block) = f_branch {
                    block.check_type(env, errs);
                }
            }
            For {
                init,
                cond,
                update,
                body,
            } => {
                if let Some(init) = init {
                    init.check_type(env, errs);
                }
                if let Some(cond) = cond {
                    let cond_ty = cond.check_type(env, errs);
                    if cond_ty != Ty::Bool && cond_ty != Ty::Unknown {
                        errs.push(format!(
                            "condition of for statement must be boolean type, found {cond_ty}"
                        ));
                    }
                }
                if let Some(update) = update {
                    update.check_type(env, errs);
                }

                body.check_type(env, errs);
            }
            Block(block) => {
                block.check_type(env, errs);
            }
        }
        Ty::Void
    }
}
