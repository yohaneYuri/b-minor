use crate::{
    ast::{BinaryOp, Block, Decl, Expr, Init, Stmt, UnaryOp},
    semantic::{symbol_table::SymbolTable, types::Ty},
};

pub trait TypeCheck {
    fn check_type(&self, env: &mut SymbolTable, errs: &mut Vec<String>) -> Ty;
}

impl TypeCheck for String {
    fn check_type(&self, env: &mut SymbolTable, errs: &mut Vec<String>) -> Ty {
        if let Some(ty) = env.lookup(&self) {
            ty.clone()
        } else {
            errs.push(format!("undefined identifier: {self}"));
            Ty::Unknown
        }
    }
}

impl TypeCheck for Expr {
    fn check_type(&self, env: &mut SymbolTable, errs: &mut Vec<String>) -> Ty {
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
    fn check_type(&self, env: &mut SymbolTable, errs: &mut Vec<String>) -> Ty {
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
                // inject parameters into the environment
                sig.params
                    .iter()
                    .for_each(|p| env.bind(&p.id, (&p.ty).into()));

                // conditional control flow analysis...
                let mut ret_guaranteed = false;
                let mut body_ret_tys = Vec::new();
                for stmt in body.0.iter() {
                    let ty = stmt.check_type(env, errs);
                    if matches!(stmt, Stmt::Ret(_)) {
                        ret_guaranteed = true;
                    }
                    body_ret_tys.push(ty);
                }

                let ret_ty = (&sig.ret_ty).into();
                if let Some(ty) = body_ret_tys
                    .iter()
                    .find(|&ty| ty != &Ty::Void && ty != &Ty::Unknown && ty != &ret_ty)
                {
                    errs.push(format!(
                        "mismatched return type in funciton {id}, expected {ret_ty}, found {ty}"
                    ));
                }
                if !ret_guaranteed && ret_ty != Ty::Void {
                    errs.push(format!(
                        "function {id} doesn't return in all possible pathes"
                    ));
                }
                env.leave();
                env.bind(id, sig.into());
            }
            _ => {}
        }
        Ty::Void
    }
}

impl TypeCheck for Init {
    fn check_type(&self, env: &mut SymbolTable, errs: &mut Vec<String>) -> Ty {
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
    fn check_type(&self, env: &mut SymbolTable, errs: &mut Vec<String>) -> Ty {
        env.enter();
        let mut ret_ty = Ty::Void;
        for stmt in self.0.iter() {
            ret_ty = stmt.check_type(env, errs);
        }
        env.leave();
        ret_ty
    }
}

impl TypeCheck for Stmt {
    fn check_type(&self, env: &mut SymbolTable, errs: &mut Vec<String>) -> Ty {
        use Stmt::*;
        match self {
            Decl(decl) => decl.check_type(env, errs),
            Expr(expr) => expr.check_type(env, errs),
            Ret(expr) => expr.check_type(env, errs),
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
                if let Some(f_branch) = f_branch {
                    f_branch.check_type(env, errs)
                } else {
                    Ty::Void
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

                body.check_type(env, errs)
            }
            Block(block) => block.check_type(env, errs),
        }
    }
}
