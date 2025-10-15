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
                if index_ty != Ty::Int {
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
            Unary { expr, op } => match op {
                UnaryOp::Incre | UnaryOp::Neg | UnaryOp::Decre => {
                    let expr_ty = expr.check_type(env, errs);
                    if expr_ty != Ty::Int {
                        errs.push(format!("target must be integer, found {expr_ty}"));
                    }
                    Ty::Int
                }
                UnaryOp::Not => {
                    let expr_ty = expr.check_type(env, errs);
                    if expr_ty != Ty::Bool {
                        errs.push(format!(
                            "logical operation {op} must be applied boolean, found {expr_ty}"
                        ));
                    }
                    Ty::Bool
                }
            },
            Binary { le, op, re } => {
                use BinaryOp::*;
                match op {
                    Exp | Mul | Div | Mod | Add | Sub => {
                        let l_ty = le.check_type(env, errs);
                        let r_ty = re.check_type(env, errs);
                        if l_ty != Ty::Int || r_ty != Ty::Int {
                            errs.push(format!("arithmetic operation {op} can only be applied to integers"));
                        }
                        Ty::Int
                    }
                    Gt | Ge | Lt | Le | Eq | NotEq => {
                        let l_ty = le.check_type(env, errs);
                        let r_ty = re.check_type(env, errs);
                        if l_ty != r_ty {
                            errs.push(format!(
                                "comparison operations {op} can only be applied to the same types"
                            ));
                        }
                        Ty::Bool
                    }
                    And | Or => {
                        let l_ty = le.check_type(env, errs);
                        let r_ty = re.check_type(env, errs);
                        if l_ty != Ty::Bool || r_ty != Ty::Bool {
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
                    if init_ty != anno_ty {
                        errs.push(format!(
                            "mismatched initializer type, expected {anno_ty}, found {init_ty}"
                        ));
                    }
                    env.bind(id, ty.into());
                }
            }
            Func { id, sig, body } => {
                env.enter();
                sig.params.iter().for_each(|p| env.bind(&p.id, (&p.ty).into()));
                let body_ret_ty = body.check_type(env, errs);
                env.leave();

                let ret_ty = (&sig.ret_ty).into();
                if body_ret_ty != ret_ty {
                    errs.push(format!(
                        "mismatched return type in funciton {id}, expected {ret_ty}, found {body_ret_ty}"
                    ));
                }
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
                    arr_size: Some(if pred != Ty::Unknown { types.len() as i32 } else { 0 }),
                    ty: Box::new(pred),
                }
            }
            Expr(expr) => expr.check_type(env, errs),
        }
    }
}

impl TypeCheck for Block {
    fn check_type(&self, env: &mut SymbolTable, errs: &mut Vec<String>) -> Ty {
        let mut ret_ty = Ty::Void;
        for stmt in self.0.iter() {
            ret_ty = stmt.check_type(env, errs);
        }
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
                if cond_ty != Ty::Bool {
                    errs.push(format!("condition of if clause must be boolean type, found {cond_ty}"));
                }

                env.enter();
                let t_ty = t_branch.check_type(env, errs);
                env.leave();
                if t_ty != Ty::Void {
                    return t_ty;
                }
                if let Some(f_branch) = f_branch {
                    env.enter();
                    let ty = f_branch.check_type(env, errs);
                    env.leave();
                    ty
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
                    if cond_ty != Ty::Bool {
                        errs.push(format!("condition of for clause must be boolean type, found {cond_ty}"));
                    }
                }
                if let Some(update) = update {
                    update.check_type(env, errs);
                }

                env.enter();
                let ret_ty = body.check_type(env, errs);
                env.leave();
                ret_ty
            }
            Block(block) => {
                env.enter();
                let ret_ty = block.check_type(env, errs);
                env.leave();
                ret_ty
            },
        }
    }
}
