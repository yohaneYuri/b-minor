use std::ops::Deref;

use crate::{
    ast::{Block, Decl, Program, SpannedNode, Stmt},
    semantic::{
        environment::{Environement, FunctionContext},
        type_infer::TypeInfer,
        types::Ty,
    },
};

mod environment;
mod type_infer;
pub mod types;

impl Program {
    pub fn semantic_check(&self) -> Vec<String> {
        let mut env = Environement::new();
        let mut errs = Vec::new();
        self.0.iter().for_each(|decl| {
            decl.semantic_check(&mut env, &mut errs);
        });
        errs
    }
}

pub trait SemanticCheck {
    fn semantic_check(&self, env: &mut Environement, errs: &mut Vec<String>);
}

impl SemanticCheck for SpannedNode<Decl> {
    fn semantic_check(&self, env: &mut Environement, errs: &mut Vec<String>) {
        use Decl::*;
        match self.deref() {
            Var { ident, ty, init } => {
                if let Some(init) = init {
                    let anno_ty = ty.deref().into();
                    if anno_ty == Ty::Void {
                        errs.push(format!("variable's type cannot be void"));
                    }
                    let init_ty = init.infer_type(env, errs);
                    if init_ty != anno_ty && init_ty != Ty::Unknown {
                        errs.push(format!(
                            "mismatched initializer type, expected {anno_ty}, found {init_ty}"
                        ));
                    }
                    env.bind(&ident.0, ty.deref().into());
                }
            }
            Func { ident, sig, body } => {
                env.bind(&ident.0, sig.deref().into());
                // treat function as special and do not call <Block as TypeCheck>::check_type
                env.enter();
                let ret_ty = sig.ret_ty.deref().into();
                env.func_ctx = Some(FunctionContext { ret_ty });
                // inject parameters into the environment
                sig.params
                    .iter()
                    .for_each(|p| env.bind(&p.ident.0, p.ty.deref().into()));

                for stmt in body.0.iter() {
                    stmt.semantic_check(env, errs);
                }

                env.func_ctx = None;
                env.leave();
            }
            _ => {}
        }
    }
}

impl SemanticCheck for SpannedNode<Stmt> {
    fn semantic_check(&self, env: &mut Environement, errs: &mut Vec<String>) {
        use Stmt::*;
        match self.deref() {
            Decl(decl) => decl.semantic_check(env, errs),
            Expr(expr) => {
                expr.infer_type(env, errs);
            },
            Ret(expr) => {
                let ty = expr.infer_type(env, errs);
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
                let cond_ty = cond.infer_type(env, errs);
                if cond_ty != Ty::Bool && cond_ty != Ty::Unknown {
                    errs.push(format!(
                        "condition of if statement must be boolean type, found {cond_ty}"
                    ));
                }

                t_branch.semantic_check(env, errs);
                if let Some(block) = f_branch {
                    block.semantic_check(env, errs);
                }
            }
            For {
                init,
                cond,
                update,
                body,
            } => {
                if let Some(init) = init {
                    init.infer_type(env, errs);
                }
                if let Some(cond) = cond {
                    let cond_ty = cond.infer_type(env, errs);
                    if cond_ty != Ty::Bool && cond_ty != Ty::Unknown {
                        errs.push(format!(
                            "condition of for statement must be boolean type, found {cond_ty}"
                        ));
                    }
                }
                if let Some(update) = update {
                    update.infer_type(env, errs);
                }

                body.semantic_check(env, errs);
            }
            Block(block) => block.semantic_check(env, errs),
        }
    }
}

impl SemanticCheck for SpannedNode<Block> {
    fn semantic_check(&self, env: &mut Environement, errs: &mut Vec<String>) {
        env.enter();
        for stmt in self.0.iter() {
            stmt.semantic_check(env, errs);
        }
        env.leave();
    }
}
