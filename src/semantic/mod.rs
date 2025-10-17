use crate::{
    ast::Program,
    semantic::{check::SemanticCheck, context::GlobalCtx, environment::Environement},
};

mod check;
mod context;
mod environment;
mod symbol;
mod type_infer;
mod lower;
pub mod types;

impl Program {
    pub fn semantic_check(&self) -> Vec<String> {
        let mut ctx = GlobalCtx::new();
        let mut env = Environement::new(&mut ctx);
        let mut errs = Vec::new();
        self.0.iter().for_each(|decl| {
            decl.semantic_check(&mut env, &mut errs);
        });
        println!("{:?}", ctx);
        errs
    }
}
