use crate::{
    ast::Program,
    semantic::{environment::Environement, type_check::TypeCheck},
};

mod environment;
mod type_check;
mod types;

impl Program {
    pub fn semantic_check(&self) -> Vec<String> {
        let mut env = Environement::new();
        let mut errs = Vec::new();
        self.0.iter().for_each(|decl| {
            decl.check_type(&mut env, &mut errs);
        });
        errs
    }
}
