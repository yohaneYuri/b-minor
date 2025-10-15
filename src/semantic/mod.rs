use crate::{
    ast::Program,
    semantic::{symbol_table::SymbolTable, type_check::TypeCheck},
};

mod symbol_table;
mod type_check;
mod types;
mod control_flow;

impl Program {
    pub fn semantic_check(&self) -> Vec<String> {
        let mut env = SymbolTable::global();
        let mut errs = Vec::new();
        self.0.iter().for_each(|decl| {
            decl.check_type(&mut env, &mut errs);
        });
        errs
    }
}
