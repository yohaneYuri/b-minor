use std::collections::HashMap;

use crate::{
    ast::NodeId,
    semantic::{context::GlobalCtx, symbol::SymbolId, types::Ty},
};

#[derive(Debug)]
pub struct Scope {
    symbols: HashMap<String, SymbolId>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionContext {
    pub ret_ty: Ty,
}

#[derive(Debug)]
pub struct Environement<'ctx> {
    global: &'ctx mut GlobalCtx,
    scopes: Vec<Scope>,
    pub func_ctx: Option<FunctionContext>,
}

impl<'ctx> Environement<'ctx> {
    pub fn new(global: &'ctx mut GlobalCtx) -> Self {
        Self {
            global,
            scopes: Vec::new(),
            func_ctx: None,
        }
    }

    pub fn enter(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn leave(&mut self) {
        // protect the global scope
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    pub fn lookup(&self, id: &str) -> Option<&Ty> {
        self.scopes
            .iter()
            .rev()
            .find_map(|s| s.symbols.get(id))
            .map(|id| self.global.get_symbol(*id))
            .flatten()
            .map(|s| &s.ty)
    }

    pub fn bind(&mut self, id: &str, ty: Ty, node_id: NodeId) {
        if let Some(s) = self.scopes.last_mut() {
            let sym_id = self.global.new_symbol(id, ty, node_id);
            s.symbols.insert(id.to_string(), sym_id);
        }
    }

    pub fn record_node_ty(&mut self, id: NodeId, ty: Ty) {
        self.global.record_node_ty(id, ty);
    }

    pub fn get_node_ty(&self, id: NodeId) -> Option<&Ty> {
        self.global.get_node_ty(id)
    }
}
