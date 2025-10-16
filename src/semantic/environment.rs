use std::collections::HashMap;

use crate::{ast::NodeId, semantic::types::Ty};

pub struct SymbolTable(Vec<Scope>);

impl SymbolTable {
    pub fn global() -> Self {
        Self(vec![Scope::new()])
    }
}

pub struct Scope {
    vars: HashMap<String, Ty>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }
}

pub struct FunctionContext {
    pub ret_ty: Ty,
}

pub struct Environement {
    symbols: SymbolTable,
    pub func_ctx: Option<FunctionContext>,
    ast_node_types: HashMap<NodeId, Ty>,
}

impl Environement {
    pub fn new() -> Self {
        Self {
            symbols: SymbolTable::global(),
            func_ctx: None,
            ast_node_types: HashMap::new(),
        }
    }

    pub fn enter(&mut self) {
        self.symbols.0.push(Scope::new());
    }

    pub fn leave(&mut self) {
        // protect the global scope
        if self.symbols.0.len() > 1 {
            self.symbols.0.pop();
        }
    }

    pub fn lookup(&self, id: &str) -> Option<&Ty> {
        self.symbols.0.iter().rev().find_map(|s| s.vars.get(id))
    }

    pub fn bind(&mut self, id: &str, ty: Ty) {
        if let Some(s) = self.symbols.0.last_mut() {
            s.vars.insert(id.to_string(), ty);
        }
    }

    pub fn record_node_ty(&mut self, id: NodeId, ty: Ty) {
        self.ast_node_types.insert(id, ty);
    }

    pub fn get_node_ty(&self, id: &NodeId) -> Option<Ty> {
        self.ast_node_types.get(id).cloned()
    }
}
