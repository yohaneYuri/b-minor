use std::collections::HashMap;

use crate::{
    ast::NodeId,
    semantic::{
        symbol::{Symbol, SymbolId, SymbolIdGenerator},
        types::Ty,
    },
};

#[derive(Debug)]
pub struct GlobalCtx {
    sym_id_gen: SymbolIdGenerator,
    symbols: HashMap<SymbolId, Symbol>,
    node_types: HashMap<NodeId, Ty>,
    ident_bindings: HashMap<NodeId, SymbolId>,
}

impl GlobalCtx {
    pub fn new() -> Self {
        Self {
            sym_id_gen: SymbolIdGenerator::new(),
            symbols: HashMap::new(),
            node_types: HashMap::new(),
            ident_bindings: HashMap::new(),
        }
    }

    pub fn new_symbol(&mut self, id: &str, ty: Ty, node_id: NodeId) -> SymbolId {
        let sym_id = self.sym_id_gen.get_id();
        let sym = Symbol::new(id, ty);
        self.symbols.insert(sym_id, sym);
        self.ident_bindings.insert(node_id, sym_id);
        sym_id
    }

    pub fn get_symbol(&self, id: SymbolId) -> Option<&Symbol> {
        self.symbols.get(&id)
    }

    pub fn get_node_ty(&self, id: NodeId) -> Option<&Ty> {
        self.node_types.get(&id)
    }

    pub fn record_node_ty(&mut self, id: NodeId, ty: Ty) {
        self.node_types.insert(id, ty);
    }
}
