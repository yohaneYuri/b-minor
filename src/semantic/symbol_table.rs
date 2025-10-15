use std::collections::HashMap;

use crate::semantic::types::Ty;

pub struct SymbolTable(Vec<Scope>);

impl SymbolTable {
    pub fn global() -> Self {
        Self(vec![Scope::new()])
    }

    pub fn enter(&mut self) {
        self.0.push(Scope::new());
    }

    pub fn leave(&mut self) {
        // protect the global scope
        if self.0.len() > 1 {
            self.0.pop();
        }
    }

    pub fn lookup(&self, id: &str) -> Option<&Ty> {
        self.0.iter().rev().find_map(|s| s.vars.get(id))
    }

    pub fn bind(&mut self, id: &str, ty: Ty) {
        if let Some(s) = self.0.last_mut() {
            s.vars.insert(id.to_string(), ty);
        }
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
