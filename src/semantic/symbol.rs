use crate::semantic::types::Ty;

#[derive(Debug)]
pub struct Symbol {
    pub name: String,
    pub ty: Ty,
}

impl Symbol {
    pub fn new(name: &str, ty: Ty) -> Self {
        Self {
            name: name.to_string(),
            ty,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct SymbolId(u32);

#[derive(Debug)]
pub struct SymbolIdGenerator(u32);

impl SymbolIdGenerator {
    pub fn new() -> Self {
        Self(0)
    }

    pub fn get_id(&mut self) -> SymbolId {
        let id = self.0;
        self.0 += 1;
        SymbolId(id)
    }
}
