#![feature(copied)]

use fnv::FnvHashMap;

use crate::pos::{Location, Span};

pub mod id;
pub mod pos;

pub type Table<T> = FnvHashMap<Symbol, T>;

/// A symbol.
///
/// This abstracts the name of the symbol, solving
/// shadowing disambiguation.
#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub struct Symbol {
    idx: id::Id,
}

impl Symbol {
    fn from_id(idx: id::Id) -> Self {
        Self { idx }
    }
}

/// `Symbol` definition.
///
/// Holds all the information about the place of definition
/// of the symbol.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Definition {
    /// The name of the symbol.
    pub name: String,
    /// The source file location of the definition.
    pub location: Span<Location>,
}

impl Definition {
    pub fn new(name: &str, location: Span<Location>) -> Self {
        Self {
            name: name.to_owned(),
            location,
        }
    }
}

/// A symbol table.
#[derive(Debug, Clone, Default)]
pub struct SymTable {
    /// Map from symbols to the it definitions.
    syms: FnvHashMap<id::Id, Definition>,
}

impl SymTable {
    fn new() -> Self {
        Self::default()
    }

    pub fn define_symbol(&mut self, sym: Symbol, def: Definition) -> Symbol {
        self.syms.insert(sym.idx, def);

        sym
    }

    pub fn get_symbol(&self, name: &str) -> Option<Symbol> {
        for (idx, def) in self.syms.iter() {
            if def.name == name {
                return Some(Symbol::from_id(*idx));
            }
        }

        None
    }

    pub fn get_definition(&self, sym: Symbol) -> Option<&Definition> {
        self.syms.get(&sym.idx)
    }
}

pub struct SymGen {
    gen: id::IdGenerator,
}

impl SymGen {
    pub fn new() -> Self {
        Self {
            gen: id::IdGenerator::new(),
        }
    }

    pub fn new_symbol(&self) -> Symbol {
        Symbol::from_id(self.gen.generate())
    }
}
