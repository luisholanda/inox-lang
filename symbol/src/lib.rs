use fnv::FnvHashMap;

use crate::pos::{Location, Span};
pub use crate::typ::{BltInTy, Type};

pub mod id;
pub mod pos;
mod typ;

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

/// A `Symbol` definition.
///
/// Holds all the information about the place of definition
/// of the symbol.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Definition {
    /// The name of the symbol.
    pub name: String,
    /// The source file location of the definition.
    pub location: Span<Location>,
    /// The type of the symbol.
    pub typ: typ::Type,
    /// The symbol is mutable?
    pub mutable: bool,
    /// The symbol was initialized?
    pub initialized: bool,
    /// The symbol was used at least once?
    pub referenced: bool,
}

impl Definition {
    pub fn new(name: &str, location: Span<Location>, typ: typ::Type) -> Self {
        Self {
            name: name.to_owned(),
            location,
            typ,
            mutable: false,
            initialized: false,
            referenced: false,
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

    pub fn get_mut_definition(&mut self, sym: Symbol) -> Option<&mut Definition> {
        self.syms.get_mut(&sym.idx)
    }
}

impl IntoIterator for SymTable {
    type IntoIter = SymTableIter;
    type Item = <SymTableIter as Iterator>::Item;

    fn into_iter(self) -> Self::IntoIter {
        SymTableIter {
            iter: self.syms.into_iter(),
        }
    }
}

pub struct SymTableIter {
    iter: <FnvHashMap<id::Id, Definition> as IntoIterator>::IntoIter,
}

impl Iterator for SymTableIter {
    type Item = (Symbol, Definition);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|(idx, def)| (Symbol::from_id(idx), def))
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

impl Default for SymGen {
    fn default() -> Self {
        SymGen::new()
    }
}
