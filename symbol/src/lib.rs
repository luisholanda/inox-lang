#![feature(copied)]

use id_arena::{Arena, Id};
use fnv::FnvHashMap;

use pos::{Span, Location};

pub mod pos;

/// A full qualified Identifier.
///
/// Each part introduces a new qualification for the name.
/// The last part is the non-qualified part of the identifier.
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Ident {
    /// Span of the identifier.
    span: Span<Location>,
    /// Qualifications of the identifier.
    parts: Vec<IdentPart>
}

impl Ident {
    pub fn new(span: Span<Location>, parts: Vec<IdentPart>) -> Self {
        Self { span, parts }
    }

    pub fn new_single(span: Span<Location>, ident: Symbol) -> Self {
        Self { span, parts: vec![IdentPart::new(span, ident)] }
    }
}

/// An identifier qualification.
///
/// Can be a package or type name.
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub  struct IdentPart {
    /// Span of the part.
    span: Span<Location>,
    /// Symbol of the part.
    part: Symbol,
}

impl IdentPart {
    pub fn new(span: Span<Location>, part: Symbol) -> Self {
        Self { span, part }
    }
}

/// A symbol.
///
/// This abstracts the name of the symbol, solving
/// shadowing disambiguation.
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Symbol {
    idx: Id<String>
}

impl Symbol {
    fn from_id(idx: Id<String>) -> Self {
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
    pub name: &'static str,
    /// The source file location of the definition.
    pub location: Span<Location>
}

/// A symbol table.
///
/// Implemented using an Arena, making the `&'static` lifetime
/// in the definition and in `Definition` valid.
///
/// Being more specific, the correct lifetime would be the
/// lifetime of the `SymTable` itself, but I couldn't express this.
#[derive(Debug)]
pub struct SymTable {
    /// The Arena that hosts all the `String`s for the known names.
    arena: Arena<String>,
    /// Map from symbols to the it definitions.
    syms: FnvHashMap<Id<String>, Definition>,
    /// Known names.
    /// Provides a way to get the correct symbol from it's name.
    names: FnvHashMap<&'static str, Id<String>>,
}

impl SymTable {
    fn new() -> Self {
        let arena = Arena::new();

        Self { arena, syms: Default::default(), names: Default::default() }
    }

    fn define_symbol(&mut self, name: &str, loc: Span<Location>) -> Symbol {
        let id = self.arena.alloc(name.to_owned());

        let alloc_str: &str = self.arena.get(id).unwrap();

        let stc_str: &'static str = unsafe {
            &*(alloc_str as *const str)
        };

        let def = Definition { name: stc_str, location: loc };

        self.syms.insert(id, def);
        self.names.insert(stc_str, id);

        Symbol::from_id(id)
    }

    fn get_symbol(&self, name: &str) -> Option<Symbol> {
        self.names.get(name).copied().map(Symbol::from_id)
    }

    fn get_definition(&self, sym: Symbol) -> Option<&Definition> {
        self.syms.get(&sym.idx)
    }
}


impl Default for SymTable {
    fn default() -> Self {
        Self::new()
    }
}