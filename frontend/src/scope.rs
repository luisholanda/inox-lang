use std::cell::RefCell;
use std::rc::Rc;

use symbol::pos::{Location, Span};
use symbol::{Definition, SymTable, Symbol, Table, Type};

/// A code block scope.
///
/// Contains a set of information about symbols defined in the
/// scope and a set of method for searching and defining these terms.
#[derive(Debug, Clone, Default)]
pub struct Scope<T> {
    parents: Vec<Rc<Scope<T>>>,
    table: RefCell<T>,
}

impl<T> Scope<T> {
    pub fn new(table: T) -> Rc<Self> {
        Rc::new(Self {
            parents: vec![],
            table: RefCell::new(table),
        })
    }
}

impl<T> Scope<Table<T>> {
    /// Search for a symbol's information in all the scopes defined
    /// until the code position.
    pub fn search(&self, sym: Symbol) -> Option<&T> {
        let value;
        unsafe {
            value = (*self.table.as_ptr()).get(&sym);
        }

        if value.is_some() {
            return value;
        }

        for parent in self.parents.iter().rev() {
            if let Some(val) = parent.search(sym) {
                return Some(val);
            }
        }

        None
    }

    /// Define a new symbol information in the scope.
    /// Returns a reference to the inserted value.
    pub fn define(&self, sym: Symbol, value: T) -> &T {
        let table = self.table.as_ptr();

        unsafe {
            (*table).insert(sym, value);
            (*table).get(&sym).unwrap()
        }
    }
}

impl Scope<SymTable> {
    pub fn search(&self, name: &str) -> Option<Symbol> {
        let value;
        unsafe {
            value = (*self.table.as_ptr()).get_symbol(name);
        }

        if value.is_some() {
            return value;
        }

        for paren in self.parents.iter().rev() {
            if let Some(val) = paren.search(name) {
                return Some(val);
            }
        }

        None
    }

    pub fn define(&self, sym: Symbol, name: &str, typ: Type, span: Span<Location>) -> Symbol {
        self.table
            .borrow_mut()
            .define_symbol(sym, Definition::new(name, span, typ))
    }

    pub fn definition(&self, sym: Symbol) -> Option<&Definition> {
        let value = unsafe { (*self.table.as_ptr()).get_definition(sym) };

        if value.is_some() {
            return value;
        }

        for paren in self.parents.iter().rev() {
            if let Some(def) = paren.definition(sym) {
                return Some(def);
            }
        }

        None
    }

    pub fn definition_mut(&self, sym: Symbol) -> Option<&mut Definition> {
        let value = unsafe { (*self.table.as_ptr()).get_mut_definition(sym) };

        if value.is_some() {
            return value;
        }

        for paren in self.parents.iter().rev() {
            if let Some(def) = paren.definition_mut(sym) {
                return Some(def);
            }
        }

        None
    }

    pub fn scoped_names(self) -> SymTable {
        self.table.into_inner()
    }
}

impl<T: Default> Scope<T> {
    pub fn from_parents(parents: &[Rc<Scope<T>>]) -> Rc<Self> {
        Rc::new(Self {
            parents: parents.to_vec(),
            table: RefCell::default(),
        })
    }

    pub fn new_child(self: Rc<Self>) -> Rc<Self> {
        let mut parents = self.parents.clone();
        parents.push(self);

        Rc::new(Self {
            parents,
            table: RefCell::default(),
        })
    }
}
