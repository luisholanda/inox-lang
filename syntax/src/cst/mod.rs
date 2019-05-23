pub use symbol::pos::Located;

mod expr;
mod stmt;
mod typ;

pub use crate::cst::expr::*;
pub use crate::cst::stmt::*;
pub use crate::cst::typ::*;

/// A syntactic expression.
///
/// The parser only do syntactics analysis. Symbols definitions and
/// reference checking are done in the following steps.
pub type SynExpr = Expr<String>;

#[derive(Debug, Clone, PartialEq)]
pub struct Module<N> {
    /// All the definitions in the module.
    pub definitions: Vec<Def<N>>,
}

pub type Def<N> = Located<DefNode<N>>;

/// Top-level definitions.
#[derive(Debug, Clone, PartialEq)]
pub enum DefNode<N> {
    Func(Func<N>),
    Sign(Sign<N>),
}

/// A Function definition.
#[derive(Debug, Clone, PartialEq)]
pub struct Func<N> {
    /// The name of the function.
    pub name: Located<N>,
    /// The name of the arguments.
    pub args: Vec<Located<N>>,
    /// The body of the function.
    pub body: Expr<N>,
}

/// A type signature.
#[derive(Debug, Clone, PartialEq)]
pub struct Sign<N> {
    /// The symbol linked to the signature.
    pub name: Located<N>,
    /// The type of the symbol referenced by `name`.
    pub type_: Type<N>,
    /// The optional documentation of the signature.
    pub doc: Option<String>,
}
