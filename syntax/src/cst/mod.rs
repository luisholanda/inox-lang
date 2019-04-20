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

/// Top-level definitions.
#[derive(Debug, Clone, PartialEq)]
pub enum Def<N> {
    Func(Func<N>),
    Sign(Sign<N>),
}

/// A Function definition.
#[derive(Debug, Clone, PartialEq)]
pub struct Func<N> {
    /// The name of the function.
    pub name: N,
    /// The name of the arguments.
    pub args: Vec<N>,
    /// The body of the function.
    pub body: Expr<N>,
}

/// A type signature.
#[derive(Debug, Clone, PartialEq)]
pub struct Sign<N> {
    /// The symbol linked to the signature.
    pub name: N,
    /// The type of the symbol referenced by `name`.
    pub type_: Type<N>,
}
