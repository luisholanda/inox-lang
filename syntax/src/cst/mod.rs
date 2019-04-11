use bumpalo::collections::{String, Vec};

mod expr;
mod stmt;
mod typ;

pub use expr::*;
pub use stmt::*;
pub use typ::*;

/// A syntatic expression.
///
/// The parser only do syntatic analysis. Symbols definitions and
/// reference checkings are done in the following steps.
pub type SynExpr<'arena> = Expr<'arena, String<'arena>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Module<'arena, N> {
    /// All the definitions in the module.
    pub definitions: Vec<'arena, Def<'arena, N>>
}

/// Top-level definitions.
#[derive(Debug, Clone, PartialEq)]
pub enum Def<'arena, N> {
    Func(Func<'arena, N>),
    Sign(Sign<'arena, N>),
}

/// A Function definition.
#[derive(Debug, Clone, PartialEq)]
pub struct Func<'arena, N> {
    /// The name of the function.
    pub name: N,
    /// The name of the arguments.
    pub args: Vec<'arena, N>,
    /// The body of the function.
    pub body: Expr<'arena, N>,
}

/// A type signature.
#[derive(Debug, Clone, PartialEq)]
pub struct Sign<'arena, N> {
    /// The symbol linked to the signature.
    pub name: N,
    /// The type of the symbol referenced by `name`.
    pub type_: Type<'arena, N>,
}
