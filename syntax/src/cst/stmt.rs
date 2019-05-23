use crate::cst::expr::*;
use crate::cst::typ::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<N> {
    Expr(Expr<N>, bool),
    Let(Let<N>),
    While(While<N>),
    Until(While<N>),
    For(For<N>),
    Assign(Assign<N>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let<N> {
    pub ident: N,
    pub mutable: bool,
    pub typ: Option<Type<N>>,
    pub value: Option<Expr<N>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct While<N> {
    pub cond: Expr<N>,
    pub expr: Expr<N>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct For<N> {
    pub value: N,
    pub iter: Expr<N>,
    pub expr: Expr<N>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign<N> {
    pub to: N,
    pub expr: Expr<N>,
}
