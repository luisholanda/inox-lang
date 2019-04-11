use crate::cst::expr::*;
use crate::cst::typ::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'arena, N> {
    Expr(ExprRef<'arena, N>, bool),
    Let(Let<'arena, N>),
    While(While<'arena, N>),
    Until(While<'arena, N>),
    For(For<'arena, N>),
    Assign(Assign<'arena, N>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let<'arena, N> {
    pub ident: N,
    pub typ: Option<Type<'arena, N>>,
    pub value: Option<ExprRef<'arena, N>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct While<'arena, N> {
    pub cond: ExprRef<'arena, N>,
    pub expr: ExprRef<'arena, N>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct For<'arena, N> {
    pub value: N,
    pub iter: ExprRef<'arena, N>,
    pub expr: ExprRef<'arena, N>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign<'arena, N> {
    pub to: N,
    pub expr: ExprRef<'arena, N>,
}
