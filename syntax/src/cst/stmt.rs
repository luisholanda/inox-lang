use crate::cst::expr::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'arena, N> {
    Expr(&'arena Expr<'arena, N>),
    Let(Let<'arena, N>),
    While { cond: &'arena Expr<'arena, N>, expr: &'arena Expr<'arena, N> },
    For { value: N, iter: &'arena Expr<'arena, N>, expr: &'arena Expr<'arena, N> }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let<'arena, N> {
    ident: N,
    value: &'arena Expr<'arena, N>
}