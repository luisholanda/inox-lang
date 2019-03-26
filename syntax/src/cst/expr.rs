use bumpalo::collections::Vec;

use crate::cst::stmt::*;
use symbol::pos::Location;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'arena, N> {
    Term(Term<'arena, N>),
    Parens(&'arena Expr<'arena, N>),
    Block(Vec<'arena, Stmt<'arena, N>>),
    If(IfExpr<'arena, N>),
    Unless(IfExpr<'arena, N>),
    Match(MatchExpr<'arena, N>),
    Arith(Oper<'arena, ArithOp, N>),
    Logic(Logic<'arena, N>),
    Question(&'arena Expr<'arena, N>),
    Call { func: &'arena Expr<'arena, N>, args: Vec<'arena, Expr<'arena, N>> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term<'arena, N> {
    Var(N),
    Lit(Literal<'arena>),
    Field { owner: &'arena Expr<'arena, N>, field: N },
    Index { owner: &'arena Expr<'arena, N>, index: &'arena Expr<'arena, N> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'arena> {
    String(&'arena str),
    Integer(i32),
    Natural(u32),
    Float(f32),
    Char(char),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfExpr<'arena, N> {
    If { cond: &'arena Expr<'arena, N>, expr: &'arena Expr<'arena, N>, else_expr: Option<&'arena Expr<'arena, N>> },
    IfLet { assign: &'arena Expr<'arena, N>, to: N, expr: &'arena Expr<'arena, N>, else_expr: Option<&'arena Expr<'arena, N>> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchExpr<'arena, N> {
    pub expr: &'arena Expr<'arena, N>,
    pub branches: Vec<'arena, MatchBranch<'arena, N>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchBranch<'arena, N> {
    pub case: MatchCase<'arena, N>,
    pub expr: &'arena Expr<'arena, N>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MatchCase<'arena, N> {
    Term(N),
    OneOf(Vec<'arena, MatchCase<'arena, N>>),
    If { case: &'arena MatchCase<'arena, N>, cond: &'arena Expr<'arena, N> },
}

pub enum Logic<'arena, N> {
    Not(&'arena Expr<'arena, N>),
    Oper(Oper<'arena, LogicOp, N>)
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Oper<'arena, Op, N> {
    op: Op,
    lhs: &'arena Expr<'arena, N>,
    rhs: &'arena Expr<'arena, N>
}

#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub enum ArithOp {}

#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub enum LogicOp {}
