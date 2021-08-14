use symbol::pos::Located;

use crate::cst::expr::*;
use crate::cst::typ::*;

pub type Stmt<N> = Located<StmtNode<N>>;

#[derive(Debug, Clone, PartialEq)]
pub enum StmtNode<N> {
    Expr(Expr<N>, bool),
    Let(Let<N>),
    While(While<N>),
    Until(While<N>),
    For(For<N>),
    Assign(Assign<N>),
}

pub type Let<N> = Located<LetNode<N>>;

#[derive(Debug, Clone, PartialEq)]
pub struct LetNode<N> {
    pub ident: Located<N>,
    pub mutable: bool,
    pub typ: Option<Type<N>>,
    pub value: Option<Expr<N>>,
}

pub type While<N> = Located<WhileNode<N>>;

#[derive(Debug, Clone, PartialEq)]
pub struct WhileNode<N> {
    pub cond: Expr<N>,
    pub expr: Expr<N>,
}

pub type For<N> = Located<ForNode<N>>;

#[derive(Debug, Clone, PartialEq)]
pub struct ForNode<N> {
    pub value: Located<N>,
    pub iter: Expr<N>,
    pub expr: Expr<N>,
}

pub type Assign<N> = Located<AssignNode<N>>;

#[derive(Debug, Clone, PartialEq)]
pub struct AssignNode<N> {
    pub to: Term<N>,
    pub expr: Expr<N>,
}
