use std::fmt;

use bumpalo::collections::{Vec, String};

use crate::cst::stmt::*;

pub type ExprRef<'arena, N> = &'arena Expr<'arena, N>; 

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'arena, N> {
    Term(Term<'arena, N>),
    Parens(ExprRef<'arena, N>),
    Block(Vec<'arena, Stmt<'arena, N>>),
    If(IfExpr<'arena, N>),
    Unless(IfExpr<'arena, N>),
    Match(MatchExpr<'arena, N>),
    Arith(Oper<'arena, ArithOp, N>),
    Logic(Logic<'arena, N>),
    Question(ExprRef<'arena, N>),
    Call {
        func: ExprRef<'arena, N>,
        args: Vec<'arena, Expr<'arena, N>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term<'arena, N> {
    Var(N),
    Lit(Literal<'arena, N>),
    Field {
        owner: ExprRef<'arena, N>,
        field: N,
    },
    Index {
        owner: ExprRef<'arena, N>,
        index: ExprRef<'arena, N>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'arena, N> {
    String(String<'arena>),
    Integer(i32),
    Natural(u32),
    Float(f32),
    Char(char),
    Vec(Vec<'arena, Expr<'arena, N>>),
    Map(Vec<'arena, (Expr<'arena, N>, Expr<'arena, N>)>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfExpr<'arena, N> {
    If {
        cond: ExprRef<'arena, N>,
        expr: ExprRef<'arena, N>,
        else_expr: Option<ExprRef<'arena, N>>,
    },
    IfLet {
        assign: ExprRef<'arena, N>,
        to: N,
        expr: ExprRef<'arena, N>,
        else_expr: Option<ExprRef<'arena, N>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchExpr<'arena, N> {
    pub expr: ExprRef<'arena, N>,
    pub branches: Vec<'arena, MatchBranch<'arena, N>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchBranch<'arena, N> {
    pub case: MatchCase<'arena, N>,
    pub expr: ExprRef<'arena, N>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MatchCase<'arena, N> {
    Term(N),
    OneOf(Vec<'arena, MatchCase<'arena, N>>),
    If {
        case: &'arena MatchCase<'arena, N>,
        cond: ExprRef<'arena, N>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Logic<'arena, N> {
    Not(ExprRef<'arena, N>),
    Oper(Oper<'arena, LogicOp, N>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Oper<'arena, Op, N> {
    pub op: Op,
    pub lhs: ExprRef<'arena, N>,
    pub rhs: ExprRef<'arena, N>,
}

/// A binary arithmetic operator token.
#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub enum ArithOp {
    Add,
    Sub,
    Mult,
    Div,
    LShift,
    RShift,
    Mod,
    Pow,
}

impl fmt::Display for ArithOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            ArithOp::Add => "+",
            ArithOp::Sub => "-",
            ArithOp::Mult => "*",
            ArithOp::Div => "/",
            ArithOp::Pow => "**",
            ArithOp::Mod => "%",
            ArithOp::LShift => "<<",
            ArithOp::RShift => ">>",
        };

        write!(f, "{}", s)
    }
}

/// A logic operator token.
#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub enum LogicOp {
    DoubleEquals,
    NotEquals,
    LessThan,
    LessEquals,
    GreaterThan,
    GreaterEquals,
    Not,
    And,
    Or,
}

impl fmt::Display for LogicOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            LogicOp::DoubleEquals => "==",
            LogicOp::NotEquals => "!=",
            LogicOp::LessThan => "<",
            LogicOp::LessEquals => "<=",
            LogicOp::GreaterEquals => ">=",
            LogicOp::GreaterThan => ">",
            LogicOp::Not => "not",
            LogicOp::And => "and",
            LogicOp::Or => "or",
        };

        write!(f, "{}", s)
    }
}
