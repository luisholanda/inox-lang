use std::boxed::Box;
use std::fmt;

use crate::cst::stmt::*;

pub type ExprRef<N> = Box<Expr<N>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<N> {
    Term(Term<N>),
    Parens(ExprRef<N>),
    Block(Vec<Stmt<N>>),
    If(IfExpr<N>),
    Unless(IfExpr<N>),
    Match(MatchExpr<N>),
    Arith(Oper<ArithOp, N>),
    Logic(Logic<N>),
    Question(ExprRef<N>),
    Call(Call<N>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call<N> {
    pub func: ExprRef<N>,
    pub args: Vec<Expr<N>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term<N> {
    Var(N),
    Lit(Literal<N>),
    Field {
        owner: ExprRef<N>,
        field: N,
    },
    Index {
        owner: ExprRef<N>,
        index: ExprRef<N>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<N> {
    String(String),
    Integer(i32),
    Natural(u32),
    Float(f32),
    Char(char),
    Vec(Vec<Expr<N>>),
    Map(Vec<(Expr<N>, Expr<N>)>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfExpr<N> {
    If {
        cond: ExprRef<N>,
        expr: ExprRef<N>,
        else_expr: Option<ExprRef<N>>,
    },
    IfLet {
        assign: ExprRef<N>,
        to: N,
        expr: ExprRef<N>,
        else_expr: Option<ExprRef<N>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchExpr<N> {
    pub expr: ExprRef<N>,
    pub branches: Vec<MatchBranch<N>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchBranch<N> {
    pub case: MatchCase<N>,
    pub expr: ExprRef<N>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MatchCase<N> {
    Term(N),
    OneOf(Vec<MatchCase<N>>),
    If {
        case: Box<MatchCase<N>>,
        cond: ExprRef<N>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Logic<N> {
    Not(ExprRef<N>),
    Oper(Oper<LogicOp, N>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Oper<Op, N> {
    pub op: Op,
    pub lhs: ExprRef<N>,
    pub rhs: ExprRef<N>,
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
