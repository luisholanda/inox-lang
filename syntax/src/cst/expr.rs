use std::boxed::Box;
use std::fmt;
use symbol::pos::Located;

use crate::cst::stmt::*;

pub type ExprRef<N> = Box<Expr<N>>;

pub type Expr<N> = Located<ExprNode<N>>;

#[derive(Debug, Clone, PartialEq)]
pub enum ExprNode<N> {
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

pub type Call<N> = Located<CallNode<N>>;

#[derive(Debug, Clone, PartialEq)]
pub struct CallNode<N> {
    pub func: Located<N>,
    pub args: Vec<Expr<N>>,
}

pub type Term<N> = Located<TermNode<N>>;

#[derive(Debug, Clone, PartialEq)]
pub enum TermNode<N> {
    Var(Located<N>),
    Lit(Literal<N>),
    Field {
        owner: ExprRef<N>,
        field: Located<N>,
    },
    Index {
        owner: ExprRef<N>,
        index: ExprRef<N>,
    },
}

pub type Literal<N> = Located<LitNode<N>>;

#[derive(Debug, Clone, PartialEq)]
pub enum LitNode<N> {
    String(String),
    Bool(bool),
    Integer(i32),
    Float(f32),
    Char(char),
    Vec(Vec<Expr<N>>),
    Map(Vec<(Expr<N>, Expr<N>)>),
}

pub type IfExpr<N> = Located<IfNode<N>>;

#[derive(Debug, Clone, PartialEq)]
pub enum IfNode<N> {
    If {
        cond: ExprRef<N>,
        expr: ExprRef<N>,
        else_expr: Option<ExprRef<N>>,
    },
    IfLet {
        assign: ExprRef<N>,
        to: Located<N>,
        expr: ExprRef<N>,
        else_expr: Option<ExprRef<N>>,
    },
}

pub type MatchExpr<N> = Located<MatchNode<N>>;

#[derive(Debug, Clone, PartialEq)]
pub struct MatchNode<N> {
    pub expr: ExprRef<N>,
    pub branches: Vec<MatchBranch<N>>,
}

pub type MatchBranch<N> = Located<BranchNode<N>>;

#[derive(Debug, Clone, PartialEq)]
pub struct BranchNode<N> {
    pub case: MatchCase<N>,
    pub expr: ExprRef<N>,
}

pub type MatchCase<N> = Located<CaseNode<N>>;

#[derive(Debug, Clone, PartialEq)]
pub enum CaseNode<N> {
    Term(N),
    OneOf(Vec<MatchCase<N>>),
    If {
        case: Box<MatchCase<N>>,
        cond: ExprRef<N>,
    },
}

pub type Logic<N> = Located<LogicNode<N>>;

#[derive(Debug, Clone, PartialEq)]
pub enum LogicNode<N> {
    Not(ExprRef<N>),
    Oper(Oper<LogicOp, N>),
}

pub type Oper<Op, N> = Located<OperNode<Op, N>>;

#[derive(Debug, Clone, PartialEq)]
pub struct OperNode<Op, N> {
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

impl LogicOp {
    pub fn is_comparison(self) -> bool {
        self == LogicOp::GreaterEquals
            || self == LogicOp::GreaterThan
            || self == LogicOp::LessEquals
            || self == LogicOp::LessThan
            || self == LogicOp::DoubleEquals
            || self == LogicOp::NotEquals
    }
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
