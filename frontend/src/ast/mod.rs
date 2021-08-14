use std::collections::BTreeMap;
use symbol::{Definition, Symbol, Table};
use syntax::cst::{ArithOp, LogicOp};

pub mod desugar;

/// A file defined module.
#[derive(Debug, Clone)]
pub struct Module {
    /// The items defined in the module.
    /// Stored in the order they are defined.
    pub items: BTreeMap<Symbol, Item>,

    /// A table with the definition of all the symbols in the module,
    /// be they exported or not.
    pub names: Table<Definition>,
}

impl Module {
    pub fn item(&self, item: Symbol) -> Option<&Item> {
        self.items.get(&item)
    }
}

#[derive(Debug, Clone)]
pub enum Item {
    Fn(Func),
}

#[derive(Debug, Clone)]
pub struct Func {
    pub sym: Symbol,
    pub arguments: Vec<Symbol>,
    pub body: Block,
}

#[derive(Debug, Clone, Default)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub last: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub node: StmtKind,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Expr(Expr),
    Let(Let),
    While(While),
    For(For),
    Assign(Assign),
}

#[derive(Debug, Clone)]
pub struct Let {
    pub ident: Symbol,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub cond: Expr,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct For {
    pub ident: Symbol,
    pub iter: Expr,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub to: AssignKind,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub enum AssignKind {
    Sym(Symbol),
    Index(Symbol, Expr),
    Field(Symbol, String),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub node: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Term(Term),
    Block(Block),
    If(If),
    Match(Match),
    Arith(Arith),
    Logic(Logic),
    Question(Box<Expr>),
    Call(Call),
    Method(Method),
}

#[derive(Debug, Clone)]
pub enum Term {
    Var(Symbol),
    Lit(Literal),
    Field { owner: Box<Expr>, field: String },
    Index { owner: Box<Expr>, index: Box<Expr> },
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Int(i32),
    Flt(f32),
    Chr(char),
    Bool(bool),
    Vec(Vec<Expr>),
    Map(Vec<(Expr, Expr)>),
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Box<Expr>,
    pub body: Block,
    pub else_body: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct Match {
    pub expr: Box<Expr>,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub case: MatchCase,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum MatchCase {
    Term(Symbol),
    If(Symbol, Box<Expr>),
}

pub type Arith = Oper<ArithOp>;

#[derive(Debug, Clone)]
pub struct Oper<Op> {
    pub lhs: Box<Expr>,
    pub op: Op,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum Logic {
    Not(Box<Expr>),
    Oper(Oper<LogicOp>),
}

#[derive(Debug, Clone)]
pub struct Call {
    pub func: Symbol,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Method {
    pub receiver: Box<Expr>,
    pub method: Symbol,
    pub args: Vec<Expr>,
}
