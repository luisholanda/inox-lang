use symbol::pos::Location;

use crate::cst::{Def, Module, Stmt, SynExpr, Type};
use crate::lexer::{error::LexerError, token::Token, Lexer};

mod grammar {
    #![allow(clippy::all)]
    include!(concat!(env!("OUT_DIR"), "/parser/grammar.rs"));
}

pub mod error;

pub type ParserResult<'a, T> = (Option<T>, &'a [error::ParserError], &'a [LexerError]);

#[derive(Debug)]
pub struct ParserSession {
    pub errors: Vec<error::ParserError>,
    pub lexer_errors: Vec<LexerError>,
}

impl ParserSession {
    pub fn new() -> Self {
        Self {
            errors: vec![],
            lexer_errors: vec![],
        }
    }

    pub fn parse(&mut self, input: &str) -> ParserResult<Module<String>> {
        let mut lex = LALRLexer::new(input);

        let result = grammar::parse_module(&mut self.errors, &mut lex);

        self.lexer_errors = lex.errors();

        (
            match result {
                Ok(expr) => Some(expr),
                Err(err) => {
                    self.errors.push(error::ParserError::from_lalrpop(err));
                    None
                }
            },
            &self.errors,
            &self.lexer_errors,
        )
    }

    pub fn parse_expr(&mut self, input: &str) -> ParserResult<SynExpr> {
        let mut lex = LALRLexer::new(input);

        let result = grammar::parse_expr(&mut self.errors, &mut lex);

        self.lexer_errors = lex.errors();

        (
            match result {
                Ok(expr) => Some(expr),
                Err(err) => {
                    self.errors.push(error::ParserError::from_lalrpop(err));
                    None
                }
            },
            &self.errors,
            &self.lexer_errors,
        )
    }

    pub fn parse_stmt(&mut self, input: &str) -> ParserResult<Stmt<String>> {
        let mut lex = LALRLexer::new(input);

        let result = grammar::parse_stmt(&mut self.errors, &mut lex);

        self.lexer_errors = lex.errors();

        (
            match result {
                Ok(stmt) => Some(stmt),
                Err(err) => {
                    self.errors.push(error::ParserError::from_lalrpop(err));
                    None
                }
            },
            &self.errors,
            &self.lexer_errors,
        )
    }

    pub fn parse_def(&mut self, input: &str) -> ParserResult<Def<String>> {
        let mut lex = LALRLexer::new(input);

        let result = grammar::parse_def(&mut self.errors, &mut lex);

        self.lexer_errors = lex.errors();

        (
            match result {
                Ok(def) => Some(def),
                Err(err) => {
                    self.errors.push(error::ParserError::from_lalrpop(err));
                    None
                }
            },
            &self.errors,
            &self.lexer_errors,
        )
    }

    pub fn parse_type(&mut self, input: &str) -> ParserResult<Type<String>> {
        let mut lex = LALRLexer::new(input);

        let result = grammar::parse_type_(&mut self.errors, &mut lex);

        self.lexer_errors = lex.errors();

        (
            match result {
                Ok(def) => Some(def),
                Err(err) => {
                    self.errors.push(error::ParserError::from_lalrpop(err));
                    None
                }
            },
            &self.errors,
            &self.lexer_errors,
        )
    }
}

struct LALRLexer<'input> {
    lexer: Lexer<'input>,
}

impl<'inp> LALRLexer<'inp> {
    fn new(input: &'inp str) -> Self {
        Self {
            lexer: Lexer::new(input),
        }
    }

    fn errors(self) -> Vec<LexerError> {
        self.lexer.errors
    }
}

impl<'inp> Iterator for LALRLexer<'inp> {
    type Item = Result<(Location, Token, Location), &'static str>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer
            .next()
            .map(|t| (t.span, t.into_inner()))
            .map(|(s, t)| Ok((s.start(), t, s.end())))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::cst::*;

    macro_rules! test_pass {
        ($src:expr, $cst:expr, $typ:ident) => {{
            let mut parser = ParserSession::new();

            let (result, par_errors, lex_errors) = parser.$typ($src);

            assert_eq!(par_errors.len(), 0);
            assert_eq!(lex_errors.len(), 0);
            assert!(result.is_some());
            assert_eq!(result.unwrap(), $cst);
        }};
    }

    macro_rules! test_expr_pass {
        ($src:expr, $cst:expr) => {{
            test_pass!($src, $cst, parse_expr);
        }};
    }

    macro_rules! test_stmt_pass {
        ($src:expr, $cst:expr) => {{
            test_pass!($src, $cst, parse_stmt);
        }};
    }

    macro_rules! test_def_pass {
        ($src: expr, $cst: expr) => {{
            test_pass!($src, $cst, parse_def);
        }};
    }

    macro_rules! int_lit {
        ($val:expr) => {{
            Expr::Term(Term::Lit(Literal::Integer($val)))
        }};
    }

    macro_rules! flt_lit {
        ($val:expr) => {{
            Expr::Term(Term::Lit(Literal::Float($val)))
        }};
    }

    macro_rules! name_lit {
        ($val:expr) => {{
            Expr::Term(Term::Var($val.to_owned()))
        }};
    }

    #[test]
    fn basic_expression() {
        test_expr_pass! {
            "1 + 1.0",
            Expr::Arith(Oper{
                lhs: box int_lit!(1),
                op: ArithOp::Add,
                rhs: box flt_lit!(1.0)
            })
        };
    }

    #[test]
    fn complex_expression() {
        test_expr_pass! {
            "1 + (2 << 3) * 3.123 * x",
            Expr::Arith(Oper{
                lhs: box int_lit!(1),
                op: ArithOp::Add,
                rhs: box Expr::Arith(Oper{
                    lhs: box Expr::Arith(Oper{
                        lhs: box Expr::Parens(box Expr::Arith(Oper{
                            lhs: box int_lit!(2),
                            op: ArithOp::LShift,
                            rhs: box int_lit!(3),
                        })),
                        op: ArithOp::Mult,
                        rhs: box flt_lit!(3.123),
                    }),
                    op: ArithOp::Mult,
                    rhs: box name_lit!("x"),
                })
            })
        }
    }

    #[test]
    fn if_expression() {
        test_expr_pass! {
            "if x > 1 and x < 10.0 { float(x); x + 1 }",
            Expr::If(IfExpr::If{
                cond: box Expr::Logic(Logic::Oper(Oper{
                    lhs: box Expr::Logic(Logic::Oper(Oper{
                        lhs: box name_lit!("x"),
                        op: LogicOp::GreaterThan,
                        rhs: box int_lit!(1),
                    })),
                    op: LogicOp::And,
                    rhs: box Expr::Logic(Logic::Oper(Oper{
                        lhs: box name_lit!("x"),
                        op: LogicOp::LessThan,
                        rhs: box flt_lit!(10.0),
                    })),
                })),
                expr: box Expr::Block(vec![
                        Stmt::Expr(Expr::Call(Call{
                            func: box name_lit!("float"),
                            args: vec![name_lit!("x")],
                        }), true),
                        Stmt::Expr(Expr::Arith(Oper{
                            lhs:  box name_lit!("x"),
                            op: ArithOp::Add,
                            rhs: box int_lit!(1),
                        }), false)
                    ]),
                else_expr: None,
            })
        }
    }

    #[test]
    fn complex_boolean_expression() {
        test_expr_pass! {
            "not x and 1 == y or x < 3 or x == 2.0",
            Expr::Logic(Logic::Oper(Oper{
                lhs: box Expr::Logic(Logic::Oper(Oper{
                    lhs: box Expr::Logic(Logic::Oper(Oper{
                        lhs: box Expr::Logic(Logic::Not(box name_lit!("x"))),
                        op: LogicOp::And,
                        rhs: box Expr::Logic(Logic::Oper(Oper{
                            lhs: box int_lit!(1),
                            op: LogicOp::DoubleEquals,
                            rhs: box name_lit!("y"),
                        })),
                    })),
                    op: LogicOp::Or,
                    rhs: box Expr::Logic(Logic::Oper(Oper{
                        lhs: box name_lit!("x"),
                        op: LogicOp::LessThan,
                        rhs: box int_lit!(3)
                    })),
                })),
                op: LogicOp::Or,
                rhs: box Expr::Logic(Logic::Oper(Oper{
                    lhs: box name_lit!("x"),
                    op: LogicOp::DoubleEquals,
                    rhs: box flt_lit!(2.0)
                }))
            }))
        }
    }

    #[test]
    fn block_expression() {
        test_expr_pass! {
            "{ test1(1, 1.0); if x { quote() } else { quote_else() }  }",
            Expr::Block(vec![
                Stmt::Expr(Expr::Call(Call{
                    func: box name_lit!("test1"),
                    args: vec![
                        int_lit!(1),
                        flt_lit!(1.0),
                    ],
                }), true),
                Stmt::Expr(Expr::If(IfExpr::If{
                    cond: box name_lit!("x"),
                    expr: box Expr::Block(vec![
                        Stmt::Expr(Expr::Call(Call{
                            func: box name_lit!("quote"),
                            args: vec![],
                        }), false),
                    ]),
                    else_expr: Some(box Expr::Block(vec![
                        Stmt::Expr(Expr::Call(Call{
                            func: box name_lit!("quote_else"),
                            args: vec![],
                        }), false)
                    ])),
                }), false),
            ])
        }
    }

    #[test]
    fn nested_blocks_expression() {
        test_expr_pass! {
            "{{ {{ {{ testing_nested() }} }} }}",
            Expr::Block(vec![
                Stmt::Expr(Expr::Block(vec![
                    Stmt::Expr(Expr::Block(vec![
                        Stmt::Expr(Expr::Block(vec![
                            Stmt::Expr(Expr::Block(vec![
                                Stmt::Expr(Expr::Block(vec![
                                    Stmt::Expr(Expr::Call(Call{
                                        func: box name_lit!("testing_nested"),
                                        args: vec![],
                                    }), false)
                                ]), false)
                            ]), false)
                        ]), false)
                    ]), false)
                ]), false)
            ])
        }
    }

    #[test]
    fn while_statement() {
        test_stmt_pass! {
            "while x.is_empty() { do_thing() }",
            Stmt::While(While{
                cond: Expr::Call(Call{
                    func: box Expr::Term(Term::Field{
                        owner: box name_lit!("x"),
                        field: "is_empty".to_owned(),
                    }),
                    args: vec![],
                }),
                expr: Expr::Block(vec![
                    Stmt::Expr(Expr::Call(Call{
                        func: box name_lit!("do_thing"),
                        args: vec![],
                    }), false)
                ])
            })
        }
    }

    fn function_definition() {
        test_def_pass! {
            "test(a, b, c) = { a; b; c }",
            Def::Func(Func{
                name: "test".to_owned(),
                args: vec![
                   "a".to_owned(),
                   "b".to_owned(),
                   "c".to_owned(),
                ],
                body: Expr::Block(vec![
                    Stmt::Expr(name_lit!("a"), true),
                    Stmt::Expr(name_lit!("b"), true),
                    Stmt::Expr(name_lit!("c"), false)
                ]),
            })
        }
    }
}
