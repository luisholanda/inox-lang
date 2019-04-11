use bumpalo::{Bump, collections::String};

use symbol::pos::Location;

use crate::cst::{Stmt, SynExpr, Def, Type, Module};
use crate::lexer::{token::Token, Lexer, error::LexerError};

mod grammar {
    #![allow(clippy::all)]
    include!(concat!(env!("OUT_DIR"), "/parser/grammar.rs"));
}

pub mod error;

pub type ParserResult<'a, T> = (Option<T>, &'a [error::ParserError], &'a [LexerError]);

#[derive(Debug)]
pub struct ParserSession<'arena, 'pool: 'arena> {
    arena: &'arena mut Bump,
    string_pool: &'pool mut Bump,
    pub errors: Vec<error::ParserError>,
    pub lexer_errors: Vec<LexerError>,
}

impl<'arena, 'pool: 'arena> ParserSession<'arena, 'pool> {
    pub fn new(arena: &'arena mut Bump, string_pool: &'pool mut Bump) -> Self {
        Self {
            arena,
            string_pool,
            errors: vec![],
            lexer_errors: vec![],
        }
    }

    pub fn parse(&'arena mut self, input: &str) -> ParserResult<'arena, Module<'arena, String<'arena>>> {
        let mut lex = LALRLexer::new(input);

        let result = grammar::parse_module(self.arena, &mut self.errors, self.string_pool, &mut lex);

        self.lexer_errors = lex.errors();

        (match result {
            Ok(expr) => Some(expr),
            Err(err) => {
                self.errors.push(error::ParserError::from_lalrpop(err));
                None
            }
        }, &self.errors, &self.lexer_errors)
    }

    pub fn parse_expr(&'arena mut self, input: &str) -> ParserResult<'arena, SynExpr<'arena>> {
        let mut lex = LALRLexer::new(input);

        let result = grammar::parse_expr(self.arena, &mut self.errors, self.string_pool, &mut lex);

        self.lexer_errors = lex.errors();

        (match result {
            Ok(expr) => Some(expr),
            Err(err) => {
                self.errors.push(error::ParserError::from_lalrpop(err));
                None
            }
        }, &self.errors, &self.lexer_errors)
    }

    pub fn parse_stmt(&'arena mut self, input: &str) -> ParserResult<'arena, Stmt<'arena, String<'arena>>> {
        let mut lex = LALRLexer::new(input);

        let result = grammar::parse_stmt(self.arena, &mut self.errors, self.string_pool, &mut lex);

        self.lexer_errors = lex.errors();

        (match result {
            Ok(stmt) => Some(stmt),
            Err(err) => {
                self.errors.push(error::ParserError::from_lalrpop(err));
                None
            }
        }, &self.errors, &self.lexer_errors)
    }

    pub fn parse_def(&'arena mut self, input: &str) -> ParserResult<'arena, Def<'arena, String<'arena>>> {
        let mut lex  = LALRLexer::new(input);

        let result = grammar::parse_def(self.arena, &mut self.errors, self.string_pool, &mut lex);

        self.lexer_errors = lex.errors();

        (match result {
            Ok(def) => Some(def),
            Err(err) => {
                self.errors.push(error::ParserError::from_lalrpop(err));
                None
            }
        }, &self.errors, &self.lexer_errors)
    }

    pub fn parse_type(&'arena mut self, input: &str) -> ParserResult<'arena, Type<'arena, String<'arena>>> {
        let mut lex  = LALRLexer::new(input);

        let result = grammar::parse_type_(self.arena, &mut self.errors, self.string_pool, &mut lex);

        self.lexer_errors = lex.errors();

        (match result {
            Ok(def) => Some(def),
            Err(err) => {
                self.errors.push(error::ParserError::from_lalrpop(err));
                None
            }
        }, &self.errors, &self.lexer_errors)
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
            .map(|t| Ok((t.span.start(), t.value, t.span.end())))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::cst::*;

    use bumpalo::collections::{String, Vec};

    macro_rules! test_expr_pass {
        ($src:expr, $cst:expr) => {{
            let mut arena = Bump::new();
            let mut string_pool = Bump::new();

            let mut parser = ParserSession::new(&mut arena, &mut string_pool);

            let result = parser.parse_expr($src).take();

            assert!(result.is_some());
            assert_eq!(result.unwrap(), $cst);
        }};
    }

    macro_rules! test_stmt_pass {
        ($src:expr, $cst:expr) => {{
            let mut arena = Bump::new();
            let mut string_pool = Bump::new();

            let mut parser = ParserSession::new(&mut arena, &mut string_pool);

            let result = parser.parse_stmt($src).take();

            assert!(result.is_some());
            assert_eq!(result.unwrap(), $cst);
        }}
    }

    macro_rules! test_def_pass {
        ($src: expr, $cst: expr) => {{
            let mut arena = Bump::new();
            let mut string_pool = Bump::new();

            let mut parser = ParserSession::new(&mut arena, &mut string_pool);

            let result = parser.parse_def($src).take();

            assert!(result.is_some());
            assert_eq!(result.unwrap(), $cst);
        }}
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
        ($val:expr, $pool:expr) => {{
            Expr::Term(Term::Var(String::from_str_in($val, &$pool)))
        }};
    }

    #[test]
    fn basic_expression() {
        test_expr_pass! {
            "1 + 1.0",
            Expr::Arith(Oper{
                lhs: &int_lit!(1),
                op: ArithOp::Add,
                rhs: &flt_lit!(1.0)
            })
        };
    }

    #[test]
    fn complex_expression() {
        let pool = Bump::new();
        test_expr_pass! {
            "1 + (2 << 3) * 3.123 * x",
            Expr::Arith(Oper{
                lhs: &int_lit!(1),
                op: ArithOp::Add,
                rhs: &Expr::Arith(Oper{
                    lhs: &Expr::Arith(Oper{
                        lhs: &Expr::Parens(&Expr::Arith(Oper{
                            lhs: &int_lit!(2),
                            op: ArithOp::LShift,
                            rhs: &int_lit!(3),
                        })),
                        op: ArithOp::Mult,
                        rhs: &flt_lit!(3.123),
                    }),
                    op: ArithOp::Mult,
                    rhs: &name_lit!("x", pool),
                })
            })
        }
    }

    #[test]
    fn if_expression() {
        let pool = Bump::new();

        test_expr_pass! {
            "if x > 1 and x < 10.0 { float(x); x + 1 }",
            Expr::If(IfExpr::If{
                cond: &Expr::Logic(Logic::Oper(Oper{
                    lhs: &Expr::Logic(Logic::Oper(Oper{
                        lhs: &name_lit!("x", pool),
                        op: LogicOp::GreaterThan,
                        rhs: &int_lit!(1),
                    })),
                    op: LogicOp::And,
                    rhs: &Expr::Logic(Logic::Oper(Oper{
                        lhs: &name_lit!("x", pool),
                        op: LogicOp::LessThan,
                        rhs: &flt_lit!(10.0),
                    })),
                })),
                expr: &Expr::Block(Vec::from_iter_in(vec![
                        Stmt::Expr(&Expr::Call{
                            func: &name_lit!("float", pool),
                            args: Vec::from_iter_in(vec![name_lit!("x", pool)].into_iter(), &pool)
                        }, true),
                        Stmt::Expr(&Expr::Arith(Oper{
                            lhs:  &name_lit!("x", pool),
                            op: ArithOp::Add,
                            rhs: &int_lit!(1),
                        }), false)
                    ].into_iter(), &pool)),
                else_expr: None,
            })
        }
    }

    #[test]
    fn complex_boolean_expression() {
        let pool = Bump::new();

        test_expr_pass! {
            "not x and 1 == y or x < 3 or x == 2.0",
            Expr::Logic(Logic::Oper(Oper{
                lhs: &Expr::Logic(Logic::Oper(Oper{
                    lhs: &Expr::Logic(Logic::Oper(Oper{
                        lhs: &Expr::Logic(Logic::Not(&name_lit!("x", pool))),
                        op: LogicOp::And,
                        rhs: &Expr::Logic(Logic::Oper(Oper{
                            lhs: &int_lit!(1),
                            op: LogicOp::DoubleEquals,
                            rhs: &name_lit!("y", pool),
                        })),
                    })),
                    op: LogicOp::Or,
                    rhs: &Expr::Logic(Logic::Oper(Oper{
                        lhs: &name_lit!("x", pool),
                        op: LogicOp::LessThan,
                        rhs: &int_lit!(3)
                    })),
                })),
                op: LogicOp::Or,
                rhs: &Expr::Logic(Logic::Oper(Oper{
                    lhs: &name_lit!("x", pool),
                    op: LogicOp::DoubleEquals,
                    rhs: &flt_lit!(2.0)
                }))
            }))
        }
    }

    #[test]
    fn block_expression() {
        let pool = Bump::new();

        test_expr_pass! {
            "{ test1(1, 1.0); if x { quote() } else { quote_else() }  }",
            Expr::Block(Vec::from_iter_in(vec![
                Stmt::Expr(&Expr::Call{
                    func: &name_lit!("test1", pool),
                    args: Vec::from_iter_in(vec![
                        int_lit!(1),
                        flt_lit!(1.0),
                    ].into_iter(), &pool)
                }, true),
                Stmt::Expr(&Expr::If(IfExpr::If{
                    cond: &name_lit!("x", pool),
                    expr: &Expr::Block(Vec::from_iter_in(vec![
                        Stmt::Expr(&Expr::Call{
                            func: &name_lit!("quote", pool),
                            args: Vec::new_in(&pool),
                        }, false),
                    ].into_iter(), &pool)),
                    else_expr: Some(&Expr::Block(Vec::from_iter_in(vec![
                        Stmt::Expr(&Expr::Call{
                            func: &name_lit!("quote_else", pool),
                            args: Vec::new_in(&pool),
                        }, false)
                    ].into_iter(), &pool))),
                }), false),
            ].into_iter(), &pool))
        }
    }

    #[test]
    fn nested_blocks_expression() {
        let pool = Bump::new();

        test_expr_pass! {
            "{{ {{ {{ testing_nested() }} }} }}",
            Expr::Block(Vec::from_iter_in(vec![
                Stmt::Expr(&Expr::Block(Vec::from_iter_in(vec![
                    Stmt::Expr(&Expr::Block(Vec::from_iter_in(vec![
                        Stmt::Expr(&Expr::Block(Vec::from_iter_in(vec![
                            Stmt::Expr(&Expr::Block(Vec::from_iter_in(vec![
                                Stmt::Expr(&Expr::Block(Vec::from_iter_in(vec![
                                    Stmt::Expr(&Expr::Call{
                                        func: &name_lit!("testing_nested", pool),
                                        args: Vec::new_in(&pool),
                                    }, false)
                                ].into_iter(), &pool)), false)
                            ].into_iter(), &pool)), false)
                        ].into_iter(), &pool)), false)
                    ].into_iter(), &pool)), false)
                ].into_iter(), &pool)), false)
            ].into_iter(), &pool))
        }
    }

    #[test]
    fn while_statement() {
        let pool = Bump::new();

        test_stmt_pass! {
            "while x.is_empty() { do_thing() }",
            Stmt::While(While{
                cond: &Expr::Call{
                    func: &Expr::Term(Term::Field{
                        owner: &name_lit!("x", pool),
                        field: String::from_str_in("is_empty", &pool),
                    }),
                    args: Vec::new_in(&pool),
                },
                expr: &Expr::Block(Vec::from_iter_in(vec![
                    Stmt::Expr(&Expr::Call{
                        func: &name_lit!("do_thing", pool),
                        args: Vec::new_in(&pool),
                    }, false)
                ].into_iter(), &pool))
            })
        }
    }

    fn function_definition() {
        let pool = Bump::new();

        test_def_pass! {
            "test(a, b, c) = { a; b; c }",
            Def::Func(Func{
                name: String::from_str_in("test", &pool),
                args: Vec::from_iter_in(vec![
                   String::from_str_in("a", &pool),
                   String::from_str_in("b", &pool),
                   String::from_str_in("c", &pool),
                ].into_iter(), &pool),
                body: Expr::Block(Vec::from_iter_in(vec![
                    Stmt::Expr(&name_lit!("a", &pool), true),
                    Stmt::Expr(&name_lit!("b", &pool), true),
                    Stmt::Expr(&name_lit!("c", &pool), false)
                ].into_iter(), &pool)),
            })
        }
    }
}
