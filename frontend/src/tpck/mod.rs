use crate::ast::*;
use std::collections::BTreeMap;
use symbol::{BltInTy, Definition, Symbol, Table, Type};
use syntax::cst::{ArithOp, LogicOp};

pub fn type_check(ast: &mut Module) -> Vec<TypeErrors> {
    let mut checker = TypeChecker::new(&mut ast.names);

    for item in ast.items.values() {
        checker.infer_item_types(item);
    }

    checker.errors
}

#[derive(Debug, Clone)]
pub enum TypeErrors {
    TypeMismatch { expected: Type, found: Type },
    IteratorExpected { found: Type },
    FunctionExpected { found: Type },
    ArithOperandsError { lhs: Type, op: ArithOp, rhs: Type },
    LogicOperandsError { lhs: Type, op: LogicOp, rhs: Type },
    InvalidParameters { expected: usize, found: usize }
}

struct TypeChecker<'t> {
    names: &'t mut Table<Definition>,
    errors: Vec<TypeErrors>,
}

impl<'t> TypeChecker<'t> {
    fn new(names: &'t mut Table<Definition>) -> Self {
        TypeChecker {
            names,
            errors: vec![],
        }
    }

    fn infer_item_types(&mut self, item: &Item) {
        match item {
            Item::Fn(func) => self.infer_fn_types(func),
        }
    }

    fn infer_fn_types(&mut self, func: &Func) {
        self.infer_block_type(&func.body);
    }

    fn infer_block_type(&mut self, block: &Block) -> Type {
        for stmt in &block.stmts {
            self.infer_stmt_types(stmt);
        }

        if let Some(last) = &block.last {
            self.infer_expr_type(&last)
        } else {
            Type::BuiltIn(BltInTy::Unit)
        }
    }

    fn infer_stmt_types(&mut self, stmt: &Stmt) {
        match &stmt.node {
            StmtKind::Expr(expr) => {
                self.infer_expr_type(expr);
            }
            StmtKind::For(for_) => {
                self.names.get_mut(&for_.ident).unwrap().typ = {
                    match self.infer_expr_type(&for_.iter) {
                        Type::BuiltIn(BltInTy::Vec(typ)) => *typ,
                        Type::BuiltIn(BltInTy::Map(key_typ, _)) => *key_typ,
                        typ => {
                            self.errors
                                .push(TypeErrors::IteratorExpected { found: typ });
                            Type::Error
                        }
                    }
                };


                self.infer_block_type(&for_.body);
            }
            StmtKind::Let(let_) => {
                if let Some(val) = &let_.value {
                    let val_typ = self.infer_expr_type(val);

                    let ident = self.names.get_mut(&let_.ident).unwrap();
                    if ident.typ.unifies(&val_typ) {
                        ident.typ = val_typ;
                    } else {
                        self.errors.push(TypeErrors::TypeMismatch {
                            expected: ident.typ.clone(),
                            found: val_typ,
                        });
                    }
                }
            }
            StmtKind::While(whl) => {
                static BOOL: Type = Type::BuiltIn(BltInTy::Bool);
                let cond_typ = self.infer_expr_type(&whl.cond);

                if !BOOL.unifies(&cond_typ) {
                    self.errors.push(TypeErrors::TypeMismatch {
                        expected: BOOL.clone(),
                        found: cond_typ,
                    })
                }

                self.infer_block_type(&whl.body);
            }
            StmtKind::Assign(assign) => {
                let val_typ = self.infer_expr_type(&assign.value);

                self.infer_assign_kind_type(&assign.to, val_typ);
            }
        }
    }

    fn infer_assign_kind_type(&mut self, to: &AssignKind, value: Type) {
        match to {
            AssignKind::Sym(sym) => {
                let ident = self.names.get_mut(&sym).unwrap();
                if ident.typ != value {
                    self.errors.push(TypeErrors::TypeMismatch {
                        expected: ident.typ.clone(),
                        found: value,
                    });
                }
            }
            AssignKind::Index(sym, idx) => {
                let idx_typ = self.infer_expr_type(&idx);
                let ident = self.names.get_mut(&sym).unwrap();

                match &ident.typ {
                    Type::BuiltIn(BltInTy::Map(key_typ, value_typ)) => {
                        if !idx_typ.unifies(&key_typ) {
                            self.errors.push(TypeErrors::TypeMismatch {
                                expected: (**key_typ).clone(),
                                found: idx_typ,
                            });
                        }

                        if !value.unifies(&value_typ) {
                            self.errors.push(TypeErrors::TypeMismatch {
                                expected: (**value_typ).clone(),
                                found: value,
                            });
                        }
                    }
                    Type::BuiltIn(BltInTy::Vec(value_typ)) => {
                        if value.unifies(&value_typ) {
                            self.errors.push(TypeErrors::TypeMismatch {
                                expected: (**value_typ).clone(),
                                found: value,
                            });
                        }
                    }
                    typ => self
                        .errors
                        .push(TypeErrors::IteratorExpected { found: typ.clone() }),
                }
            }
            AssignKind::Field(_, _) => unimplemented!("Field access not yet implemented"),
        }
    }

    fn infer_expr_type(&mut self, expr: &Expr) -> Type {
        match &expr.node {
            ExprKind::Term(term) => self.infer_term_type(term),
            ExprKind::Block(block) => self.infer_block_type(block),
            ExprKind::If(if_) => {
                static BOOL: Type = Type::BuiltIn(BltInTy::Bool);
                let cond_typ = self.infer_expr_type(&if_.cond);
                if !BOOL.unifies(&cond_typ) {
                    self.errors.push(TypeErrors::TypeMismatch{
                        expected: BOOL.clone(),
                        found: cond_typ
                    });
                }

                let body_typ = self.infer_block_type(&if_.body);
                let else_typ = if_.else_body.as_ref().map_or(Type::BuiltIn(BltInTy::Unit), |eb| {
                    self.infer_block_type(&eb)
                });

                if !body_typ.unifies(&else_typ) {
                    self.errors.push(TypeErrors::TypeMismatch {
                        expected: body_typ.clone(),
                        found: else_typ,
                    });
                }

                body_typ
            }
            ExprKind::Arith(arith) => self.infer_arith_type(arith),
            ExprKind::Logic(logic) => {
                self.check_logic_expr(logic);

                Type::BuiltIn(BltInTy::Bool)
            }
            ExprKind::Call(call) => {
                let func_typ = self.names.get(&call.func).unwrap().typ.clone();

                match func_typ {
                    Type::Fn(args, ret) => {
                        if args.len() != call.args.len() {
                            self.errors.push(TypeErrors::InvalidParameters{
                                expected: args.len(),
                                found: call.args.len(),
                            })
                        }

                        let args_typ = call.args.iter().map(|a| self.infer_expr_type(&a));

                        args.into_iter()
                            .zip(args_typ)
                            .filter(|(fa, ca)| !fa.unifies(ca))
                            .collect::<Vec<_>>()
                            .into_iter()
                            .for_each(|(fa, ca)| {
                                self.errors.push(TypeErrors::TypeMismatch {
                                    expected: fa.clone(),
                                    found: ca.clone(),
                                })
                            });

                        *ret
                    }
                    func_typ => {
                        self.errors
                            .push(TypeErrors::FunctionExpected { found: func_typ });

                        Type::Error
                    }
                }
            }
            _ => unimplemented!("expression type not yet implemented"),
        }
    }

    fn infer_term_type(&mut self, term: &Term) -> Type {
        match term {
            Term::Var(sym) => if let Some(def) = self.names.get(sym) {
                def.typ.clone()
            } else {
                Type::Error
            },
            Term::Lit(lit) => match lit {
                Literal::String(_) => Type::BuiltIn(BltInTy::Str),
                Literal::Int(_) => Type::BuiltIn(BltInTy::Int),
                Literal::Flt(_) => Type::BuiltIn(BltInTy::Flt),
                Literal::Chr(_) => Type::BuiltIn(BltInTy::Char),
                Literal::Bool(_) => Type::BuiltIn(BltInTy::Bool),
                Literal::Vec(items) => {
                    let mut items_typ = items.iter().map(|i| self.infer_expr_type(i));
                    let first = items_typ.next().unwrap();

                    items_typ.filter(|it| !first.unifies(it))
                        .collect::<Vec<_>>()
                        .into_iter()
                        .for_each(|it| {
                            self.errors.push(TypeErrors::TypeMismatch {
                                expected: first.clone(),
                                found: it,
                            })
                        });

                    Type::BuiltIn(BltInTy::Vec(box first))
                }
                Literal::Map(items) => {
                    let mut items_typ = items
                        .iter()
                        .map(|(k, v)| (self.infer_expr_type(k), self.infer_expr_type(v)));
                    let (key, val) = items_typ.next().unwrap();
                    dbg!(&val);
                    let (keys, values): (Vec<_>, Vec<_>) = items_typ.unzip();

                    keys.into_iter().filter(|k| !key.unifies(k)).for_each(|k| {
                        self.errors.push(TypeErrors::TypeMismatch {
                            expected: key.clone(),
                            found: k,
                        })
                    });

                    values
                        .into_iter()
                        .filter(|v| !val.unifies(v))
                        .for_each(|v| {
                            self.errors.push(TypeErrors::TypeMismatch {
                                expected: val.clone(),
                                found: v,
                            })
                        });

                    Type::BuiltIn(BltInTy::Map(box key, box val))
                }
            },
            Term::Index { owner, index } => {
                let owner_typ = self.infer_expr_type(owner);

                match owner_typ {
                    Type::BuiltIn(BltInTy::Vec(item)) => {
                        let idx_typ = self.infer_expr_type(index);
                        if idx_typ != Type::BuiltIn(BltInTy::Int) {
                            self.errors.push(TypeErrors::TypeMismatch{
                                expected: (*item).clone(),
                                found: idx_typ,
                            })
                        }

                        *item
                    },
                    Type::BuiltIn(BltInTy::Map(key, val)) => {
                        let idx_typ = self.infer_expr_type(index);
                        if !key.unifies(&idx_typ) {
                            self.errors.push(TypeErrors::TypeMismatch{
                                expected: (*key).clone(),
                                found: idx_typ,
                            })
                        }

                        *val
                    },
                    _ => Type::Error,
                }
            }
            _ => unimplemented!("Only variables literals and indexing implemented"),
        }
    }

    fn infer_arith_type(&mut self, arith: &Arith) -> Type {
        let lhs_typ = self.infer_expr_type(&arith.lhs);
        let rhs_typ = self.infer_expr_type(&arith.rhs);

        match (lhs_typ, rhs_typ) {
            (Type::BuiltIn(lhs), Type::BuiltIn(rhs)) => match (lhs, rhs) {
                (BltInTy::Int, rhs) => {
                    if rhs != BltInTy::Int {
                        self.errors.push(TypeErrors::TypeMismatch {
                            expected: Type::BuiltIn(BltInTy::Int),
                            found: Type::BuiltIn(rhs),
                        });
                    }

                    Type::BuiltIn(BltInTy::Int)
                }
                (BltInTy::Flt, rhs) => {
                    if rhs != BltInTy::Int || rhs != BltInTy::Flt {
                        self.errors.push(TypeErrors::TypeMismatch {
                            expected: Type::BuiltIn(BltInTy::Flt),
                            found: Type::BuiltIn(rhs),
                        });
                    }

                    Type::BuiltIn(BltInTy::Flt)
                }
                (lhs, rhs) => {
                    self.errors.push(TypeErrors::ArithOperandsError {
                        lhs: Type::BuiltIn(lhs),
                        op: arith.op,
                        rhs: Type::BuiltIn(rhs),
                    });

                    Type::Error
                }
            },
            (lhs, rhs) => {
                if lhs != Type::Error && rhs != Type::Error {
                    self.errors.push(TypeErrors::ArithOperandsError {
                        lhs,
                        op: arith.op,
                        rhs,
                    });
                }

                Type::Error
            }
        }
    }

    fn check_logic_expr(&mut self, logic: &Logic) {
        match logic {
            Logic::Not(expr) => {
                let exp_typ = self.infer_expr_type(expr);

                if exp_typ != Type::BuiltIn(BltInTy::Bool) {
                    self.errors.push(TypeErrors::TypeMismatch {
                        expected: Type::BuiltIn(BltInTy::Bool),
                        found: exp_typ,
                    });
                }
            }
            Logic::Oper(oper) => {
                let lhs_typ = self.infer_expr_type(&oper.lhs);
                let rhs_typ = self.infer_expr_type(&oper.rhs);

                match (lhs_typ, rhs_typ) {
                    (Type::BuiltIn(BltInTy::Bool), Type::BuiltIn(BltInTy::Bool))
                        if oper.op == LogicOp::And || oper.op == LogicOp::Or =>
                    {
                        ()
                    }
                    (Type::BuiltIn(BltInTy::Int), Type::BuiltIn(BltInTy::Int))
                        if oper.op.is_comparison() =>
                    {
                        ()
                    }
                    (Type::BuiltIn(BltInTy::Flt), Type::BuiltIn(BltInTy::Flt))
                        if oper.op.is_comparison() =>
                    {
                        ()
                    }
                    (lhs, rhs) => {
                        if lhs != Type::Error && rhs != Type::Error {
                            self.errors.push(TypeErrors::LogicOperandsError {
                                lhs,
                                op: oper.op,
                                rhs,
                            });
                        }
                    }
                }
            }
        };
    }
}
