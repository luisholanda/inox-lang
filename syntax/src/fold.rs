use crate::cst::*;

pub trait Folder<'c, A, B>: Sized {
    fn fold_name(&mut self, name: &'c A) -> B;
    fn fold_type_name(&mut self, typ_name: &'c A) -> B;
    fn fold_type_var(&mut self, typ_var: &'c A) -> B;

    fn fold_module(&mut self, module: &'c Module<A>) -> Module<B> {
        let mut definitions = Vec::with_capacity(module.definitions.len());
        let mut funcs = Vec::with_capacity(module.definitions.len());

        for def in &module.definitions {
            match def {
                Def::Sign(sign) => definitions.push(self.fold_sign(sign)),
                Def::Func(func) => funcs.push(func),
            }
        }

        for func in funcs.drain(..) {
            definitions.push(self.fold_func(func))
        }

        Module { definitions }
    }

    fn fold_func(&mut self, func: &'c Func<A>) -> Func<B> {
        Func {
            name: self.fold_name(&func.name),
            args: func.args.iter().map(|a| self.fold_name(a)),
            body: self.fold_block(&func.body),
        }
    }

    fn fold_sign(&mut self, sign: &'c Sign<A>) -> Sign<B> {
        Sign {
            name: self.fold_name(&sign.name),
            type_: self.fold_type(&sign.type_)
        }
    }

    fn fold_stmt(&mut self, stmt: &'c Stmt<A>) -> Stmt<B> {
        match stmt {
            Stmt::Expr(expr, comma) =>
                Stmt::Expr(self.fold_expr(expr), *comma),
            Stmt::Let(let_) =>
                Stmt::Let(self.fold_let(let_)),
            Stmt::While(while_) =>
                Stmt::While(self.fold_while(while_)),
            Stmt::Until(until) =>
                Stmt::Until(self.fold_until(until)),
            Stmt::For(for_) =>
                Stmt::For(self.fold_for(for_)),
            Stmt::Assign(assign) =>
                Stmt::Assign(self.fold_assign(assign))
        }
    }

    fn fold_let(&mut self, let_: &'c Let<A>) -> Let<B> {
        let folded_value = let_.value.as_ref().map(|val| self.fold_expr(val));

        Let {
            ident: self.fold_name(&let_.ident),
            typ: let_.typ.as_ref().map(|typ| self.fold_type(typ)),
            value: folded_value
        }
    }
    fn fold_while(&mut self, while_: &'c While<A>) -> While<B> {
        While {
            cond: self.fold_expr(&while_.cond),
            expr: self.fold_block(&while_.expr),
        }
    }

    fn fold_until(&mut self, until: &'c While<A>) -> While<B> {
        self.fold_while(until)
    }

    fn fold_for(&mut self, for_: &'c For<A>) -> For<B> {
        let folded_iter = self.fold_expr(&for_.iter);
        let folded_value = self.fold_name(&for_.value);

        For {
            value: folded_value,
            iter: folded_iter,
            expr: self.fold_block(&for_.expr)
        }
    }

    fn fold_assign(&mut self, assign: &'c Assign<A>) -> Assign<B> {
        let folded_expr = self.fold_expr(&assign.expr);

        Assign {
            to: self.fold_name(&assign.to),
            expr: folded_expr,
        }
    }

    fn fold_expr(&mut self, expr: &'c Expr<A>) -> Expr<B> {
        match expr {
            Expr::Term(term) => Expr::Term(self.fold_term(term)),
            Expr::Parens(expr) => Expr::Parens(self.fold_expr(expr)),
            Expr::Block(block) => Expr::Block(self.fold_block(block)),
            Expr::If(if_) => Expr::If(self.fold_if(if_)),
            Expr::Unless(unless) => Expr::Unless(self.fold_unless(unless)),
            Expr::Match(match_) => Expr::Match(self.fold_match(match_)),
            Expr::Arith(arith) => Expr::Arith(self.fold_arith(arith)),
            Expr::Logic(logic) => Expr::Logic(self.fold_logic(logic)),
            Expr::Question(quest) => Expr::Question(self.fold_question(quest)),
            Expr::Call(call) => Expr::Call(self.fold_call(call)),
        }
    }

    fn fold_term(&mut self, term: &'c Term<A>) -> Term<B> {
        match term {
            Term::Var(name) => self.fold_name(name),
            Term::Lit(lit) => self.fold_lit(lit),
            Term::Field {owner, field} => {
                let folded_owner = self.fold_expr(owner);

                Term::Field {
                    owner: folded_owner,
                    field: self.fold_name(field),
                }
            }
            Term::Index {owner, index} => {
                let folded_owner = self.fold_expr(owner);
                let folded_index = self.fold_expr(index);

                Term::Index {
                    owner: folded_owner,
                    index: folded_index,
                }
            }
        }
    }

    fn fold_block(&mut self, block: &'c [Stmt<A>]) -> Vec<B> {
        block.iter().map(|stmt| self.fold_stmt(stmt)).collect()
    }

    fn fold_if(&mut self, if_: &'c IfExpr<A>) -> IfExpr<B> {
        match if_ {
            IfExpr::If {cond, expr, else_expr} => {
                let folded_cond = self.fold_expr(cond);

                IfExpr::If {
                    cond: folded_cond,
                    expr: self.fold_expr(expr),
                    else_expr: else_expr.map(|ex| self.fold_expr(&ex)),
                }
            }
            IfExpr::IfLet { assign, to, expr, else_expr } => {
                let folded_assign = self.fold_expr(assign);
                let folded_to = self.fold_name(to);

                IfExpr::IfLet {
                    assign: folded_assign,
                    to: folded_to,
                    expr: self.fold_expr(expr),
                    else_expr: else_expr.map(|ex| self.fold_expr(&ex)),
                }
            }
        }
    }

    fn fold_unless(&mut self, unless: &'c IfExpr<A>) -> IfExpr<B> {
        self.fold_if(unless)
    }

    fn fold_match(&mut self, match_: &'c MatchExpr<A>) -> MatchExpr<B> {
        let folded_expr = self.fold_expr(&match_.expr);

        MatchExpr {
            expr: folded_expr,
            branches: match_.branches.iter().map(|br| self.fold_match_branch(br)),
        }
    }

    fn fold_match_branch(&mut self, branch: &'c MatchBranch<A>) -> MatchBranch<B> {
        let folded_case = self.fold_match_case(&branch.case);
        MatchBranch {
            case: folded_case,
            expr: self.fold_expr(&branch.expr),
        }
    }

    fn fold_match_case(&mut self, case: &'c MatchCase<A>) -> MatchCase<A> {
        match case {
            MatchCase::Term(term) => self.fold_name(term),
            MatchCase::OneOf(cases) => cases.iter().map(|c| self.fold_match_case(c)),
            MatchCase::If {case, cond} => {
                let folded_case = self.fold_match_case(case);

                MatchCase::If {
                    case: folded_case,
                    cond: self.fold_expr(cond),
                }
            }
        }
    }

    fn fold_arith(&mut self, arith: &'c Oper<ArithOp, A>) -> Oper<ArithOp, B> {
        let folded_lhs = self.fold_expr(&arith.lhs);
        let folded_rhs = self.fold_expr(&arith.rhs);

        Oper {
            op: arith.op,
            lhs: folded_lhs,
            rhs: folded_rhs,
        }
    }

    fn fold_logic(&mut self, logic: &'c Logic<A>) -> Logic<B> {
        match logic {
            Logic::Not(logic) => Logic::Not(self.fold_expr(logic)),
            Logic::Oper(oper) => {
                let folded_lhs = self.fold_expr(&oper.lhs);
                let folded_rhs = self.fold_expr(&oper.rhs);

                Logic::Oper(Oper {
                    op: oper.op,
                    lhs: folded_lhs,
                    rhs: folded_rhs,
                })
            }
        }
    }

    fn fold_question(&mut self, quest: &'c Expr<A>) -> Expr<B> {
        self.fold_expr(quest)
    }

    fn fold_call(&mut self, call: &'c Call<A>) -> Call<B> {
        let folded_func = self.fold_expr(&call.func);

        Call {
            func: folded_func,
            args: call.args.iter().map(|a| self.fold_expr(a)),
        }
    }

    fn fold_lit(&mut self, lit: &'c Literal<A>) -> Literal<B> {
        match lit {
            Literal::Vec(vec) => Literal::Vec(vec.iter().map(|i| self.fold_expr(i))),
            Literal::Map(map) => Literal::Map(vec.iter().map(|(k, v)| {
                let folded_value = self.fold_expr(v);
                let folded_key = self.fold_expr(k);

                (folded_key, folded_value)
            })),
            Literal::String(str) => Literal::String(str.clone()),
            Literal::Integer(i) => Literal::Integer(*i),
            Literal::Natural(n) => Literal::Natural(*n),
            Literal::Float(f) => Literal::Float(*f),
            Literal::Char(ch) => Literal::Char(*ch),
        }
    }

    fn fold_type(&mut self, typ: &'c Type<A>) -> Type<B> {
        match typ {
            Type::Term(typ_name) => Type::Term(self.fold_type_name(typ_name)),
            Type::Var(typ_var) => Type::Var(self.fold_type_var(typ_var)),
            Type::App(base, args) => {
                let folded_base = self.fold_type(base);
                let folded_args = args.iter().map(|a| self.fold_type(a)).collect();

                Type::App(folded_base, folded_args)
            }
            Type::Arrow(arg, ret) => {
                let folded_arg = self.fold_type(arg);
                let folded_ret = self.fold_type(ret);

                Type::Arrow(folded_arg, folded_ret)
            }
            Type::Method(typ) => Type::Method(self.fold_type(typ)),
            Type::Parens(typ) => Type::Parens(self.fold_type(typ)),
            Type::Forall(forall) => Type::Forall(self.fold_forall(forall)),
        }
    }

    fn fold_forall(&mut self, forall: &'c Forall<A>) -> Forall<B> {
        let folded_vars = forall.vars.iter().map(|v| self.fold_type_var(v)).collect();
        let folded_constraint = forall.constraint.map(|c| self.fold_constraint(&c));

        Forall {
            vars: folded_vars,
            constraint: folded_constraint,
            base: self.fold_type(&forall.base),
        }
    }

    fn fold_constraint(&mut self, constr: &'c Constraint<A>) -> Constraint<B> {
        match constr {
            Constraint::Parens(constrs) =>
                Constraint::Parens(constrs.iter().map(|c| self.fold_constraint(c)).collect()),
            Constraint::Constraint(name, args) => {
                let folded_args = args.iter().map(|a| self.fold_type(a));

                Constraint::Constraint(self.fold_type_name(name), folded_args)
            }
        }
    }
}
