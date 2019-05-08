use crate::cst::*;
use std::borrow::Borrow;

pub trait Folder<A, B>: Sized {
    fn fold_name(&mut self, name: &A) -> B;
    fn fold_type_name(&mut self, typ_name: &A) -> B;
    fn fold_type_var(&mut self, typ_var: &A) -> B;

    fn fold_module(&mut self, module: &Module<A>) -> Module<B> {
        fold_module(self, module)
    }

    fn fold_func(&mut self, func: &Func<A>) -> Func<B> {
        fold_func(self, func)
    }

    fn fold_sign(&mut self, sign: &Sign<A>) -> Sign<B> {
        fold_sign(self, sign)
    }

    fn fold_stmt(&mut self, stmt: &Stmt<A>) -> Stmt<B> {
        fold_stmt(self, stmt)
    }

    fn fold_let(&mut self, let_: &Let<A>) -> Let<B> {
        fold_let(self, let_)
    }
    fn fold_while(&mut self, while_: &While<A>) -> While<B> {
        fold_while(self, while_)
    }

    fn fold_until(&mut self, until: &While<A>) -> While<B> {
        fold_until(self, until)
    }

    fn fold_for(&mut self, for_: &For<A>) -> For<B> {
        fold_for(self, for_)
    }

    fn fold_assign(&mut self, assign: &Assign<A>) -> Assign<B> {
        fold_assign(self, assign)
    }

    fn fold_expr(&mut self, expr: &Expr<A>) -> Expr<B> {
        fold_expr(self, expr)
    }

    fn fold_term(&mut self, term: &Term<A>) -> Term<B> {
        fold_term(self, term)
    }

    fn fold_block(&mut self, block: &[Stmt<A>]) -> Vec<Stmt<B>> {
        fold_block(self, block)
    }

    fn fold_if(&mut self, if_: &IfExpr<A>) -> IfExpr<B> {
        fold_if(self, if_)
    }

    fn fold_unless(&mut self, unless: &IfExpr<A>) -> IfExpr<B> {
        fold_unless(self, unless)
    }

    fn fold_match(&mut self, match_: &MatchExpr<A>) -> MatchExpr<B> {
        fold_match(self, match_)
    }

    fn fold_match_branch(&mut self, branch: &MatchBranch<A>) -> MatchBranch<B> {
        fold_match_branch(self, branch)
    }

    fn fold_match_case(&mut self, case: &MatchCase<A>) -> MatchCase<B> {
        fold_match_case(self, case)
    }

    fn fold_arith(&mut self, arith: &Oper<ArithOp, A>) -> Oper<ArithOp, B> {
        fold_arith(self, arith)
    }

    fn fold_logic(&mut self, logic: &Logic<A>) -> Logic<B> {
        fold_logic(self, logic)
    }

    fn fold_question(&mut self, quest: &Expr<A>) -> Expr<B> {
        fold_question(self, quest)
    }

    fn fold_call(&mut self, call: &Call<A>) -> Call<B> {
        fold_call(self, call)
    }

    fn fold_lit(&mut self, lit: &Literal<A>) -> Literal<B> {
        fold_lit(self, lit)
    }

    fn fold_type(&mut self, typ: &Type<A>) -> Type<B> {
        fold_type(self, typ)
    }

    fn fold_forall(&mut self, forall: &Forall<A>) -> Forall<B> {
        fold_forall(self, forall)
    }

    fn fold_constraint(&mut self, constr: &Constraint<A>) -> Constraint<B> {
        fold_constraint(self, constr)
    }
}

pub fn walk_body<A, B, F>(folder: &mut F, body: &Expr<A>) -> Expr<B>
where
    F: Folder<A, B>,
{
    match body.borrow() {
        Expr::Block(block) => Expr::Block(folder.fold_block(block)),
        _ => unreachable!("Only blocks should be bodies."),
    }
}

pub fn fold_module<F, A, B>(visitor: &mut F, module: &Module<A>) -> Module<B>
where
    F: Folder<A, B>,
{
    let mut definitions = Vec::with_capacity(module.definitions.len());
    let mut funcs = Vec::with_capacity(module.definitions.len());

    for def in &module.definitions {
        match def {
            Def::Sign(sign) => definitions.push(Def::Sign(visitor.fold_sign(sign))),
            Def::Func(func) => funcs.push(func),
        }
    }

    for func in funcs.drain(..) {
        definitions.push(Def::Func(visitor.fold_func(func)))
    }

    Module { definitions }
}

pub fn fold_func<F, A, B>(visitor: &mut F, func: &Func<A>) -> Func<B>
where
    F: Folder<A, B>,
{
    Func {
        name: visitor.fold_name(&func.name),
        args: func.args.iter().map(|a| visitor.fold_name(a)).collect(),
        body: walk_body(visitor, &func.body),
    }
}

pub fn fold_sign<F, A, B>(visitor: &mut F, sign: &Sign<A>) -> Sign<B>
where
    F: Folder<A, B>,
{
    Sign {
        name: visitor.fold_name(&sign.name),
        type_: visitor.fold_type(&sign.type_),
    }
}

pub fn fold_stmt<F, A, B>(visitor: &mut F, stmt: &Stmt<A>) -> Stmt<B>
where
    F: Folder<A, B>,
{
    match stmt {
        Stmt::Expr(expr, comma) => Stmt::Expr(visitor.fold_expr(expr), *comma),
        Stmt::Let(let_) => Stmt::Let(visitor.fold_let(let_)),
        Stmt::While(while_) => Stmt::While(visitor.fold_while(while_)),
        Stmt::Until(until) => Stmt::Until(visitor.fold_until(until)),
        Stmt::For(for_) => Stmt::For(visitor.fold_for(for_)),
        Stmt::Assign(assign) => Stmt::Assign(visitor.fold_assign(assign)),
    }
}

pub fn fold_let<F, A, B>(visitor: &mut F, let_: &Let<A>) -> Let<B>
where
    F: Folder<A, B>,
{
    let folded_value = let_.value.as_ref().map(|val| visitor.fold_expr(val));

    Let {
        ident: visitor.fold_name(&let_.ident),
        typ: let_.typ.as_ref().map(|typ| visitor.fold_type(typ)),
        value: folded_value,
    }
}

pub fn fold_while<F, A, B>(visitor: &mut F, while_: &While<A>) -> While<B>
where
    F: Folder<A, B>,
{
    While {
        cond: visitor.fold_expr(&while_.cond),
        expr: walk_body(visitor, &while_.expr),
    }
}

pub fn fold_until<F, A, B>(visitor: &mut F, until: &While<A>) -> While<B>
where
    F: Folder<A, B>,
{
    visitor.fold_while(until)
}

pub fn fold_for<F, A, B>(visitor: &mut F, for_: &For<A>) -> For<B>
where
    F: Folder<A, B>,
{
    let folded_iter = visitor.fold_expr(&for_.iter);
    let folded_value = visitor.fold_name(&for_.value);

    For {
        value: folded_value,
        iter: folded_iter,
        expr: walk_body(visitor, &for_.expr),
    }
}

pub fn fold_assign<F, A, B>(visitor: &mut F, assign: &Assign<A>) -> Assign<B>
where
    F: Folder<A, B>,
{
    let folded_expr = visitor.fold_expr(&assign.expr);

    Assign {
        to: visitor.fold_name(&assign.to),
        expr: folded_expr,
    }
}

pub fn fold_expr<F, A, B>(visitor: &mut F, expr: &Expr<A>) -> Expr<B>
where
    F: Folder<A, B>,
{
    match expr {
        Expr::Term(term) => Expr::Term(visitor.fold_term(term)),
        Expr::Parens(expr) => Expr::Parens(box visitor.fold_expr(expr)),
        Expr::Block(block) => Expr::Block(visitor.fold_block(block)),
        Expr::If(if_) => Expr::If(visitor.fold_if(if_)),
        Expr::Unless(unless) => Expr::Unless(visitor.fold_unless(unless)),
        Expr::Match(match_) => Expr::Match(visitor.fold_match(match_)),
        Expr::Arith(arith) => Expr::Arith(visitor.fold_arith(arith)),
        Expr::Logic(logic) => Expr::Logic(visitor.fold_logic(logic)),
        Expr::Question(quest) => Expr::Question(box visitor.fold_question(quest)),
        Expr::Call(call) => Expr::Call(visitor.fold_call(call)),
    }
}

pub fn fold_term<F, A, B>(visitor: &mut F, term: &Term<A>) -> Term<B>
where
    F: Folder<A, B>,
{
    match term {
        Term::Var(name) => Term::Var(visitor.fold_name(name)),
        Term::Lit(lit) => Term::Lit(visitor.fold_lit(lit)),
        Term::Field { owner, field } => {
            let folded_owner = visitor.fold_expr(owner);

            Term::Field {
                owner: box folded_owner,
                field: visitor.fold_name(field),
            }
        }
        Term::Index { owner, index } => {
            let folded_owner = visitor.fold_expr(owner);
            let folded_index = visitor.fold_expr(index);

            Term::Index {
                owner: box folded_owner,
                index: box folded_index,
            }
        }
    }
}

pub fn fold_block<F, A, B>(visitor: &mut F, block: &[Stmt<A>]) -> Vec<Stmt<B>>
where
    F: Folder<A, B>,
{
    block.iter().map(|stmt| visitor.fold_stmt(stmt)).collect()
}

pub fn fold_if<F, A, B>(visitor: &mut F, if_: &IfExpr<A>) -> IfExpr<B>
where
    F: Folder<A, B>,
{
    match if_ {
        IfExpr::If {
            cond,
            expr,
            else_expr,
        } => {
            let folded_cond = visitor.fold_expr(cond);

            IfExpr::If {
                cond: box folded_cond,
                expr: box walk_body(visitor, &expr),
                else_expr: else_expr.as_ref().map(|ex| box walk_body(visitor, ex)),
            }
        }
        IfExpr::IfLet {
            assign,
            to,
            expr,
            else_expr,
        } => {
            let folded_assign = visitor.fold_expr(assign);
            let folded_to = visitor.fold_name(to);

            IfExpr::IfLet {
                assign: box folded_assign,
                to: folded_to,
                expr: box walk_body(visitor, expr),
                else_expr: else_expr.as_ref().map(|ex| box walk_body(visitor, ex)),
            }
        }
    }
}

pub fn fold_unless<F, A, B>(visitor: &mut F, unless: &IfExpr<A>) -> IfExpr<B>
where
    F: Folder<A, B>,
{
    visitor.fold_if(unless)
}

pub fn fold_match<F, A, B>(visitor: &mut F, match_: &MatchExpr<A>) -> MatchExpr<B>
where
    F: Folder<A, B>,
{
    let folded_expr = visitor.fold_expr(&match_.expr);

    MatchExpr {
        expr: box folded_expr,
        branches: match_
            .branches
            .iter()
            .map(|br| visitor.fold_match_branch(br))
            .collect(),
    }
}

pub fn fold_match_branch<F, A, B>(visitor: &mut F, branch: &MatchBranch<A>) -> MatchBranch<B>
where
    F: Folder<A, B>,
{
    let folded_case = visitor.fold_match_case(&branch.case);
    MatchBranch {
        case: folded_case,
        expr: box visitor.fold_expr(&branch.expr),
    }
}

pub fn fold_match_case<F, A, B>(visitor: &mut F, case: &MatchCase<A>) -> MatchCase<B>
where
    F: Folder<A, B>,
{
    match case {
        MatchCase::Term(term) => MatchCase::Term(visitor.fold_name(term)),
        MatchCase::OneOf(cases) => {
            MatchCase::OneOf(cases.iter().map(|c| visitor.fold_match_case(c)).collect())
        }
        MatchCase::If { case, cond } => {
            let folded_case = visitor.fold_match_case(case);

            MatchCase::If {
                case: box folded_case,
                cond: box visitor.fold_expr(cond),
            }
        }
    }
}

pub fn fold_arith<F, A, B>(visitor: &mut F, arith: &Oper<ArithOp, A>) -> Oper<ArithOp, B>
where
    F: Folder<A, B>,
{
    let folded_lhs = visitor.fold_expr(&arith.lhs);
    let folded_rhs = visitor.fold_expr(&arith.rhs);

    Oper {
        op: arith.op,
        lhs: box folded_lhs,
        rhs: box folded_rhs,
    }
}

pub fn fold_logic<F, A, B>(visitor: &mut F, logic: &Logic<A>) -> Logic<B>
where
    F: Folder<A, B>,
{
    match logic {
        Logic::Not(logic) => Logic::Not(box visitor.fold_expr(logic)),
        Logic::Oper(oper) => {
            let folded_lhs = visitor.fold_expr(&oper.lhs);
            let folded_rhs = visitor.fold_expr(&oper.rhs);

            Logic::Oper(Oper {
                op: oper.op,
                lhs: box folded_lhs,
                rhs: box folded_rhs,
            })
        }
    }
}

pub fn fold_question<F, A, B>(visitor: &mut F, quest: &Expr<A>) -> Expr<B>
where
    F: Folder<A, B>,
{
    visitor.fold_expr(quest)
}

pub fn fold_call<F, A, B>(visitor: &mut F, call: &Call<A>) -> Call<B>
where
    F: Folder<A, B>,
{
    let folded_func = visitor.fold_expr(&call.func);

    Call {
        func: box folded_func,
        args: call.args.iter().map(|a| visitor.fold_expr(a)).collect(),
    }
}

pub fn fold_lit<F, A, B>(visitor: &mut F, lit: &Literal<A>) -> Literal<B>
where
    F: Folder<A, B>,
{
    match lit {
        Literal::Vec(vec) => Literal::Vec(vec.iter().map(|i| visitor.fold_expr(i)).collect()),
        Literal::Map(map) => Literal::Map(
            map.iter()
                .map(|(k, v)| {
                    let folded_value = visitor.fold_expr(v);
                    let folded_key = visitor.fold_expr(k);

                    (folded_key, folded_value)
                })
                .collect(),
        ),
        Literal::String(str) => Literal::String(str.clone()),
        Literal::Integer(i) => Literal::Integer(*i),
        Literal::Natural(n) => Literal::Natural(*n),
        Literal::Float(f) => Literal::Float(*f),
        Literal::Char(ch) => Literal::Char(*ch),
    }
}

pub fn fold_type<F, A, B>(visitor: &mut F, typ: &Type<A>) -> Type<B>
where
    F: Folder<A, B>,
{
    match typ {
        Type::Term(typ_name) => Type::Term(visitor.fold_type_name(typ_name)),
        Type::Var(typ_var) => Type::Var(visitor.fold_type_var(typ_var)),
        Type::App(base, args) => {
            let folded_base = visitor.fold_type(base);
            let folded_args = args.iter().map(|a| visitor.fold_type(a)).collect();

            Type::App(box folded_base, folded_args)
        }
        Type::Arrow(arg, ret) => {
            let folded_arg = arg.as_ref().map(|a| box visitor.fold_type(a));
            let folded_ret = visitor.fold_type(ret);

            Type::Arrow(folded_arg, box folded_ret)
        }
        Type::Method(typ) => Type::Method(box visitor.fold_type(typ)),
        Type::Parens(typ) => Type::Parens(box visitor.fold_type(typ)),
        Type::Forall(forall) => Type::Forall(visitor.fold_forall(forall)),
    }
}

pub fn fold_forall<F, A, B>(visitor: &mut F, forall: &Forall<A>) -> Forall<B>
where
    F: Folder<A, B>,
{
    let folded_vars = forall
        .vars
        .iter()
        .map(|v| visitor.fold_type_var(v))
        .collect();
    let folded_constraint = forall
        .constraint
        .as_ref()
        .map(|c| visitor.fold_constraint(&c));

    Forall {
        vars: folded_vars,
        constraint: folded_constraint,
        base: box visitor.fold_type(&forall.base),
    }
}

pub fn fold_constraint<F, A, B>(visitor: &mut F, constr: &Constraint<A>) -> Constraint<B>
where
    F: Folder<A, B>,
{
    match constr {
        Constraint::Parens(constrs) => {
            Constraint::Parens(constrs.iter().map(|c| visitor.fold_constraint(c)).collect())
        }
        Constraint::Constraint(name, args) => {
            let folded_args = args.iter().map(|a| visitor.fold_type(a)).collect();

            Constraint::Constraint(visitor.fold_type_name(name), folded_args)
        }
    }
}
