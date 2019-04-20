use crate::cst::*;

pub trait Visitor<'c>: Sized {
    fn visit_module<'a, N>(&mut self, module: &'c Module<N>) {
        visit_module(self, module);
    }
    fn visit_func<'a, N>(&mut self, func: &'c Func<N>) {
        visit_func(self, func);
    }
    fn visit_sign<'a, N>(&mut self, sign: &'c Sign<N>) {
        visit_sign(self, sign);
    }

    fn visit_stmt<'a, N>(&mut self, stmt: &'c Stmt<N>) {
        visit_stmt(self, stmt);
    }
    fn visit_let<'a, N>(&mut self, let_: &'c Let<N>) {
        visit_let(self, let_);
    }
    fn visit_while<'a, N>(&mut self, while_: &'c While<N>) {
        visit_while(self, while_);
    }
    fn visit_until<'a, N>(&mut self, until: &'c While<N>) {
        visit_until(self, until);
    }
    fn visit_for<'a, N>(&mut self, for_: &'c For<N>) {
        visit_for(self, for_);
    }
    fn visit_assign<'a, N>(&mut self, assign: &'c Assign<N>) {
        visit_assign(self, assign);
    }

    fn visit_expr<'a, N>(&mut self, expr: &'c Expr<N>) {
        visit_expr(self, expr);
    }
    fn visit_term<'a, N>(&mut self, term: &'c Term<N>) {
        visit_term(self, term);
    }
    fn visit_block<'a, N>(&mut self, block: &'c [Stmt<N>]) {
        visit_block(self, block);
    }
    fn visit_if<'a, N>(&mut self, if_: &'c IfExpr<N>) {
        visit_if(self, if_);
    }
    fn visit_unless<'a, N>(&mut self, unless: &'c IfExpr<N>) {
        visit_unless(self, unless);
    }
    fn visit_match<'a, N>(&mut self, match_: &'c MatchExpr<N>) {
        visit_match(self, match_);
    }
    fn visit_match_branch<'a, N>(&mut self, branch: &'c MatchBranch<N>) {
        visit_match_branch(self, branch);
    }
    fn visit_match_case<'a, N>(&mut self, case: &'c MatchCase<N>) {
        visit_match_case(self, case);
    }
    fn visit_arith<'a, N>(&mut self, arith: &'c Oper<ArithOp, N>) {
        visit_arith(self, arith);
    }
    fn visit_logic<'a, N>(&mut self, logic: &'c Logic<N>) {
        visit_logic(self, logic);
    }
    fn visit_question<'a, N>(&mut self, quest: &'c Expr<N>) {
        visit_question(self, quest);
    }
    fn visit_call<'a, N>(&mut self, call: &'c Call<N>) {
        visit_call(self, call);
    }

    fn visit_lit<'a, N>(&mut self, lit: &'c Literal<N>) {
        visit_lit(self, lit);
    }

    fn visit_name<N>(&mut self, name: &'c N) {
        // Nothing to do.
    }

    fn visit_type<'a, N>(&mut self, typ: &'c Type<N>) {
        visit_type(self, typ);
    }
    fn visit_type_name<N>(&mut self, typ_name: &'c N) {
        // Nothing to do.
    }
    fn visit_type_var<N>(&mut self, typ_var: &'c N) {
        // Nothing to do.
    }
    fn visit_forall<'a, N>(&mut self, forall: &'c Forall<N>) {
        visit_forall(self, forall);
    }
    fn visit_constraint<'a, N>(&mut self, constr: &'c Constraint<N>) {
        visit_constraint(self, constr);
    }
}

pub fn visit_module<'c, V, N>(visitor: &mut V, module: &'c Module<N>)
where
    V: Visitor<'c>,
{
    let mut funcs = Vec::with_capacity(module.definitions.len());

    for def in &module.definitions {
        match def {
            Def::Func(ref func) => funcs.push(func),
            Def::Sign(ref sign) => visitor.visit_sign(sign),
        }
    }

    for func in funcs.drain(..) {
        visitor.visit_func(func);
    }
}

pub fn visit_func<'c, V, N>(visitor: &mut V, func: &'c Func<N>)
where
    V: Visitor<'c>,
{
    visitor.visit_name(&func.name);

    for arg in &func.args {
        visitor.visit_name(arg);
    }

    match func.body {
        Expr::Block(ref block) => visitor.visit_block(block),
        _ => unreachable!("Only block expressions should form the body of a function."),
    }
}

pub fn visit_sign<'c, V, N>(visitor: &mut V, sign: &'c Sign<N>)
where
    V: Visitor<'c>,
{
    visitor.visit_name(&sign.name);
    visitor.visit_type(&sign.type_);
}

pub fn visit_stmt<'c, V, N>(visitor: &mut V, stmt: &'c Stmt<N>)
where
    V: Visitor<'c>,
{
    match stmt {
        Stmt::Expr(expr, _) => visitor.visit_expr(expr),
        Stmt::Let(let_) => visitor.visit_let(let_),
        Stmt::While(while_) => visitor.visit_while(while_),
        Stmt::Until(until_) => visitor.visit_until(until_),
        Stmt::For(for_) => visitor.visit_for(for_),
        Stmt::Assign(assign) => visitor.visit_assign(assign),
    }
}

pub fn visit_let<'c, V, N>(visitor: &mut V, let_: &'c Let<N>)
where
    V: Visitor<'c>,
{
    if let Some(value) = &let_.value {
        visitor.visit_expr(value);
    }

    visitor.visit_name(&let_.ident);

    if let Some(ref typ) = &let_.typ {
        visitor.visit_type(typ);
    }
}

pub fn visit_while<'c, V, N>(visitor: &mut V, while_: &'c While<N>)
where
    V: Visitor<'c>,
{
    visitor.visit_expr(&while_.cond);

    match &while_.expr {
        Expr::Block(block) => visitor.visit_block(block),
        _ => unreachable!("Only block expressions are allowed in while statements"),
    }
}

pub fn visit_until<'c, V, N>(visitor: &mut V, until: &'c While<N>)
where
    V: Visitor<'c>,
{
}

pub fn visit_for<'c, V, N>(visitor: &mut V, for_: &'c For<N>)
where
    V: Visitor<'c>,
{
    visitor.visit_expr(&for_.iter);
    visitor.visit_name(&for_.value);

    match &for_.expr {
        Expr::Block(block) => visitor.visit_block(block),
        _ => unreachable!("Only block expressions are allowed in for statements"),
    }
}

pub fn visit_assign<'c, V, N>(visitor: &mut V, assign: &'c Assign<N>)
where
    V: Visitor<'c>,
{
    visitor.visit_name(&assign.to);
    visitor.visit_expr(&assign.expr);
}

pub fn visit_expr<'c, V, N>(visitor: &mut V, expr: &'c Expr<N>)
where
    V: Visitor<'c>,
{
    match expr {
        Expr::Term(term) => visitor.visit_term(term),
        Expr::Parens(expr) => visitor.visit_expr(expr),
        Expr::Block(block) => visitor.visit_block(block),
        Expr::If(if_) => visitor.visit_if(if_),
        Expr::Unless(unless) => visitor.visit_unless(unless),
        Expr::Match(match_) => visitor.visit_match(match_),
        Expr::Arith(arith) => visitor.visit_arith(arith),
        Expr::Logic(logic) => visitor.visit_logic(logic),
        Expr::Question(quest) => visitor.visit_question(quest),
        Expr::Call(call) => visitor.visit_call(call),
    }
}

pub fn visit_term<'c, V, N>(visitor: &mut V, term: &'c Term<N>)
where
    V: Visitor<'c>,
{
    match term {
        Term::Var(name) => visitor.visit_name(name),
        Term::Lit(_) => (),
        Term::Field { owner, field } => {
            visitor.visit_expr(owner);
            visitor.visit_name(field);
        }
        Term::Index { owner, index } => {
            visitor.visit_expr(owner);
            visitor.visit_expr(index);
        }
    }
}

pub fn visit_block<'c, V, N>(visitor: &mut V, block: &'c [Stmt<N>])
where
    V: Visitor<'c>,
{
    for stmt in block {
        visitor.visit_stmt(stmt);
    }
}

pub fn visit_if<'c, V, N>(visitor: &mut V, if_: &'c IfExpr<N>)
where
    V: Visitor<'c>,
{
    match if_ {
        IfExpr::If {
            cond,
            expr,
            else_expr,
        } => {
            visitor.visit_expr(cond);
            visitor.visit_expr(expr);

            if let Some(else_) = else_expr {
                visitor.visit_expr(else_);
            }
        }
        IfExpr::IfLet {
            assign,
            to,
            expr,
            else_expr,
        } => {
            visitor.visit_expr(assign);
            visitor.visit_name(to);
            visitor.visit_expr(expr);

            if let Some(else_) = else_expr {
                visitor.visit_expr(else_);
            }
        }
    }
}

pub fn visit_unless<'c, V, N>(visitor: &mut V, unless: &'c IfExpr<N>)
where
    V: Visitor<'c>
{
    visit_if(visitor, unless);
}

pub fn visit_match<'c, V, N>(visitor: &mut V, match_: &'c MatchExpr<N>)
where
    V: Visitor<'c>,
{
    visitor.visit_expr(&match_.expr);

    for branch in &match_.branches {
        visitor.visit_match_branch(branch);
    }
}

pub fn visit_match_branch<'c, V, N>(visitor: &mut V, branch: &'c MatchBranch<N>)
where
    V: Visitor<'c>,
{
    visitor.visit_match_case(&branch.case);
    visitor.visit_expr(&branch.expr);
}

pub fn visit_match_case<'c, V, N>(visitor: &mut V, case: &'c MatchCase<N>)
where
    V: Visitor<'c>,
{
    match case {
        MatchCase::Term(term) => visitor.visit_name(term),
        MatchCase::OneOf(cases) => {
            for case in cases {
                visitor.visit_match_case(case);
            }
        }
        MatchCase::If {case, cond} => {
            visitor.visit_expr(cond);
            visitor.visit_match_case(case);
        }
    }
}

pub fn visit_arith<'c, V, N>(visitor: &mut V, arith: &'c Oper<ArithOp, N>)
where
    V: Visitor<'c>,
{
    visitor.visit_expr(&arith.lhs);
    visitor.visit_expr(&arith.rhs);
}

pub fn visit_logic<'c, V, N>(visitor: &mut V, logic: &'c Logic<N>)
where
    V: Visitor<'c>,
{
    match logic {
        Logic::Not(expr) => visitor.visit_expr(expr),
        Logic::Oper(oper) => {
            visitor.visit_expr(&oper.lhs);
            visitor.visit_expr(&oper.rhs);
        }
    }
}

pub fn visit_question<'c, V, N>(visitor: &mut V, quest: &'c Expr<N>)
where
    V: Visitor<'c>,
{
    visitor.visit_expr(quest);
}

pub fn visit_call<'c, V, N>(visitor: &mut V, call: &'c Call<N>)
where
    V: Visitor<'c>,
{
    visitor.visit_expr(&call.func);

    for arg in &call.args {
        visitor.visit_expr(arg);
    }
}

pub fn visit_lit<'c, V, N>(visitor: &mut V, lit: &'c Literal<N>)
where
    V: Visitor<'c>,
{
    match lit {
        Literal::Vec(vec) => {
            for expr in vec {
                visitor.visit_expr(expr);
            }
        }
        Literal::Map(map) => {
            for (key, value) in map {
                visitor.visit_expr(value);
                visitor.visit_expr(key);
            }
        }
        _ => ()
    }
}

pub fn visit_type<'c, V, N>(visitor: &mut V, typ: &'c Type<N>)
where
    V: Visitor<'c>,
{
    match typ {
        Type::Term(term) => visitor.visit_type_name(term),
        Type::Var(var) => visitor.visit_type_var(var),
        Type::App(base, args) => {
            visitor.visit_type(base);

            for arg in args {
                visitor.visit_type(arg);
            }
        }
        Type::Arrow(arg, ret) => {
            visitor.visit_type(arg);
            visitor.visit_type(ret);
        }
        Type::Method(ret) => visitor.visit_type(ret),
        Type::Parens(typ) => visitor.visit_type(typ),
        Type::Forall(forall) => visitor.visit_forall(forall),
    }
}

pub fn visit_forall<'c, V, N>(visitor: &mut V, forall: &'c Forall<N>)
where
    V: Visitor<'c>,
{
    for var in &forall.vars {
        visitor.visit_type_var(var);
    }

    if let Some(constr) = &forall.constraint {
        visitor.visit_constraint(constr);
    }

    visitor.visit_type(&forall.base);
}

pub fn visit_constraint<'c, V, N>(visitor: &mut V, constr: &'c Constraint<N>)
where
    V: Visitor<'c>,
{
    match constr {
        Constraint::Constraint(name, args) => {
            visitor.visit_type_name(name);

            for arg in args {
                visitor.visit_type(arg);
            }
        }
        Constraint::Parens(constrs) => {
            for constr in constrs {
                visitor.visit_constraint(constr);
            }
        }
    }
}
