///! CST desugaring to an AST.
///!
///! As CST and AST are very similar, the desugaring process can be interpreted
///! as a simple folding of the CST. While most of the work is a simple translation,
///! some steps are bit more involved.
///!
///! First of all, we need to make sure we consume each signature (that are eliminated,
///! in the AST) before starting consuming function definitions. This make each function
///! available in all the file, independent of where it is defined.
///!
///! Second, we need to make sure each symbol name is correctly translated to it ID,
///! while separating identifiers and types namespaces. To prevent collisions, all the
///! IDs come from the same source. Names that aren't defined are replaced by the ID of
///! a special identifier defined by `NAME_ERROR`. This is important for type checking,
///! where this symbol have the particular `Any` type.
///!
///! Currently the desugaring is limited to what error handling it can do, as the CST
///! doesn't have spans information. After we refactor the CST to include that information,
///! we can do some nice things when we found an undefined name, e.g. search for the
///! closest most similar name in the scope, and suggesting it to the user.
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;
use symbol::pos::{Located, Location, Span};
use symbol::{BltInTy, Definition, SymGen, SymTable, Symbol, Table, Type};
use syntax::cst;

use crate::ast;
use crate::scope::Scope;

type ScpSymTbl = Scope<SymTable>;

const NAME_ERROR: &str = "##INOXC__NAME_ERROR__";

#[derive(Debug, Clone)]
pub enum DesugarError {
    RedefiningBuiltIn { span: Span<Location>, builtin: String },
    RedefiningFunction { span: Span<Location>, function: String, defined: Span<Location> },
    UnknownName { span: Span<Location>, name: String },
    AssignToImmutable { span: Span<Location>, name: String, defined: Span<Location> },
    InvalidType { span: Span<Location> },
    UninitializedUsage { span: Span<Location>, name: String, defined: Span<Location> },
    UnusedName { name: String, defined: Span<Location> },
    MissingMain,
}

/// A desugaring session.
///
/// Lowers a CST to an AST, while translating identifiers to symbols
/// and removing explicitly written types.
///
/// Unknown identifiers, i.e. not yet defined, are replaced by the symbol
/// of the `NAME_ERROR` constant.
pub struct Desugar<'g> {
    /// The symbol generator used through the desugaring process.
    gen: &'g SymGen,
    /// Currently known names. Scoped to prevent collisions due to shadowing.
    known_names: Rc<ScpSymTbl>,
    /// Set of unknown names.
    undefined_names: BTreeSet<Located<String>>,
    /// Known type names.
    ///
    /// As types are a global concept, and the user can't define types
    /// only in some scope, we don't need to scope the table.
    type_names: SymTable,
    /// All names defined till now.
    names: Table<Definition>,
    /// Next index to be used when creating an infer type.
    next_infer: u32,
    /// Symbol used when the desugar encounters an unknown identifier.
    error_sym: Symbol,
    /// Current span of the desugaring.
    cur_loc: Span<Location>,
    /// Built-in functions.
    builtins: BTreeSet<String>,
    /// List of occurred errors.
    errors: Vec<DesugarError>
}

impl<'g> Desugar<'g> {
    pub fn new(gen: &'g SymGen) -> Self {
        let base_table = Scope::new(SymTable::default());
        let cur_loc = Span::new(Location::default(), Location::default());
        let error_sym = base_table.define(gen.new_symbol(), NAME_ERROR, Type::Error, cur_loc);

        Self {
            gen,
            known_names: base_table,
            undefined_names: BTreeSet::default(),
            type_names: SymTable::default(),
            names: Table::default(),
            next_infer: 0,
            error_sym,
            cur_loc,
            builtins: BTreeSet::default(),
            errors: Vec::default()
        }
    }

    pub fn desugar(self, sugar: &cst::Module<String>) -> Result<ast::Module, Vec<DesugarError>> {
        self.desugar_module(sugar)
    }

    fn with_scope<F, B>(&mut self, f: F) -> B
    where
        F: FnOnce(&mut Self) -> B,
    {
        use std::mem;

        let old_name_scope = Rc::clone(&self.known_names);
        self.known_names = Rc::clone(&old_name_scope).new_child();

        let result = f(self);

        let scoped_names = mem::replace(&mut self.known_names, old_name_scope);

        let scoped_names = Rc::try_unwrap(scoped_names)
            .expect("The bottom scope shouldn't have more than one reference")
            .scoped_names();
        for (sym, def) in scoped_names.into_iter() {
            self.names.insert(sym, def);
        }

        result
    }

    fn new_name(&mut self, name: &str, typ: Option<Type>) -> Symbol {
        let typ = typ.unwrap_or(Type::Infer);
        let name_sym = self.gen.new_symbol();
        self.known_names.define(name_sym, name, typ, self.cur_loc);

        name_sym
    }

    fn define_builtins(&mut self) {
        use std::ops::Deref;
        macro_rules! builtin {
            ($name:expr, $typ:expr) => {
                {
                    let name_sym = self.gen.new_symbol();
                    self.known_names.define(name_sym, $name, $typ, self.cur_loc);
                    self.builtins.insert($name.to_string());
                }
            }
        }

        builtin!("read", Type::Fn(vec![], box Type::BuiltIn(BltInTy::Str)));
        builtin!("print", Type::Fn(vec![Type::BuiltIn(BltInTy::Str)], box Type::BuiltIn(BltInTy::Unit)));
        builtin!("show", Type::Fn(vec![Type::Any], box Type::BuiltIn(BltInTy::Str)));
        builtin!("append", Type::Fn(vec![Type::BuiltIn(BltInTy::Str), Type::BuiltIn(BltInTy::Char)], box Type::BuiltIn(BltInTy::Str)));
        builtin!("concat", Type::Fn(vec![Type::BuiltIn(BltInTy::Str), Type::BuiltIn(BltInTy::Str)], box Type::BuiltIn(BltInTy::Str)));

        let names = self.known_names.deref().clone();

        for (sym, def) in names.scoped_names().into_iter() {
            self.names.insert(sym, def);
        }
    }
}

impl Desugar<'_> {
    fn desugar_module(mut self, module: &cst::Module<String>) -> Result<ast::Module, Vec<DesugarError>> {
        self.define_builtins();

        let items = self.with_scope(|des| {
            let mut items = BTreeMap::new();

            // Functions and signatures are defined in pairs.
            let mut funcs = Vec::with_capacity(module.definitions.len() / 2);

            // Walk over all the signatures before walking over the functions.
            // This way, the functions are available to use anywhere in the file.
            for def in &module.definitions {
                des.cur_loc = def.span;

                match &def.value {
                    cst::DefNode::Sign(sign) => des.desugar_sign(sign),
                    cst::DefNode::Func(func) => {
                        // Only builtin functions are present in this step.
                        // We should ignore any re-declaration of them.
                        if !des.builtins.contains(&func.name.value) {
                            funcs.push((def.span, func))
                        }
                    }
                }
            }

            for (sp, func) in funcs.drain(..) {
                des.cur_loc = sp;
                if let Some(func) = des.desugar_func(&func) {
                    items.insert(func.sym, ast::Item::Fn(func));
                }
            }

            items
        });

        let mut has_main = false;
        for def in self.names.values() {
            if !def.referenced
                && !self.builtins.contains(&def.name)
                    && def.name.as_str() != "main"
                    && def.name.as_str() != NAME_ERROR {
                self.errors.push(DesugarError::UnusedName{
                    name: def.name.clone(),
                    defined: def.location
                });
            } else if !has_main && def.name.as_str() == "main" {
                has_main = true;
            }
        }

        if !has_main {
            self.errors.push(DesugarError::MissingMain);
        }

        if self.errors.is_empty() {
            Ok(ast::Module {
                items,
                names: self.names,
            })
        } else {
            Err(self.errors)
        }

    }

    fn desugar_sign(&mut self, sign: &cst::Sign<String>) {
        if let Some(sym) = self.known_names.search(&sign.name) {
            if self.builtins.contains(&sign.name.value) {
                self.errors.push(DesugarError::RedefiningBuiltIn{
                    span: sign.name.span,
                    builtin: sign.name.value.clone()
                });
            } else {
                let def = self.known_names.definition(sym).unwrap();

                self.errors.push(DesugarError::RedefiningFunction{
                    span: sign.name.span,
                    function: sign.name.value.clone(),
                    defined: def.location,
                });
            }
        } else {
            let ty = self.desugar_ty(&sign.type_);
            self.new_name(&sign.name, Some(ty));
        }
    }

    fn desugar_func(&mut self, func: &cst::Func<String>) -> Option<ast::Func> {
        self.with_scope(move |des| {
            let func_sym = match des.known_names.search(&func.name) {
                Some(sym) => sym,
                None => {
                    // As the desugaring process walks over all the signatures
                    // before walking over the functions, if we don't find the
                    // symbol for a function, it wasn't defined. In this case
                    // we can ignore ignore the desugaring for it
                    des.undefined_names.insert(func.name.clone());
                    des.errors.push(DesugarError::UnknownName{
                        span: func.name.span,
                        name: func.name.value.clone(),
                    });
                    return None;
                }
            };

            // We could define the functions arguments with types to be infered,
            // and unifying the function type with them in the type checking,
            // but is unnecessary as we already known the correct types.
            let func_def = des.known_names.definition(func_sym).unwrap();
            let args = match func_def.typ.clone() {
                Type::Fn(args, _) => func
                    .args
                    .iter()
                    .zip(args.into_iter())
                    .map(|(a, t)| {
                        des.cur_loc = a.span;
                        let a_sym = des.new_name(a, Some(t));
                        let def = des.known_names.definition_mut(a_sym).unwrap();
                        def.initialized = true;

                        a_sym
                    })
                    .collect(),
                Type::Method(recv, args, _) => {
                    // The receiver of the method is always the first argument.
                    let args_ty = std::iter::once(*recv).chain(args.into_iter());

                    func.args
                        .iter()
                        .zip(args_ty)
                        .map(|(a, t)| {
                            des.cur_loc = a.span;
                            let a_sym = des.new_name(a, Some(t));
                            let def = des.known_names.definition_mut(a_sym).unwrap();
                            def.initialized = true;

                            a_sym
                        })
                        .collect()
                }
                _ => return None,
            };

            Some(ast::Func {
                sym: func_sym,
                arguments: args,
                body: des.desugar_block(&func.body),
            })
        })
    }

    fn desugar_block(&mut self, block: &cst::ExprNode<String>) -> ast::Block {
        self.with_scope(move |dsg| {
            let block = match block {
                cst::ExprNode::Block(block) => block,
                _ => unreachable!("Only ExprNode::Block should be blocks"),
            };

            if block.is_empty() {
                return ast::Block {
                    stmts: vec![],
                    last: None,
                };
            }

            let mut stmts: Vec<_> = block
                .iter()
                .take(block.len() - 1)
                .map(|stmt| dsg.desugar_stmt(stmt))
                .collect();

            let last = block.last().and_then(|l| match &l.value {
                cst::StmtNode::Expr(expr, false) => Some(box dsg.desugar_expr(expr)),
                stmt => {
                    stmts.push(dsg.desugar_stmt(&stmt));
                    None
                }
            });

            ast::Block { stmts, last }
        })
    }

    fn desugar_stmt(&mut self, stmt: &cst::StmtNode<String>) -> ast::Stmt {
        ast::Stmt {
            node: match stmt {
                cst::StmtNode::Expr(expr, _) => ast::StmtKind::Expr(self.desugar_expr(expr)),
                cst::StmtNode::Let(let_) => ast::StmtKind::Let(self.desugar_let(let_)),
                cst::StmtNode::While(whl) => ast::StmtKind::While(self.desugar_while(whl)),
                cst::StmtNode::Until(utl) => {
                    let whl = self.desugar_while(utl);

                    ast::StmtKind::While(ast::While {
                        cond: ast::Expr {
                            node: ast::ExprKind::Logic(ast::Logic::Not(box whl.cond)),
                        },
                        body: whl.body,
                    })
                }
                cst::StmtNode::For(fr) => ast::StmtKind::For(self.desugar_for(fr)),
                cst::StmtNode::Assign(asgn) => ast::StmtKind::Assign(self.desugar_assign(asgn)),
            },
        }
    }

    fn desugar_let(&mut self, lt: &cst::LetNode<String>) -> ast::Let {
        let let_typ = lt.typ.as_ref().map(|ty| self.desugar_ty(ty));
        let value = lt.value.as_ref().map(|ex| self.desugar_expr(ex));

        self.cur_loc = lt.ident.span;
        let let_sym = self.new_name(&lt.ident, let_typ);

        let lt_def = self.known_names.definition_mut(let_sym).unwrap();
        lt_def.initialized = value.is_some();
        lt_def.mutable = lt.mutable;

        ast::Let {
            ident: let_sym,
            value,
        }
    }

    fn desugar_while(&mut self, whl: &cst::WhileNode<String>) -> ast::While {
        ast::While {
            cond: self.desugar_expr(&whl.cond),
            body: self.desugar_block(&whl.expr),
        }
    }

    fn desugar_for(&mut self, fr: &cst::ForNode<String>) -> ast::For {
        let desugared_iter = self.desugar_expr(&fr.iter);

        self.with_scope(|des| {
            let for_sym = des.new_name(&fr.value, None);
            let def = des.known_names.definition_mut(for_sym).unwrap();
            def.initialized = true;

            ast::For {
                ident: for_sym,
                iter: desugared_iter,
                body: des.desugar_block(&fr.expr),
            }
        })
    }

    fn desugar_assign(&mut self, assign: &cst::AssignNode<String>) -> ast::Assign {
        let desugared_value = self.desugar_expr(&assign.expr);

        let asgn = self.desugar_assign_kind(&assign.to);

        ast::Assign {
            to: asgn,
            value: desugared_value,
        }
    }

    fn desugar_assign_kind(&mut self, to: &cst::Term<String>) -> ast::AssignKind {
        match &to.value {
            cst::TermNode::Var(to) => match self.known_names.search(&to) {
                None => {
                    self.undefined_names.insert(to.clone());
                    self.errors.push(DesugarError::UnknownName{
                        span: to.span,
                        name: to.value.clone()
                    });

                    ast::AssignKind::Sym(self.error_sym)
                }
                Some(asgn_sym) => {
                    let asgn_def = self.known_names.definition_mut(asgn_sym).unwrap();
                    if asgn_def.mutable || !asgn_def.initialized {
                        asgn_def.initialized = true;

                        ast::AssignKind::Sym(asgn_sym)
                    } else {
                        self.errors.push(DesugarError::AssignToImmutable{
                            span: to.span,
                            name: to.value.clone(),
                            defined: asgn_def.location,
                        });
                        ast::AssignKind::Sym(self.error_sym)
                    }
                }
            },
            cst::TermNode::Field { owner, field } => match self.desugar_expr(&owner).node {
                ast::ExprKind::Term(ast::Term::Var(to)) => {
                    ast::AssignKind::Field(to, field.value.clone())
                }
                _ => ast::AssignKind::Field(self.error_sym, field.value.clone()),
            },
            cst::TermNode::Index { owner, index } => {
                let index = self.desugar_expr(&index);
                match self.desugar_expr(&owner).node {
                    ast::ExprKind::Term(ast::Term::Var(asgn)) => {
                        let asgn_def = self.known_names.definition_mut(asgn).unwrap();

                        if !asgn_def.initialized {
                            self.errors.push(DesugarError::UninitializedUsage{
                                span: to.span,
                                name: asgn_def.name.clone(),
                                defined: asgn_def.location,
                            });
                            ast::AssignKind::Sym(self.error_sym)
                        } else if asgn_def.mutable {
                            ast::AssignKind::Index(asgn, index)
                        } else {
                            self.errors.push(DesugarError::AssignToImmutable{
                                span: to.span,
                                name: asgn_def.name.clone(),
                                defined: asgn_def.location,
                            });
                            ast::AssignKind::Sym(self.error_sym)
                        }
                    },
                    _ => ast::AssignKind::Index(self.error_sym, index),
                }
            }
            _ => ast::AssignKind::Sym(self.error_sym),
        }
    }

    fn desugar_expr(&mut self, expr: &cst::ExprNode<String>) -> ast::Expr {
        let node = match expr {
            cst::ExprNode::Term(term) => ast::ExprKind::Term(self.desugar_term(term)),
            cst::ExprNode::Parens(ex) => return self.desugar_expr(ex),
            block @ cst::ExprNode::Block(_) => ast::ExprKind::Block(self.desugar_block(block)),
            cst::ExprNode::If(if_) => ast::ExprKind::If(self.desugar_if(if_)),
            cst::ExprNode::Unless(unl) => {
                let if_ = self.desugar_if(unl);

                ast::ExprKind::If(ast::If {
                    cond: box ast::Expr {
                        node: ast::ExprKind::Logic(ast::Logic::Not(if_.cond)),
                    },
                    ..if_
                })
            }
            cst::ExprNode::Match(mtch) => ast::ExprKind::Match(self.desugar_match(mtch)),
            cst::ExprNode::Arith(arith) => ast::ExprKind::Arith(self.desugar_oper(arith)),
            cst::ExprNode::Logic(logic) => ast::ExprKind::Logic(self.desugar_logic(logic)),
            cst::ExprNode::Question(quest) => ast::ExprKind::Question(box self.desugar_expr(quest)),
            cst::ExprNode::Call(call) => {
                let call = self.desugar_call(call);

                ast::ExprKind::Call(call)
            }
        };

        ast::Expr { node }
    }

    fn desugar_term(&mut self, term: &cst::Term<String>) -> ast::Term {
        match &term.value {
            cst::TermNode::Var(var) => match self.known_names.search(var) {
                None => {
                    self.undefined_names.insert(var.clone());
                    self.errors.push(DesugarError::UnknownName{
                        span: var.span,
                        name: var.value.clone()
                    });

                    ast::Term::Var(self.error_sym)
                }
                Some(var_sym) => {
                    let var_def = self.known_names.definition_mut(var_sym).unwrap();
                    var_def.referenced = true;

                    if var_def.initialized {
                        ast::Term::Var(var_sym)
                    } else {
                        self.errors.push(DesugarError::UninitializedUsage{
                            span: term.span,
                            name: var_def.name.clone(),
                            defined: var_def.location,
                        });

                        ast::Term::Var(self.error_sym)
                    }
                }
            },
            cst::TermNode::Lit(lit) => ast::Term::Lit(match &lit.value {
                cst::LitNode::String(str) => ast::Literal::String(str.clone()),
                cst::LitNode::Integer(i) => ast::Literal::Int(*i),
                cst::LitNode::Char(ch) => ast::Literal::Chr(*ch),
                cst::LitNode::Bool(bl) => ast::Literal::Bool(*bl),
                cst::LitNode::Float(f) => ast::Literal::Flt(*f),
                cst::LitNode::Vec(vec) => {
                    ast::Literal::Vec(vec.iter().map(|e| self.desugar_expr(e)).collect())
                }
                cst::LitNode::Map(map) => ast::Literal::Map(
                    map.iter()
                        .map(|(k, v)| (self.desugar_expr(k), self.desugar_expr(v)))
                        .collect(),
                ),
            }),
            cst::TermNode::Field { owner, field } => ast::Term::Field {
                owner: box self.desugar_expr(owner),
                field: field.to_string(),
            },
            cst::TermNode::Index { owner, index } => ast::Term::Index {
                owner: box self.desugar_expr(owner),
                index: box self.desugar_expr(index),
            },
        }
    }

    fn desugar_if(&mut self, if_: &cst::IfNode<String>) -> ast::If {
        match if_ {
            cst::IfNode::If {
                cond,
                expr,
                else_expr,
            } => ast::If {
                cond: box self.desugar_expr(cond),
                body: self.desugar_block(expr),
                else_body: else_expr.as_ref().map(|b| self.desugar_block(b)),
            },
            _ => unimplemented!("if-let not implemented yet."),
        }
    }

    fn desugar_oper<Op: Copy>(&mut self, oper: &cst::Oper<Op, String>) -> ast::Oper<Op> {
        ast::Oper {
            lhs: box self.desugar_expr(&oper.lhs),
            op: oper.op,
            rhs: box self.desugar_expr(&oper.rhs),
        }
    }

    fn desugar_match(&mut self, mtch: &cst::MatchExpr<String>) -> ast::Match {
        ast::Match {
            expr: box self.desugar_expr(&mtch.expr),
            arms: mtch
                .branches
                .iter()
                .map(|b| self.desugar_match_arm(b))
                .collect(),
        }
    }

    fn desugar_match_arm(&mut self, branch: &cst::MatchBranch<String>) -> ast::MatchArm {
        let case = &branch.value.case;
        match &case.value {
            cst::CaseNode::Term(term) => self.with_scope(move |dsg| {
                dsg.cur_loc = case.span;
                let term_sym = dsg.new_name(term, None);

                ast::MatchArm {
                    case: ast::MatchCase::Term(term_sym),
                    expr: box dsg.desugar_expr(&branch.value.expr),
                }
            }),
            _ => unimplemented!("many cases in one arm or arm guards are not yet implemented."),
        }
    }

    fn desugar_logic(&mut self, logic: &cst::LogicNode<String>) -> ast::Logic {
        match logic {
            cst::LogicNode::Not(log) => ast::Logic::Not(box self.desugar_expr(log)),
            cst::LogicNode::Oper(oper) => ast::Logic::Oper(self.desugar_oper(oper)),
        }
    }

    fn desugar_call(&mut self, call: &cst::Call<String>) -> ast::Call {
        let func = match self.known_names.search(&call.func) {
            None => {
                self.undefined_names.insert(call.func.clone());
                self.errors.push(DesugarError::UnknownName{
                    span: call.func.span,
                    name: call.func.value.clone()
                });

                self.error_sym
            }
            Some(var_sym) => {
                let var_def = self.known_names.definition_mut(var_sym).unwrap();
                var_def.referenced = true;

                var_sym
            }
        };

        ast::Call {
            func,
            args: call.args.iter().map(|a| self.desugar_expr(a)).collect(),
        }
    }

    fn desugar_ty(&mut self, ty: &cst::Type<String>) -> Type {
        self.cur_loc = ty.span;
        match &ty.value {
            cst::TypeNode::Term(term) => {
                self.try_convert_to_builtin(&ty, &[]).unwrap_or_else(|| {
                    self.type_names
                        .get_symbol(term)
                        .map_or_else(|| {
                            self.errors.push(DesugarError::InvalidType {
                                span: ty.span
                            });

                            Type::Error
                        }, Type::Named)
                })
            }
            cst::TypeNode::Parens(ty) => self.desugar_ty(ty),
            cst::TypeNode::App(base, args) => {
                if let Some(ty) = self.try_convert_to_builtin(base, args) {
                    ty
                } else {
                    let des_base = self.desugar_ty(base);
                    let des_args = args.iter().map(|a| self.desugar_ty(a)).collect();

                    Type::Generic(box des_base, des_args)
                }
            }
            // In case of a method arrow, we need to convert it to a function type
            // first, as the format is the same. Only after that we can extract the
            // receiver.
            cst::TypeNode::Method(func) => match self.desugar_ty(func) {
                Type::Fn(mut args, ret) => {
                    let mut args = args.drain(..);
                    // A method have at least one parameter, the receiver.
                    let receiver = args.next().unwrap();

                    Type::Method(box receiver, args.collect(), ret)
                }
                _ => unreachable!("Only functions type are present in methods."),
            },
            cst::TypeNode::Arrow(arg, ret) => {
                let mut ret = ret;
                let args = match arg {
                    // An arrow with no argument, e.g. `main: -> ()`.
                    None => vec![],
                    // In case the arrow have a first argument, we can have nested arrows,
                    // e.g. `foo: vec[int] -> map[int, string] -> vec[string]`.
                    //
                    // Search for all the arguments for the function.
                    Some(ty) => {
                        let mut args = vec![self.desugar_ty(ty)];

                        while let cst::TypeNode::Arrow(arg, new_ret) = &ret.value {
                            // An inner arrow always have an argument.
                            args.push(self.desugar_ty(
                                &arg.as_ref().expect("Inner arrow must have argument"),
                            ));
                            ret = new_ret;
                        }

                        args
                    }
                };

                Type::Fn(args, box self.desugar_ty(ret))
            }
            _ => panic!("Generics not yet implemented."),
        }
    }

    fn try_convert_to_builtin(
        &mut self,
        base: &cst::Type<String>,
        args: &[cst::Type<String>],
    ) -> Option<Type> {
        match &base.value {
            cst::TypeNode::Term(term) => match term.value.as_str() {
                "map" => Some(match args.len() {
                    2 => Type::BuiltIn(BltInTy::Map(
                        box self.desugar_ty(&args[0]),
                        box self.desugar_ty(&args[1]),
                    )),
                    _ => Type::Error,
                }),
                "vec" => Some(match args.len() {
                    1 => Type::BuiltIn(BltInTy::Vec(box self.desugar_ty(&args[0]))),
                    _ => Type::Error,
                }),
                "int" => Some(if args.is_empty() {
                    Type::BuiltIn(BltInTy::Int)
                } else {
                    Type::Error
                }),
                "float" => Some(if args.is_empty() {
                    Type::BuiltIn(BltInTy::Flt)
                } else {
                    Type::Error
                }),
                "string" => Some(if args.is_empty() {
                    Type::BuiltIn(BltInTy::Str)
                } else {
                    Type::Error
                }),
                "unit" => Some(Type::BuiltIn(BltInTy::Unit)),
                "bool" => Some(Type::BuiltIn(BltInTy::Bool)),
                "char" => Some(Type::BuiltIn(BltInTy::Char)),
                _ => None,
            },
            _ => None,
        }
    }
}
