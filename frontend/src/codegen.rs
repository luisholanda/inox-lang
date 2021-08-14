use crate::ast::*;
use proc_macro2::{TokenStream, Ident, Span};
use quote::quote;
use std::collections::HashSet;
use std::iter::FromIterator;
use symbol::{Definition, Symbol, Table, Type, BltInTy};
use syntax::cst::{ArithOp, LogicOp};

pub fn emit(ast: &Module) -> TokenStream {
    let mut codegen = Codegen::new(&ast.names);
    codegen.emit_file_header();

    for item in ast.items.values() {
        codegen.emit_item(item);
    }

    codegen.stream
}

struct Codegen<'a> {
    names: &'a Table<Definition>,
    stream: TokenStream,
}

impl<'a> Codegen<'a> {
    fn new(names: &'a Table<Definition>) -> Self {
        Self { names, stream: TokenStream::new() }
    }

    fn definition(&self, sym: Symbol) -> &'a Definition {
        self.names.get(&sym).unwrap()
    }
}

impl Codegen<'_> {
    fn emit_name(&self, name: &str) -> Ident {
        let mut safe_name = String::from("_");
        safe_name.push_str(name);

        Ident::new(&safe_name, Span::call_site())
    }

    fn emit_file_header(&mut self) {
        self.stream.extend(quote!{
            /// Start of file header
            use std::collections::HashMap;
            use std::fmt::Debug;
            use std::hash::Hash;

            fn main() {
                _main();
            }

            trait InoxIter {
                type Item;

                fn inox_iter(self) -> Vec<Self::Item>;
            }
        });

        self.stream.extend(quote!{
            impl<T> InoxIter for Vec<T> {
                type Item = T;

                fn inox_iter(self) -> Vec<Self::Item> {
                    self
                }
            }
        });

        self.stream.extend(quote!{
            impl<K, V> InoxIter for HashMap<K, V>
            where
                K: Hash + Eq + Clone
            {
                type Item = K;

                fn inox_iter(self) -> Vec<Self::Item> {
                    self.keys().cloned().collect()
                }
            }
        });

        let mut builtins: HashSet<&'static str> = HashSet::from_iter(vec![
            "read",
            "show",
            "append",
            "concat"
        ]);

        macro_rules! include_builtin {
            ($name:expr, $code: expr) => {
                if !builtins.contains($name) {
                    self.stream.extend($code);
                }
            }
        }

        for def in self.names.values() {
            builtins.remove(def.name.as_str());
        }

        include_builtin!("read", quote!{
            fn _read() -> String {
                use std::io;
                use std::io::prelude::*;

                let mut input = String::new();
                io::stdout().flush();
                io::stdin().read_line(&mut input).unwrap_or(0);

                input
            }
        });

        include_builtin!("print", quote!{
            fn _print(text: String) {
                print!("{}", text)
            }
        });

        include_builtin!("show", quote!{
            fn _show(text: impl Debug) -> String {
                format!("{:?}", text)
            }
        });

        include_builtin!("append", quote!{
            fn _append<E, T>(mut e: E, x: T) -> E
                where E: Extend<T>
            {
                e.extend(vec![x]);

                e
            }
        });

        include_builtin!("concat", quote!{
            fn _concat(mut s: String, s2: String) -> String {
                s.push_str(s2.as_str());

                s
            }
        });

        self.stream.extend(quote!{
            ///End of file header
        })
    }
}

impl Codegen<'_> {
    fn emit_item(&mut self, item: &Item) {
        match item {
            Item::Fn(func) => self.emit_func(func)
        }
    }

    fn emit_func(&mut self, func: &Func) {
        let func_header = self.func_header(func.sym, &func.arguments);

        let ret_typ = {
            let def = self.definition(func.sym);

            match &def.typ {
                Type::Fn(_, ret) => (*ret).clone(),
                _ => unreachable!("Function Type must be Fn"),
            }
        };

        let func_body = self.emit_block(&func.body, Some(&ret_typ));

        self.stream.extend(quote! {
            #func_header
            #func_body

        });
    }

    fn func_header(&self, func: Symbol, args: &[Symbol]) -> TokenStream {
        let func_def = self.definition(func);
        let args = args.into_iter()
            .map(|a| self.definition(*a))
            .map(|a| {
                let name = self.emit_name(&a.name);

                if a.mutable {
                    quote! { mut #name }
                } else {
                    quote! { #name }
                }
            });

        let (arg_tps, ret) = match &func_def.typ {
            Type::Fn(args, ret) => {
                (args.iter()
                    .map(|a| self.emit_type(a))
                    .collect::<Vec<_>>(),
                 self.emit_type(&ret))
            },
            _ => unreachable!("Functions should have function type")
        };

        let func_name = self.emit_name(&func_def.name);
        quote! { fn #func_name( #(#args: #arg_tps),* ) -> #ret }
    }

    fn emit_type(&self, typ: &Type) -> TokenStream {
        match typ {
            Type::BuiltIn(blt) => {
                match blt {
                    BltInTy::Int => quote!(i32),
                    BltInTy::Flt => quote!(f32),
                    BltInTy::Str => quote!(String),
                    BltInTy::Char => quote!(char),
                    BltInTy::Bool => quote!(bool),
                    BltInTy::Unit => quote!(()),
                    BltInTy::Vec(item) => {
                        let it = self.emit_type(&item);

                        quote!(Vec<#it>)
                    },
                    BltInTy::Map(key, val) => {
                        let k = self.emit_type(&key);
                        let v = self.emit_type(&val);

                        quote!(HashMap<#k, #v>)
                    }
                }
            },
            _ => TokenStream::new()
        }
    }

    fn emit_block(&self, block: &Block, cast: Option<&Type>) -> TokenStream {
        let stmts: Vec<_> = block.stmts.iter().map(|s| self.emit_stmt(s)).collect();
        let last = block.last.as_ref().map_or_else(TokenStream::new, |l| {
            let l = self.emit_expr(l);

            if let Some(typ) = cast {
                let typ = self.emit_type(&typ);

                quote!(#l as #typ)
            } else {
                l
            }
        });


        quote! {
            {
                #(#stmts
                  )*
                #last
            }
        }
    }

    fn emit_stmt(&self, stmt: &Stmt) -> TokenStream {
        match &stmt.node {
            StmtKind::Expr(expr) => {
                let expr = self.emit_expr(expr);
                quote!(#expr;)
            },
            StmtKind::Let(let_) => {
                let i_def = self.definition(let_.ident);

                let def;
                if i_def.referenced {
                    let i_name = self.emit_name(&i_def.name);
                    let i_typ = self.emit_type(&i_def.typ);

                    def = if i_def.mutable {
                        quote!(let mut #i_name: #i_typ)
                    } else {
                        quote!(let #i_name: #i_typ)
                    };
                } else {
                    // The name isn't used, so there's no need to define it.
                    def = TokenStream::new();
                }

                let val = let_.value.as_ref().map_or_else(TokenStream::new, |v| {
                    let val = self.emit_expr(v);

                    if def.is_empty() {
                        val
                    } else {
                        quote!(= #val)
                    }
                });

                quote!(#def #val;)
            },
            StmtKind::While(whl) => {
                let cond = self.safe_emit_expr(&whl.cond);
                let body = self.emit_block(&whl.body, None);

                quote!(while #cond #body)
            },
            StmtKind::For(for_) => {
                let ident = self.definition(for_.ident);
                let ident_name = self.emit_name(&ident.name);
                let iter = self.emit_expr(&for_.iter);
                let body = self.emit_block(&for_.body, None);

                quote!(for #ident_name in &#iter.clone().inox_iter() #body)
            },
            StmtKind::Assign(asn) => {
                let val = self.safe_emit_expr(&asn.value);

                match &asn.to {
                    AssignKind::Sym(sym) => {
                        let ident = self.definition(*sym);
                        let ident_name = self.emit_name(&ident.name);

                        quote!(#ident_name = #val;)
                    },
                    AssignKind::Index(sym, idx) => {
                        let ident = self.definition(*sym);
                        let ident_name = self.emit_name(&ident.name);
                        let idx = self.emit_expr(idx);

                        if let Type::BuiltIn(BltInTy::Vec(_)) = &ident.typ {
                            quote!({
                                let __idx = #idx as usize;
                                let __vec = &mut #ident_name;
                                __vec.resize(__idx + 1, Default::default());
                                __vec.insert(__idx, #val);
                            };)
                        } else {
                            quote!(#ident_name.insert(#idx, #val);)
                        }
                    },
                    AssignKind::Field(sym, field) => {
                        let ident = self.definition(*sym);
                        let ident_name = self.emit_name(&ident.name);
                        let field = self.emit_name(field);

                        quote!(#ident_name.#field = #val;)
                    }
                }
            },
        }
    }

    fn emit_expr(&self, expr: &Expr) -> TokenStream {
        match &expr.node {
            ExprKind::Term(term) => self.emit_term(term),
            ExprKind::Block(blk) => self.emit_block(blk, None),
            ExprKind::If(if_) => {
                let cond = self.emit_expr(&if_.cond);
                let body = self.emit_block(&if_.body, None);
                let else_body = if_.else_body.as_ref()
                    .map_or_else(TokenStream::new, |eb| {
                        let eb = self.emit_block(eb, None);

                        quote!(else #eb)
                    });

                quote!(if #cond #body #else_body)
            },
            ExprKind::Call(call) => {
                let func = self.definition(call.func);
                let func_name = self.emit_name(&func.name);
                let args = call.args
                    .iter()
                    .map(|a| self.safe_emit_expr(a))
                    .collect::<Vec<_>>();

                quote!(#func_name(#(#args),*))
            },
            ExprKind::Arith(arith) => {
                let lhs = self.emit_expr(&arith.lhs);
                let rhs = self.emit_expr(&arith.rhs);

                let lhs = quote!((#lhs));
                let rhs = quote!((#rhs));

                match arith.op {
                    ArithOp::Add => quote!(#lhs + #rhs),
                    ArithOp::Sub => quote!(#lhs - #rhs),
                    ArithOp::Mult => quote!(#lhs * #rhs),
                    ArithOp::Div => quote!(#lhs / #rhs),
                    ArithOp::Pow => quote!(#lhs.pow(#rhs)),
                    ArithOp::Mod => quote!(#lhs % #rhs),
                    ArithOp::LShift => quote!(#lhs << #rhs),
                    ArithOp::RShift => quote!(#lhs >> #rhs),
                }
            },
            ExprKind::Logic(logic) => match &logic {
                Logic::Not(expr) => {
                    let expr = self.emit_expr(expr);
                    quote!(!#expr)
                },
                Logic::Oper(oper) => {
                    let lhs = self.emit_expr(&oper.lhs);
                    let rhs = self.emit_expr(&oper.rhs);

                    let lhs = quote!((#lhs));
                    let rhs = quote!((#rhs));

                    match oper.op {
                        LogicOp::DoubleEquals => quote!(#lhs == #rhs),
                        LogicOp::NotEquals => quote!(#lhs != #rhs),
                        LogicOp::LessThan => quote!(#lhs < #rhs),
                        LogicOp::LessEquals => quote!(#lhs <= #rhs),
                        LogicOp::GreaterThan => quote!(#lhs > #rhs),
                        LogicOp::GreaterEquals => quote!(#lhs >= #rhs),
                        LogicOp::And => quote!(#lhs && #rhs),
                        LogicOp::Or => quote!(#lhs || #rhs),
                        _ => TokenStream::new()
                    }
                }
            },
            _ => TokenStream::new()
        }
    }

    fn emit_term(&self, term: &Term) -> TokenStream {
        match term {
            Term::Var(sym) => {
                let var = self.definition(*sym);
                let var_name = self.emit_name(&var.name);

                quote!(#var_name)
            },
            Term::Field{owner, field} => {
                let field = self.emit_name(field);
                let owner = self.emit_expr(owner);

                quote!(#owner.#field)
            },
            Term::Index{owner, index} =>  {
                let owner = self.emit_expr(owner);
                let index = self.emit_expr(index);

                quote!(#owner[#index])
            },
            Term::Lit(lit) => match lit {
                Literal::String(s) => quote!(#s.to_string()),
                Literal::Chr(c) => quote!(#c),
                Literal::Int(i) => quote!(#i),
                Literal::Flt(f) => quote!(#f),
                Literal::Bool(b) => quote!(#b),
                Literal::Vec(items) => {
                    let items: Vec<_> = items.iter().map(|i| self.safe_emit_expr(i)).collect();
                    let items_len = items.len();

                    quote!({
                        let mut vec = Vec::with_capacity(#items_len);
                        #(vec.push(#items);)*

                        vec
                    })
                },
                Literal::Map(items) => {
                    let (keys, values): (Vec<_>, Vec<_>) = items
                        .iter()
                        .map(|(k, v)| (self.safe_emit_expr(k), self.safe_emit_expr(v)))
                        .unzip();

                    let items_len = items.len();

                    quote!({
                        let mut map = HashMap::with_capacity(#items_len);
                        #(map.insert(#keys, #values);)*

                        map
                    })
                }
            }
        }
    }

    fn safe_emit_expr(&self, expr: &Expr) -> TokenStream {
        let a = self.emit_expr(expr);

        match &expr.node {
            ExprKind::Term(Term::Var(_))
            | ExprKind::Term(Term::Index{..})
            | ExprKind::Term(Term::Field{..}) => quote!(#a.clone()),
            _ => a
        }
    }
}
