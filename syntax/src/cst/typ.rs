use bumpalo::collections::Vec;

type TypeRef<'arena, N> = &'arena Type<'arena, N>;

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'arena, N> {
    Term(N),
    Var(N),
    App(TypeRef<'arena, N>, Vec<'arena, Type<'arena, N>>),
    Arrow(TypeRef<'arena, N>, TypeRef<'arena, N>),
    Method(TypeRef<'arena, N>),
    Parens(TypeRef<'arena, N>),
    Forall(Forall<'arena, N>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Forall<'arena, N> {
    pub vars: Vec<'arena, N>,
    pub constraint: Option<Constraint<'arena, N>>,
    pub base: TypeRef<'arena, N>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint<'arena, N> {
    Constraint(N, Vec<'arena, Type<'arena, N>>),
    Parens(Vec<'arena, Constraint<'arena, N>>),
}
