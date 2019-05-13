use std::boxed::Box;

type TypeRef<N> = Box<Type<N>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Type<N> {
    Term(N),
    Var(N),
    App(TypeRef<N>, Vec<Type<N>>),
    Arrow(Option<TypeRef<N>>, TypeRef<N>),
    Method(TypeRef<N>),
    Parens(TypeRef<N>),
    Forall(Forall<N>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Forall<N> {
    pub vars: Vec<N>,
    pub constraint: Option<Constraint<N>>,
    pub base: TypeRef<N>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint<N> {
    Constraint(N, Vec<Type<N>>),
    Parens(Vec<Constraint<N>>),
}
