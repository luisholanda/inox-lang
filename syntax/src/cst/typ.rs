use std::boxed::Box;
use symbol::pos::Located;

type TypeRef<N> = Box<Type<N>>;

pub type Type<N> = Located<TypeNode<N>>;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeNode<N> {
    Term(Located<N>),
    Var(Located<N>),
    App(TypeRef<N>, Vec<Type<N>>),
    Arrow(Option<TypeRef<N>>, TypeRef<N>),
    Method(TypeRef<N>),
    Parens(TypeRef<N>),
    Forall(Forall<N>),
}

pub type Forall<N> = Located<ForallNode<N>>;

#[derive(Debug, Clone, PartialEq)]
pub struct ForallNode<N> {
    pub vars: Vec<Located<N>>,
    pub constraint: Option<Constraint<N>>,
    pub base: TypeRef<N>,
}

pub type Constraint<N> = Located<ConstraintNode<N>>;

#[derive(Debug, Clone, PartialEq)]
pub enum ConstraintNode<N> {
    Constraint(Located<N>, Vec<Type<N>>),
    Parens(Vec<Constraint<N>>),
}
