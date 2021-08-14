use crate::Symbol;
use std::collections::BTreeMap;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type {
    Named(Symbol),
    BuiltIn(BltInTy),
    Generic(Box<Type>, Vec<Type>),
    Fn(Vec<Type>, Box<Type>),
    Method(Box<Type>, Vec<Type>, Box<Type>),
    Error,
    Infer,
    Any,
}

impl Type {
    pub fn unifies(&self, other: &Self) -> bool {
        match other {
            Type::Error => return true,
            Type::Any => return true,
            _ => ()
        };

        match self {
            Type::Named(sym) => match other {
                Type::Named(osym) => sym == osym,
                _ => false,
            },
            Type::BuiltIn(blt) => match other {
                Type::BuiltIn(oblt) => blt.unifies(oblt),
                _ => false,
            },
            Type::Fn(args, ret) => match other {
                Type::Fn(oargs, oret) => {
                    ret.unifies(&oret) && args.iter().zip(oargs.iter()).all(|(a, oa)| a.unifies(oa))
                }
                _ => false,
            },
            Type::Error => true,
            Type::Any => true,
            _ => unimplemented!("Generics and methods not implemented"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum BltInTy {
    Int,
    Flt,
    Str,
    Char,
    Vec(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Unit,
    Bool,
}

impl BltInTy {
    pub fn unifies(&self, other: &Self) -> bool {
        match self {
            BltInTy::Int => *other == BltInTy::Int,
            BltInTy::Char => *other == BltInTy::Char,
            BltInTy::Bool => *other == BltInTy::Bool,
            BltInTy::Unit => *other == BltInTy::Unit,
            BltInTy::Str => *other == BltInTy::Str,
            BltInTy::Flt => {
                *other == BltInTy::Flt || *other == BltInTy::Int
            }
            BltInTy::Vec(inner) => match other {
                BltInTy::Vec(oinner) => inner.unifies(&oinner),
                _ => false,
            },
            BltInTy::Map(key, value) => match other {
                BltInTy::Map(okey, oval) => key.unifies(&okey) && value.unifies(&oval),
                _ => false,
            },
        }
    }
}
