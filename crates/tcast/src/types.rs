use common::effects::Effect;
use std::mem;
use string_cache::DefaultAtom as Atom;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Type {
    Int,
    UInt,
    Float,
    String,
    Bool,
    Char,
    Unit,
    Fn {
        return_type: Box<Type>,
        params: Vec<Type>,
        effects: Vec<Effect>,
    },
    Struct {
        alias: Atom,
        fields: Vec<(Atom, Type)>,
    },
    Namespace {
        alias: Atom,
        fields: Vec<(Atom, Type)>,
    },
    Infer {
        candidate: Box<Type>,
    },
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(self, Self::Int | Self::UInt | Self::Float)
    }

    pub fn is_whole(&self) -> bool {
        matches!(self, Self::Int | Self::UInt)
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Self::Int)
    }

    pub fn is_uint(&self) -> bool {
        matches!(self, Self::UInt)
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::Float)
    }

    pub fn is_signed_numeric(&self) -> bool {
        matches!(self, Self::Int | Self::Float)
    }

    pub fn is_string(&self) -> bool {
        *self == Type::String
    }

    pub fn is_fn(&self) -> bool {
        mem::discriminant(self)
            == mem::discriminant(&Type::Fn {
                return_type: Box::new(Type::Unit),
                params: vec![],
                effects: vec![],
            })
    }

    pub fn is_inferred(&self) -> bool {
        mem::discriminant(self)
            == mem::discriminant(&Type::Infer {
                candidate: Box::new(Type::Unit),
            })
    }
}

impl Type {
    pub fn as_fn(self) -> Option<(Box<Type>, Vec<Type>, Vec<Effect>)> {
        match self {
            Self::Fn {
                return_type,
                params,
                effects,
            } => Some((return_type, params, effects)),
            _ => None,
        }
    }

    pub fn as_struct(self) -> Option<(Atom, Vec<(Atom, Type)>)> {
        match self {
            Self::Struct { fields, alias } => Some((alias, fields)),
            _ => None,
        }
    }

    pub fn as_namespace(self) -> Option<(Atom, Vec<(Atom, Type)>)> {
        match self {
            Self::Namespace { fields, alias } => Some((alias, fields)),
            _ => None,
        }
    }

    pub fn as_inferred(self) -> Option<Box<Type>> {
        match self {
            Self::Infer { candidate } => Some(candidate),
            _ => None,
        }
    }

    pub fn can_infer(&self, from: &Type) -> bool {
        match from {
            Type::Infer { candidate } => match self {
                ty if ty.is_int() || ty.is_uint() => candidate.is_int() || candidate.is_uint(),
                ty if ty.is_float() => candidate.is_float(),
                _ => false,
            },
            _ => false,
        }
    }
}

// #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct TypeId(u64);

// static COUNTER: AtomicU64 = AtomicU64::new(0);

// impl TypeId {
//     pub fn sequential() -> Self {
//         let counter = COUNTER.fetch_add(1, Ordering::Relaxed);
//         Self(counter)
//     }
// }

// pub const UNIT_TYPE_ID: TypeId = TypeId(13);
