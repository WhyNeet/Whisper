use std::mem;

use common::effects::Effect;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Type {
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
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
        alias: String,
        fields: Vec<(String, Type)>,
    },
    Infer {
        candidate: Box<Type>,
    },
    Namespace {
        alias: String,
        fields: Vec<(String, Type)>,
    },
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Self::Int8
                | Self::Int16
                | Self::Int32
                | Self::Int64
                | Self::UInt8
                | Self::UInt16
                | Self::UInt32
                | Self::UInt64
                | Self::Float32
                | Self::Float64
        )
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Self::Int8 | Self::Int16 | Self::Int32 | Self::Int64)
    }

    pub fn is_uint(&self) -> bool {
        matches!(
            self,
            Self::UInt8 | Self::UInt16 | Self::UInt32 | Self::UInt64
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::Float32 | Self::Float64)
    }

    pub fn is_signed_numeric(&self) -> bool {
        matches!(
            self,
            Self::Int8 | Self::Int16 | Self::Int32 | Self::Int64 | Self::Float32 | Self::Float64
        )
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

    pub fn as_struct(self) -> Option<(String, Vec<(String, Type)>)> {
        match self {
            Self::Struct { fields, alias } => Some((alias, fields)),
            _ => None,
        }
    }

    pub fn as_namespace(self) -> Option<(String, Vec<(String, Type)>)> {
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
