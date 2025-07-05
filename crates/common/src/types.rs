use std::mem;

use crate::effects::Effect;

#[derive(Debug, Clone)]
pub enum Type {
    Int8,
    Int16,
    Int32,
    Int64,
    /// Inferred Int
    IntN,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
    /// Inferred Float
    FloatN,
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
        fields: Vec<(String, Type)>,
    },
    StructInstance {
        of: Box<Type>,
    },
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::IntN => other.is_int() || other.is_uint(),
            Self::FloatN => other.is_float(),
            _ => mem::discriminant(self) == mem::discriminant(other),
        }
    }

    fn ne(&self, other: &Self) -> bool {
        match self {
            Self::IntN => !other.is_int() && !other.is_uint(),
            Self::FloatN => !other.is_float(),
            _ => mem::discriminant(self) != mem::discriminant(other),
        }
    }
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        match self {
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
            | Self::IntN => true,
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Self::Int8 | Self::Int16 | Self::Int32 | Self::Int64 | Self::IntN => true,
            _ => false,
        }
    }

    pub fn is_uint(&self) -> bool {
        match self {
            Self::UInt8 | Self::UInt16 | Self::UInt32 | Self::UInt64 => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Self::FloatN | Self::Float32 | Self::Float64 => true,
            _ => false,
        }
    }

    pub fn is_inferred(&self) -> bool {
        match self {
            Self::FloatN | Self::IntN => true,
            _ => false,
        }
    }

    pub fn is_signed_numeric(&self) -> bool {
        match self {
            Self::Int8
            | Self::Int16
            | Self::Int32
            | Self::Int64
            | Self::Float32
            | Self::Float64
            | Self::IntN => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        *self == Type::String
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

    pub fn as_struct(self) -> Option<Vec<(String, Type)>> {
        match self {
            Self::Struct { fields } => Some(fields),
            _ => None,
        }
    }

    pub fn as_struct_instance(self) -> Option<Box<Type>> {
        match self {
            Self::StructInstance { of } => Some(of),
            _ => None,
        }
    }
}

impl TryFrom<&str> for Type {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "i8" => Ok(Self::Int8),
            "i16" => Ok(Self::Int16),
            "i32" => Ok(Self::Int32),
            "i64" => Ok(Self::Int64),
            "u8" => Ok(Self::UInt8),
            "u16" => Ok(Self::UInt16),
            "u32" => Ok(Self::UInt32),
            "u64" => Ok(Self::UInt64),
            "f32" => Ok(Self::Float32),
            "f64" => Ok(Self::Float64),
            "bool" => Ok(Self::Bool),
            "char" => Ok(Self::Char),
            "string" => Ok(Self::String),
            "unit" => Ok(Self::Unit),
            _ => Err("not a type."),
        }
    }
}
