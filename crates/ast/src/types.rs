use std::mem;

#[derive(Debug, Clone)]
pub enum Type {
    Int8,
    Int16,
    Int32,
    Int64,
    /// Inferred Int/UInt
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
    Custom(String),
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

impl From<&str> for Type {
    fn from(value: &str) -> Self {
        match value {
            "i8" => Self::Int8,
            "i16" => Self::Int16,
            "i32" => Self::Int32,
            "i64" => Self::Int64,
            "u8" => Self::UInt8,
            "u16" => Self::UInt16,
            "u32" => Self::UInt32,
            "u64" => Self::UInt64,
            "f32" => Self::Float32,
            "f64" => Self::Float64,
            "bool" => Self::Bool,
            "char" => Self::Char,
            "string" => Self::String,
            "unit" => Self::Unit,
            other => Self::Custom(other.to_string()),
        }
    }
}
