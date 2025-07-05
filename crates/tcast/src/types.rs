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
        fields: Vec<(String, Type)>,
    },
    StructInstance {
        of: Box<Type>,
    },
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
            | Self::Float64 => true,
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Self::Int8 | Self::Int16 | Self::Int32 | Self::Int64 => true,
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
            Self::Float32 | Self::Float64 => true,
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
            | Self::Float64 => true,
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
