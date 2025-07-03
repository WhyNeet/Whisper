#[derive(Debug, Clone, Copy, PartialEq)]
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
