#[derive(Debug, Clone)]
pub enum Type {
    Alias(String),
    InferInt,
    InferFloat,
    // Generic, Function, etc.
}

impl From<&str> for Type {
    fn from(value: &str) -> Self {
        Self::Alias(value.to_string())
    }
}
