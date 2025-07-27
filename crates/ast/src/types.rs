#[derive(Debug, Clone)]
pub enum Type {
    Alias(String),
}

impl From<&str> for Type {
    fn from(value: &str) -> Self {
        Self::Alias(value.to_string())
    }
}
