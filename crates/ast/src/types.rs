#[derive(Debug, Clone)]
pub enum Type {
    Alias(String),
}

impl From<&str> for Type {
    fn from(value: &str) -> Self {
        Self::Alias(value.to_string())
    }
}

impl Type {
    pub fn is_int(&self) -> bool {
        match self {
            Self::Alias(ty) => ty == "Int",
        }
    }

    pub fn is_uint(&self) -> bool {
        match self {
            Self::Alias(ty) => ty == "UInt",
        }
    }

    pub fn is_whole(&self) -> bool {
        self.is_int() || self.is_uint()
    }

    pub fn is_float(&self) -> bool {
        match self {
            Self::Alias(ty) => ty == "Float",
        }
    }
}
