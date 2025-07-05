use std::fmt;

use tcast::types::Type;

pub struct TypeWrapper(Type);

impl fmt::Display for TypeWrapper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match &self.0 {
                Type::Bool => "Boolean".to_string(),
                Type::Char | Type::String => "String".to_string(),
                ty if ty.is_numeric() => "Number".to_string(),
                Type::StructInstance { of } => Self(of.as_ref().clone()).to_string(),
                Type::Unit => String::new(),
                _ => todo!(),
            }
        )
    }
}
