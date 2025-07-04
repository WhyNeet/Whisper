#[derive(Debug, Clone, PartialEq)]
pub enum Annotation {
    Main,
}

impl TryFrom<&str> for Annotation {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "main" => Ok(Self::Main),
            _ => Err("Not an annotation."),
        }
    }
}
