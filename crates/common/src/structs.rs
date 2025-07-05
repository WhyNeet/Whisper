use crate::types::Type;

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub ty: Type,
    pub is_pub: bool,
}
