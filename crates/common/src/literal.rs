use crate::types::Type;

#[derive(Debug, Clone)]
pub struct Literal {
    pub value: LiteralValue,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    String(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
    Unit,
}
