#[derive(Debug, Clone)]
pub enum LiteralValue {
    String(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
    Unit,
}
