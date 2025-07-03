#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Effect {
    Fs,
    Net,
    NonDet,
    Mut,
}

impl TryFrom<&str> for Effect {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "fs" => Ok(Self::Fs),
            "net" => Ok(Self::Net),
            "mut" => Ok(Self::Mut),
            "nondet" => Ok(Self::NonDet),
            _ => Err("invalid effect."),
        }
    }
}
