use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Effect {
    Fs,
    Net,
    Io,
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
            "io" => Ok(Self::Io),
            "nondet" => Ok(Self::NonDet),
            _ => Err("invalid effect."),
        }
    }
}

impl fmt::Display for Effect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Fs => "fs",
                Self::Mut => "mut",
                Self::Net => "net",
                Self::Io => "io",
                Self::NonDet => "nondet",
            }
        )
    }
}
