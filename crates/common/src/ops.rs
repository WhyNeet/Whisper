use std::fmt;

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Neg,
    Not,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}",
            match self {
                Self::Neg => "-",
                Self::Not => "!",
            }
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

impl BinaryOperator {
    pub fn is_bitwise(&self) -> bool {
        matches!(
            self,
            BinaryOperator::BitOr
                | BinaryOperator::BitAnd
                | BinaryOperator::BitXor
                | BinaryOperator::Shl
                | BinaryOperator::Shr
        )
    }

    pub fn is_logic(&self) -> bool {
        matches!(self, BinaryOperator::Or | BinaryOperator::And)
    }

    pub fn is_arithmetic(&self) -> bool {
        matches!(
            self,
            BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mul | BinaryOperator::Div
        )
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}",
            match self {
                Self::Add => "+",
                Self::Sub => "-",
                Self::Mul => "*",
                Self::Div => "/",
                Self::And => "&&",
                Self::Or => "||",
                Self::BitAnd => "&",
                Self::BitOr => "|",
                Self::BitXor => "^",
                Self::Shr => ">>",
                Self::Shl => "<<",
            }
        )
    }
}
