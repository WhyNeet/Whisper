use std::fmt;

use crate::types::Type;

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

impl UnaryOperator {
    pub fn accepts_type(&self, ty: Type) -> bool {
        match self {
            Self::Neg => ty.is_signed_numeric(),
            Self::Not => ty == Type::Bool,
        }
    }

    pub fn result_type(&self, ty: Type) -> Type {
        match self {
            Self::Neg => ty,
            Self::Not => Type::Bool,
        }
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

impl BinaryOperator {
    pub fn is_bitwise(&self) -> bool {
        match self {
            Self::BitOr | Self::BitAnd | Self::BitXor | Self::Shl | Self::Shr => true,
            _ => false,
        }
    }

    pub fn is_logic(&self) -> bool {
        match self {
            Self::Or | Self::And => true,
            _ => false,
        }
    }

    pub fn is_arithmetic(&self) -> bool {
        match self {
            Self::Add | Self::Sub | Self::Mul | Self::Div => true,
            _ => false,
        }
    }

    pub fn accepts_type(&self, left: Type, right: Type) -> bool {
        match self {
            Self::Add => match left {
                ty if ty.is_numeric() => ty == right,
                Type::Char | Type::String => right == Type::Char || right.is_string(),
                _ => false,
            },
            Self::Sub => left.is_numeric() && left == right,
            op if op.is_bitwise() => left.is_numeric() && left == right,
            op if op.is_logic() => left == Type::Bool && left == right,
            _ => false,
        }
    }
    pub fn result_type(&self, left: Type, _right: Type) -> Type {
        match self {
            Self::Add => match left {
                ty if ty.is_numeric() => ty,
                Type::String | Type::Char => Type::String,
                _ => unreachable!(),
            },
            op if op.is_arithmetic() => match left {
                ty if ty.is_numeric() => ty,
                _ => unreachable!(),
            },
            op if op.is_logic() => match left {
                Type::Bool => left,
                _ => unreachable!(),
            },
            op if op.is_bitwise() => match left {
                ty if ty.is_numeric() => ty,
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}
