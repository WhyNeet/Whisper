use std::ops::Deref;

use crate::types::Type;
use common::ops::{BinaryOperator, UnaryOperator};

pub struct TypedUnaryOperator(UnaryOperator);
pub struct TypedBinaryOperator(BinaryOperator);

impl From<UnaryOperator> for TypedUnaryOperator {
    fn from(value: UnaryOperator) -> Self {
        Self(value)
    }
}

impl From<BinaryOperator> for TypedBinaryOperator {
    fn from(value: BinaryOperator) -> Self {
        Self(value)
    }
}

impl Deref for TypedUnaryOperator {
    type Target = UnaryOperator;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Deref for TypedBinaryOperator {
    type Target = BinaryOperator;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TypedUnaryOperator {
    pub fn accepts_type(&self, ty: &Type) -> bool {
        let op = self.0;
        match op {
            UnaryOperator::Neg => ty.is_signed_numeric(),
            UnaryOperator::Not => *ty == Type::Bool,
        }
    }

    pub fn result_type(&self, ty: &Type) -> Type {
        let op = self.0;
        match op {
            UnaryOperator::Neg => ty.clone(),
            UnaryOperator::Not => Type::Bool,
        }
    }
}

impl TypedBinaryOperator {
    pub fn accepts_type(&self, left: &Type, right: &Type) -> bool {
        let op = self.0;
        match op {
            BinaryOperator::Add => match left {
                ty if ty.is_numeric() => ty == right,
                Type::Char | Type::String => *right == Type::Char || right.is_string(),
                _ => false,
            },
            BinaryOperator::Sub => left.is_numeric() && left == right,
            op if op.is_bitwise() => left.is_numeric() && left == right,
            op if op.is_logic() => *left == Type::Bool && left == right,
            _ => false,
        }
    }
    pub fn result_type(&self, left: &Type, _right: &Type) -> Type {
        let op = self.0;
        match op {
            BinaryOperator::Add => match left {
                ty if ty.is_numeric() => ty.clone(),
                Type::String | Type::Char => Type::String,
                _ => unreachable!(),
            },
            op if op.is_arithmetic() => match left {
                ty if ty.is_numeric() => ty.clone(),
                _ => unreachable!(),
            },
            op if op.is_logic() => match left {
                Type::Bool => left.clone(),
                _ => unreachable!(),
            },
            op if op.is_bitwise() => match left {
                ty if ty.is_numeric() => ty.clone(),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}
