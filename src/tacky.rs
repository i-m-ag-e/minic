pub mod tacky_gen;
use serde::Serialize;

use crate::ast::expr::{BinaryOp, UnaryOp};

#[derive(Debug, Clone, Serialize)]
pub struct Program(pub FunctionDef);

#[derive(Debug, Clone, Serialize)]
pub struct FunctionDef {
    pub name: String,
    pub body: Vec<Instruction>,
}

#[derive(Debug, Clone, Serialize)]
pub enum Instruction {
    Binary {
        op: BinaryOp,
        dest: Value,
        left: Value,
        right: Value,
    },
    Copy {
        dest: Value,
        src: Value,
    },
    Jump(String),
    JumpIfZero(Value, String),
    JumpIfNotZero(Value, String),
    Label(String),
    Return(Value),
    Unary {
        op: UnaryOp,
        dest: Value,
        src: Value,
    },
}

pub type VarID = usize;

#[derive(Debug, Clone, Serialize)]
pub enum Value {
    Constant(i64),
    Var(VarID),
}

#[cfg(test)]
mod tacky_tests;
