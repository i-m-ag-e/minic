pub mod tacky_gen;
use std::fmt::Display;

use serde::{Serialize, ser::SerializeStruct};

use crate::{
    ast::expr::{BinaryOp, UnaryOp},
    debug_info::DebugInfo,
};

#[derive(Debug, Clone, Serialize)]
pub struct Program(pub FunctionDef);

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub body: Vec<Instruction>,
    pub pos: (usize, usize),
}

impl Serialize for FunctionDef {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("FunctionDef", 2)?;
        state.serialize_field("name", &self.name)?;
        state.serialize_field(
            "body",
            &self
                .body
                .iter()
                .map(|inst| inst.kind.clone())
                .collect::<Vec<_>>(),
        )?;
        state.end()
    }
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub debug_info: DebugInfo,
}

#[derive(Debug, Clone, Serialize)]
pub enum InstructionKind {
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

impl InstructionKind {
    pub fn debug_info(&self) -> String {
        match self {
            InstructionKind::Binary {
                op,
                dest,
                left,
                right,
            } => {
                format!("`{}` = `{}` {} `{}`", dest, left, op, right)
            }
            InstructionKind::Copy { dest, src } => {
                format!("(copy) `{}` = `{}`", dest, src)
            }
            InstructionKind::Jump(_label) => String::new(),
            InstructionKind::JumpIfNotZero(cond, _label) => format!("jmp if `{}` != 0", cond),
            InstructionKind::JumpIfZero(cond, _label) => format!("jmp if `{}` == 0", cond),
            InstructionKind::Label(_label) => String::new(),
            InstructionKind::Return(val) => format!("return `{}`", val),
            InstructionKind::Unary { op, dest, src } => {
                format!("(unary) `{}` = `{}` {}", dest, op, src)
            }
        }
    }
}

pub type VarID = usize;

#[derive(Debug, Clone, Serialize)]
pub enum Value {
    Constant(i64),
    Var(VarID),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Constant(c) => write!(f, "{}", c),
            Value::Var(id) => write!(f, "<stack_var>.{}", id),
        }
    }
}

#[cfg(test)]
mod tacky_tests;
