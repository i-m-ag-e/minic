mod asm_gen;
use core::panic;
use std::fmt::Display;

use crate::{
    ast::expr::{BinaryOp, UnaryOp},
    debug_info::DebugInfo,
    tacky::VarID,
};
pub use asm_gen::tacky_to_asm;

const INDENT: &str = "    ";

#[derive(Debug, Clone)]
pub struct Program {
    function_defs: Vec<FunctionDef>,
}

impl Program {
    fn new() -> Self {
        Self {
            function_defs: Vec::new(),
        }
    }

    pub fn to_asm_string<W: std::fmt::Write>(
        &self,
        w: &mut W,
        no_comments: bool,
    ) -> std::fmt::Result {
        for func in &self.function_defs {
            func.to_asm_string(w, no_comments)?;
        }
        write!(w, "\n    .section .note.GNU-stack,\"\",@progbits\n")?;
        Ok(())
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.to_asm_string(f, false)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    name: String,
    stack_size: i32,
    body: Vec<Instruction>,
}

impl FunctionDef {
    fn to_asm_string<W: std::fmt::Write>(&self, w: &mut W, no_comments: bool) -> std::fmt::Result {
        write!(w, "{}.globl {}\n", INDENT, self.name)?;
        write!(w, "{}:\n", self.name)?;
        write!(w, "{}pushq %rbp\n", INDENT)?;
        write!(w, "{}movq %rsp, %rbp\n", INDENT)?;
        for instr in &self.body {
            let inst_str = if let InstructionKind::Label(_) = instr.kind {
                format!("{}", instr.kind)
            } else {
                format!("{}{}", INDENT, instr.kind)
            };

            if !no_comments && !matches!(instr.kind, InstructionKind::Label(_)) {
                write!(w, "{:<40}# {}\n", inst_str, instr.debug_info)?;
            } else {
                write!(w, "{}\n", inst_str)?;
            }
        }
        Ok(())
    }
}

impl Display for FunctionDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.to_asm_string(f, false)
    }
}

#[derive(Debug, Clone)]
pub struct Instruction {
    kind: InstructionKind,
    debug_info: DebugInfo,
}

#[derive(Debug, Clone)]
pub enum InstructionKind {
    AllocateStack(usize),
    Binary {
        op: BinaryOp,
        src: Operand,
        dest: Operand,
    },
    Cmp(Operand, Operand),
    Cqo,
    Idiv(Operand),
    Jmp(String),
    JmpCC(Condition, String),
    SetCC(Condition, Operand),
    Label(String),
    Mov {
        src: Operand,
        dest: Operand,
    },
    Ret,
    Unary(UnaryOp, Operand),
}

impl InstructionKind {
    fn unary_op_to_inst(op: &UnaryOp) -> &'static str {
        match op {
            UnaryOp::Negate => "negq",
            UnaryOp::BitNot => "notq",
            UnaryOp::Not => panic!("Logical NOT should be handled separately with JmpCC and SetCC"),
        }
    }

    fn binary_op_to_inst(op: &BinaryOp) -> &'static str {
        match op {
            BinaryOp::Add => "addq",
            BinaryOp::Subtract => "subq",
            BinaryOp::Multiply => "imulq",
            BinaryOp::Divide => "idivq",
            BinaryOp::Modulus => "idivq",
            BinaryOp::BitAnd => "andq",
            BinaryOp::BitOr => "orq",
            BinaryOp::BitXor => "xorq",
            BinaryOp::LeftShift => "shlq",
            BinaryOp::RightShift => "shrq",
            BinaryOp::And
            | BinaryOp::Or
            | BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::Greater
            | BinaryOp::GreaterEqual
            | BinaryOp::LessThan
            | BinaryOp::LessEqual => {
                panic!("Logical operations should be handled separately with JmpCC and SetCC")
            }
        }
    }

    fn get_operands_mut(&mut self) -> Vec<&mut Operand> {
        match self {
            InstructionKind::Binary { src, dest, .. } => vec![src, dest],
            InstructionKind::Cmp(lhs, rhs) => vec![lhs, rhs],
            InstructionKind::Idiv(operand) => vec![operand],
            InstructionKind::Mov { src, dest } => vec![src, dest],
            InstructionKind::SetCC(_, operand) => vec![operand],
            InstructionKind::Unary(_, operand) => vec![operand],
            _ => vec![],
        }
    }
}

impl Display for InstructionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InstructionKind::AllocateStack(n) => {
                if *n > 0 {
                    write!(f, "{:<10}\t\t${}, %rsp", "subq", n)
                } else {
                    write!(f, "")
                }
            }
            InstructionKind::Binary { op, src, dest } => {
                write!(
                    f,
                    "{:<10}\t\t{}, {}",
                    Self::binary_op_to_inst(op),
                    src,
                    dest
                )
            }
            InstructionKind::Cmp(lhs, rhs) => {
                write!(f, "{:<10}\t\t{}, {}", "cmpq", lhs, rhs)
            }
            InstructionKind::Cqo => write!(f, "{:<10}", "cqo"),
            InstructionKind::Label(label) => write!(f, "{}:", label),
            InstructionKind::Jmp(label) => write!(f, "{:<10}\t\t{}", "jmp", label),
            InstructionKind::JmpCC(cond, label) => {
                write!(f, "{:<10}\t\t{}", format!("j{}", cond), label)
            }
            InstructionKind::Idiv(operand) => write!(f, "{:<10}\t\t{}", "idivq", operand),
            InstructionKind::Mov { src, dest } => write!(f, "{:<10}\t\t{}, {}", "movq", src, dest),
            InstructionKind::Ret => {
                write!(f, "{:<10}\t\t%rbp, %rsp\n", "movq")?;
                write!(f, "{}{:<10}\t\t%rbp\n", INDENT, "popq")?;
                write!(f, "{}{:<10}", INDENT, "ret")
            }
            InstructionKind::SetCC(cond, operand) => {
                write!(f, "{:<10}\t\t{}", format!("set{}", cond), operand)
            }
            InstructionKind::Unary(op, operand) => {
                write!(f, "{:<10}\t\t{}", Self::unary_op_to_inst(op), operand)
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Imm(i64),
    Pseudo(VarID),
    Register(Register),
    Stack(i32),
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Imm(value) => write!(f, "${}", value),
            Operand::Pseudo(var) => write!(f, "pseudo.{}", var),
            Operand::Register(reg) => reg.fmt(f),
            Operand::Stack(n) => write!(f, "-{}(%rbp)", n),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Condition {
    E,
    NE,
    L,
    LE,
    G,
    GE,
}

impl Display for Condition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let cond_str = match self {
            Condition::E => "e",
            Condition::NE => "ne",
            Condition::L => "l",
            Condition::LE => "le",
            Condition::G => "g",
            Condition::GE => "ge",
        };
        write!(f, "{}", cond_str)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Register {
    AL,
    AX,
    CL,
    CX,
    DL,
    DX,
    R10B,
    R10,
    R11B,
    R11,
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let reg_str = match self {
            Register::AL => "%al",
            Register::AX => "%rax",
            Register::CL => "%cl",
            Register::CX => "%rcx",
            Register::DL => "%dl",
            Register::DX => "%rdx",
            Register::R10B => "%r10b",
            Register::R10 => "%r10",
            Register::R11B => "%r11b",
            Register::R11 => "%r11",
        };
        write!(f, "{}", reg_str)
    }
}

#[cfg(test)]
mod asm_tests;
