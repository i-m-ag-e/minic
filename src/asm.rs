mod asm_gen;
use core::panic;
use std::fmt::Display;

use crate::{
    ast::expr::{BinaryOp, UnaryOp},
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

    pub fn to_string(&self) -> String {
        let mut asm_str = String::new();
        for func in &self.function_defs {
            asm_str.push_str(&func.to_string());
        }
        asm_str.push_str("\n    .section .note.GNU-stack,\"\",@progbits\n");
        asm_str
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    name: String,
    stack_size: i32,
    body: Vec<Instruction>,
}

impl FunctionDef {
    fn to_string(&self) -> String {
        let mut func_str = format!("{}.globl {}\n", INDENT, self.name);
        func_str.push_str(&format!("{}:\n", self.name));
        func_str.push_str(&format!("{}pushq %rbp\n", INDENT));
        func_str.push_str(&format!("{}movq %rsp, %rbp\n", INDENT));
        for instr in &self.body {
            func_str.push_str(&format!("{}\n", instr.to_string()));
        }
        func_str
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
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

impl Instruction {
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

    fn to_string(&self) -> String {
        let (insts, indent) = match self {
            Instruction::AllocateStack(n) => {
                if *n > 0 {
                    (vec![format!("subq\t\t${}, %rsp", n)], true)
                } else {
                    (vec![], true)
                }
            }
            Instruction::Binary { op, src, dest } => (
                vec![format!(
                    "{}\t\t{}, {}",
                    Self::binary_op_to_inst(op),
                    src.to_string(),
                    dest.to_string()
                )],
                true,
            ),
            Instruction::Cmp(lhs, rhs) => (
                vec![format!("cmpq\t\t{}, {}", lhs.to_string(), rhs.to_string())],
                true,
            ),
            Instruction::Cqo => (vec!["cqo".to_string()], true),
            Instruction::Label(label) => (vec![format!("{}:", label)], false),
            Instruction::Jmp(label) => (vec![format!("jmp\t\t{}", label)], true),
            Instruction::JmpCC(cond, label) => (vec![format!("j{}\t\t{}", cond, label)], true),
            Instruction::Idiv(operand) => (vec![format!("idivq\t\t{}", operand.to_string())], true),
            Instruction::Mov { src, dest } => (
                vec![format!("movq\t\t{}, {}", src.to_string(), dest.to_string())],
                true,
            ),
            Instruction::Ret => (
                vec![
                    format!("movq\t\t%rbp, %rsp"),
                    format!("popq\t\t%rbp"),
                    format!("ret"),
                ],
                true,
            ),
            Instruction::SetCC(cond, operand) => (
                vec![format!("set{}\t\t{}", cond, operand.to_string())],
                true,
            ),
            Instruction::Unary(op, operand) => (
                vec![format!(
                    "{}\t\t{}",
                    Self::unary_op_to_inst(op),
                    operand.to_string()
                )],
                true,
            ),
        };
        insts
            .into_iter()
            .map(|s| format!("{}{}", if indent { INDENT } else { "" }, s))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Imm(i64),
    Pseudo(VarID),
    Register(Register),
    Stack(i32),
}

impl Operand {
    fn to_string(&self) -> String {
        match self {
            Operand::Imm(value) => format!("${}", value),
            Operand::Pseudo(var) => format!("pseudo.{}", var),
            Operand::Register(reg) => reg.to_string(),
            Operand::Stack(n) => format!("{}(%rbp)", n),
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
