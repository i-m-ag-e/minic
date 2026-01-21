pub mod expr;
pub mod stmt;

use stmt::Stmt;

use crate::ast::{
    expr::{ExprRefVisitor, ExprVisitor},
    stmt::{StmtRefVisitor, StmtVisitor},
};

#[derive(Debug, Clone)]
pub struct Program {
    pub function_defs: Vec<FunctionDef>,
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    // pub params: Vec<String>,
    pub body: Option<Vec<Stmt>>,
}

pub trait ASTVisitor: StmtVisitor<Self::StmtResult> + ExprVisitor<Self::ExprResult> {
    type StmtResult;
    type ExprResult;
    type ProgramResult;
    type FunctionDefResult;

    fn visit_program(&mut self, program: Program) -> Self::ProgramResult;
    fn visit_function_def(&mut self, func_def: FunctionDef) -> Self::FunctionDefResult;
}

pub trait ASTRefVisitor:
    StmtRefVisitor<Self::StmtResult> + ExprRefVisitor<Self::ExprResult>
{
    type StmtResult;
    type ExprResult;
    type ProgramResult;
    type FunctionDefResult;

    fn visit_program(&mut self, program: &Program) -> Self::ProgramResult;
    fn visit_function_def(&mut self, func_def: &FunctionDef) -> Self::FunctionDefResult;
}
