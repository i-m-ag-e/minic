pub mod expr;
pub mod stmt;

use crate::with_token::WithToken;
use crate::{
    ast::{
        expr::{Expr, ExprRefVisitor, ExprVisitor},
        stmt::{StmtRefVisitor, StmtVisitor},
    },
    symbol::Symbol,
};
use stmt::Stmt;

#[derive(Debug, Clone)]
pub struct Program {
    pub function_defs: Vec<FunctionDef>,
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: WithToken<String>,
    // pub params: Vec<String>,
    pub body: Option<Vec<BlockItem>>,
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Stmt(Stmt),
    Decl(VarDeclaration),
}

#[derive(Debug, Clone)]
pub struct VarDeclaration {
    pub name: WithToken<Symbol>,
    pub initializer: Option<WithToken<Expr>>,
}

pub trait ASTVisitor: StmtVisitor<Self::StmtResult> + ExprVisitor<Self::ExprResult> {
    type BlockItemResult;
    type ExprResult;
    type FunctionDefResult;
    type ProgramResult;
    type StmtResult;
    type VarDeclResult;

    fn visit_program(&mut self, program: Program) -> Self::ProgramResult;
    fn visit_function_def(&mut self, func_def: FunctionDef) -> Self::FunctionDefResult;
    fn visit_block_item(&mut self, item: BlockItem) -> Self::BlockItemResult;

    fn visit_var_decl(&mut self, var_decl: VarDeclaration) -> Self::VarDeclResult;
}

pub trait ASTRefVisitor:
    StmtRefVisitor<Self::StmtResult> + ExprRefVisitor<Self::ExprResult>
{
    type BlockItemResult;
    type ExprResult;
    type FunctionDefResult;
    type ProgramResult;
    type StmtResult;
    type VarDeclResult;

    fn visit_program(&mut self, program: &Program) -> Self::ProgramResult;
    fn visit_function_def(&mut self, func_def: &FunctionDef) -> Self::FunctionDefResult;
    fn visit_block_item(&mut self, item: &BlockItem) -> Self::BlockItemResult;

    fn visit_var_decl(&mut self, var_decl: &VarDeclaration) -> Self::VarDeclResult;
}
