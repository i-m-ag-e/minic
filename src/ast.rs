pub mod expr;
pub mod stmt;

use crate::ast::stmt::Stmt;
use crate::with_token::WithToken;
use crate::{
    ast::{
        expr::{Expr, ExprRefVisitor, ExprVisitor},
        stmt::{StmtRefVisitor, StmtVisitor},
    },
    symbol::Symbol,
};

#[derive(Debug, Clone)]
pub struct Program {
    pub function_defs: Vec<FunctionDef>,
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: WithToken<String>,
    // pub params: Vec<String>,
    pub body: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub body: Vec<BlockItem>,
    pub block_begin: WithToken<()>,
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
    type BlockResult;
    type ExprResult;
    type FunctionDefResult;
    type ProgramResult;
    type StmtResult;
    type VarDeclResult;

    fn visit_program(&mut self, program: Program) -> Self::ProgramResult;
    fn visit_function_def(&mut self, func_def: FunctionDef) -> Self::FunctionDefResult;
    fn visit_block_item(&mut self, item: BlockItem) -> Self::BlockItemResult;
    fn visit_block(&mut self, block: Block) -> Self::BlockResult;

    fn visit_var_decl(&mut self, var_decl: VarDeclaration) -> Self::VarDeclResult;
}

pub trait ASTRefVisitor:
    StmtRefVisitor<Self::StmtResult> + ExprRefVisitor<Self::ExprResult>
{
    type BlockItemResult;
    type BlockResult;
    type ExprResult;
    type FunctionDefResult;
    type ProgramResult;
    type StmtResult;
    type VarDeclResult;

    fn visit_program(&mut self, program: &Program) -> Self::ProgramResult;
    fn visit_function_def(&mut self, func_def: &FunctionDef) -> Self::FunctionDefResult;
    fn visit_block_item(&mut self, item: &BlockItem) -> Self::BlockItemResult;
    fn visit_block(&mut self, block: &Block) -> Self::BlockResult;

    fn visit_var_decl(&mut self, var_decl: &VarDeclaration) -> Self::VarDeclResult;
}
