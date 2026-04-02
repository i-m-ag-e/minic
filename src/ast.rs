pub mod expr;
pub mod folder;
pub mod stmt;

use crate::ast::stmt::Stmt;
use crate::with_token::WithToken;
use crate::{
    ast::{
        expr::{Expr, ExprVisitor},
        stmt::StmtVisitor,
    },
    symbol::Symbol,
};

#[derive(Debug, Clone)]
pub struct Program {
    pub function_defs: Vec<FunctionDecl>,
}

pub type FunctionParam = WithToken<Option<Symbol>>;

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: WithToken<Symbol>,
    pub params: Vec<FunctionParam>,
    pub body: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub block_begin: WithToken<()>,
    pub body: Vec<BlockItem>,
    pub introduce_scope: bool,
}

pub type MultiVarDeclaration = Vec<VarDeclaration>;

#[derive(Debug, Clone)]
pub enum BlockItem {
    FunctionDecl(FunctionDecl),
    Stmt(Stmt),
    VarDecl(MultiVarDeclaration),
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
    type FunctionDeclResult;
    type ProgramResult;
    type StmtResult;
    type VarDeclResult;

    fn visit_program(&mut self, program: &Program) -> Self::ProgramResult;
    fn visit_function_def(&mut self, func_decl: &FunctionDecl) -> Self::FunctionDeclResult;
    fn visit_block_item(&mut self, item: &BlockItem) -> Self::BlockItemResult;
    fn visit_block(&mut self, block: &Block) -> Self::BlockResult;

    fn visit_var_decl(&mut self, var_decl: &VarDeclaration) -> Self::VarDeclResult;
}
