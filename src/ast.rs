pub mod expr;
pub mod stmt;

use stmt::Stmt;

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
