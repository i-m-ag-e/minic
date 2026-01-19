mod expr;
mod stmt;

#[derive(Debug, Clone)]
pub struct Program {
    pub stmts: Vec<stmt::Stmt>,
}
