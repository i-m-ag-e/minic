use crate::with_token::WithToken;

use super::Block;
use super::expr::Expr;

#[derive(Debug, Clone)]
pub enum Stmt {
    Compound(Block),
    Expr(Expr),
    Goto(WithToken<String>),
    If(IfStmt),
    Label(Label),
    Null,
    Return(WithToken<Option<Expr>>),
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: WithToken<Expr>,
    pub then_stmt: Box<Stmt>,
    pub else_stmt: Option<WithToken<Box<Stmt>>>,
}

#[derive(Debug, Clone)]
pub struct Label {
    pub name: WithToken<String>,
    pub next_stmt: Box<Stmt>,
}

pub trait StmtVisitor<R> {
    fn visit_compound(&mut self, block: Block) -> R;
    fn visit_expr_stmt(&mut self, stmt: Expr) -> R;
    fn visit_goto_stmt(&mut self, stmt: WithToken<String>) -> R;
    fn visit_if_stmt(&mut self, stmt: IfStmt) -> R;
    fn visit_label_stmt(&mut self, stmt: Label) -> R;
    fn visit_null_stmt(&mut self) -> R;
    fn visit_return_stmt(&mut self, stmt: WithToken<Option<Expr>>) -> R;

    fn visit_stmt(&mut self, stmt: Stmt) -> R {
        match stmt {
            Stmt::Compound(block) => self.visit_compound(block),
            Stmt::Expr(expr) => self.visit_expr_stmt(expr),
            Stmt::Goto(goto_stmt) => self.visit_goto_stmt(goto_stmt),
            Stmt::If(if_stmt) => self.visit_if_stmt(if_stmt),
            Stmt::Label(label_stmt) => self.visit_label_stmt(label_stmt),
            Stmt::Null => self.visit_null_stmt(),
            Stmt::Return(ret) => self.visit_return_stmt(ret),
        }
    }
}

pub trait StmtRefVisitor<R> {
    fn visit_compound(&mut self, block: &Block) -> R;
    fn visit_expr_stmt(&mut self, stmt: &Expr) -> R;
    fn visit_goto_stmt(&mut self, stmt: &WithToken<String>) -> R;
    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> R;
    fn visit_label_stmt(&mut self, stmt: &Label) -> R;
    fn visit_null_stmt(&mut self) -> R;
    fn visit_return_stmt(&mut self, stmt: &WithToken<Option<Expr>>) -> R;

    fn visit_stmt(&mut self, stmt: &Stmt) -> R {
        match stmt {
            Stmt::Compound(block) => self.visit_compound(block),
            Stmt::Expr(expr) => self.visit_expr_stmt(expr),
            Stmt::Goto(goto_stmt) => self.visit_goto_stmt(goto_stmt),
            Stmt::If(if_stmt) => self.visit_if_stmt(if_stmt),
            Stmt::Label(label_stmt) => self.visit_label_stmt(label_stmt),
            Stmt::Null => self.visit_null_stmt(),
            Stmt::Return(ret) => self.visit_return_stmt(ret),
        }
    }
}
