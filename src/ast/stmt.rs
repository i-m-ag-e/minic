use serde::Serialize;

use crate::ast::MultiVarDeclaration;
use crate::with_token::WithToken;

use super::Block;
use super::expr::Expr;

#[derive(Debug, Clone)]
pub enum Stmt {
    Break(BreakStmt),
    Case(CaseStmt),
    Compound(Block),
    Continue(ContinueStmt),
    Default(DefaultStmt),
    DoWhile(DoWhileStmt),
    Expr(Expr),
    For(ForStmt),
    Goto(WithToken<String>),
    If(IfStmt),
    Label(Label),
    Null,
    Return(WithToken<Option<Expr>>),
    Switch(SwitchStmt),
    While(WhileStmt),
}

pub type LoopID = i64;

#[derive(Debug, Clone, Copy, Serialize)]
pub enum BreakTarget {
    Loop(LoopID),
    Switch(LoopID),
}

#[derive(Debug, Clone)]
pub struct BreakStmt(pub WithToken<BreakTarget>);

#[derive(Debug, Clone)]
pub struct CaseStmt {
    pub value: WithToken<Expr>,
    pub next_stmt: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct ContinueStmt(pub WithToken<LoopID>);

#[derive(Debug, Clone)]
pub struct DefaultStmt {
    pub default_token: WithToken<()>,
    pub next_stmt: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct DoWhileStmt {
    pub loop_id: LoopID,
    pub condition: WithToken<Expr>,
    pub body: WithToken<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub enum ForStmtInit {
    Declaration(MultiVarDeclaration),
    Expression(Expr),
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub loop_id: LoopID,
    pub initializer: WithToken<Option<ForStmtInit>>,
    pub condition: WithToken<Option<Expr>>,
    pub step: WithToken<Option<Expr>>,
    pub body: Box<Stmt>,
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

#[derive(Debug, Clone)]
pub struct SwitchStmt {
    pub switch_id: LoopID,
    pub condition: WithToken<Expr>,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub loop_id: LoopID,
    pub condition: WithToken<Expr>,
    pub body: Box<Stmt>,
}

pub trait StmtVisitor<R> {
    fn visit_break(&mut self, stmt: &BreakStmt) -> R;
    fn visit_case(&mut self, stmt: &CaseStmt) -> R;
    fn visit_compound(&mut self, block: &Block) -> R;
    fn visit_continue(&mut self, stmt: &ContinueStmt) -> R;
    fn visit_default(&mut self, stmt: &DefaultStmt) -> R;
    fn visit_do_while_stmt(&mut self, stmt: &DoWhileStmt) -> R;
    fn visit_expr_stmt(&mut self, stmt: &Expr) -> R;
    fn visit_for_stmt(&mut self, stmt: &ForStmt) -> R;
    fn visit_goto_stmt(&mut self, stmt: &WithToken<String>) -> R;
    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> R;
    fn visit_label_stmt(&mut self, stmt: &Label) -> R;
    fn visit_null_stmt(&mut self) -> R;
    fn visit_return_stmt(&mut self, stmt: &WithToken<Option<Expr>>) -> R;
    fn visit_switch_stmt(&mut self, stmt: &SwitchStmt) -> R;
    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> R;

    fn visit_stmt(&mut self, stmt: &Stmt) -> R {
        match stmt {
            Stmt::Break(stmt) => self.visit_break(stmt),
            Stmt::Case(case) => self.visit_case(case),
            Stmt::Compound(block) => self.visit_compound(block),
            Stmt::Continue(stmt) => self.visit_continue(stmt),
            Stmt::Default(default) => self.visit_default(default),
            Stmt::DoWhile(stmt) => self.visit_do_while_stmt(stmt),
            Stmt::Expr(expr) => self.visit_expr_stmt(expr),
            Stmt::For(stmt) => self.visit_for_stmt(stmt),
            Stmt::Goto(goto_stmt) => self.visit_goto_stmt(goto_stmt),
            Stmt::If(if_stmt) => self.visit_if_stmt(if_stmt),
            Stmt::Label(label_stmt) => self.visit_label_stmt(label_stmt),
            Stmt::Null => self.visit_null_stmt(),
            Stmt::Return(ret) => self.visit_return_stmt(ret),
            Stmt::Switch(switch) => self.visit_switch_stmt(switch),
            Stmt::While(stmt) => self.visit_while_stmt(stmt),
        }
    }
}
