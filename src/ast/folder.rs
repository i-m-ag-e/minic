use crate::ast::expr::Expr;
use crate::ast::stmt::{self, Stmt};
use crate::ast::{Block, BlockItem, FunctionDef, Program, VarDeclaration, expr};
use crate::lexer::token::Literal;
use crate::symbol::Symbol;
use crate::with_token::WithToken;

pub trait ASTFolder<E> {
    // ---------------- Expressions ------------------------
    fn visit_expr(&mut self, expr: Expr) -> Result<Expr, E> {
        self.fold_expr(expr)
    }

    fn fold_expr(&mut self, expr: Expr) -> Result<Expr, E> {
        match expr {
            Expr::Assignment(assign) => self.visit_assignment_expr(assign),
            Expr::Binary(binary_expr) => self.visit_binary_expr(binary_expr),
            Expr::Conditional(conditional_expr) => self.visit_conditional_expr(conditional_expr),
            Expr::Constant(lit) => self.visit_constant(lit),
            Expr::Unary(unary_expr) => self.visit_unary_expr(unary_expr),
            Expr::Variable(var) => self.visit_variable(var),
        }
    }

    fn visit_assignment_expr(&mut self, expr: expr::AssignExpr) -> Result<Expr, E> {
        self.fold_assignment_expr(expr)
    }

    fn fold_assignment_expr(&mut self, expr: expr::AssignExpr) -> Result<Expr, E> {
        let target = Box::new(self.visit_expr(*expr.target)?);
        let right = Box::new(self.visit_expr(*expr.right)?);
        Ok(Expr::Assignment(expr::AssignExpr {
            target,
            eq_token: expr.eq_token,
            right,
        }))
    }

    fn visit_binary_expr(&mut self, expr: expr::BinaryExpr) -> Result<Expr, E> {
        self.fold_binary_expr(expr)
    }

    fn fold_binary_expr(&mut self, expr: expr::BinaryExpr) -> Result<Expr, E> {
        let left = Box::new(self.visit_expr(*expr.left)?);
        let right = Box::new(self.visit_expr(*expr.right)?);
        Ok(Expr::Binary(expr::BinaryExpr {
            left,
            operator: expr.operator,
            right,
        }))
    }

    fn visit_conditional_expr(&mut self, expr: expr::ConditionalExpr) -> Result<Expr, E> {
        self.fold_conditional_expr(expr)
    }

    fn fold_conditional_expr(&mut self, expr: expr::ConditionalExpr) -> Result<Expr, E> {
        let condition = Box::new(self.visit_expr(*expr.condition)?);
        let then_expr = expr
            .then_expr
            .map(|e| self.visit_expr(*e))
            .transpose()?
            .into_boxed();
        let else_expr = expr
            .else_expr
            .map(|e| self.visit_expr(*e))
            .transpose()?
            .into_boxed();
        Ok(Expr::Conditional(expr::ConditionalExpr {
            condition,
            then_expr,
            else_expr,
        }))
    }

    fn visit_constant(&mut self, expr: WithToken<Literal>) -> Result<Expr, E> {
        self.fold_constant(expr)
    }

    fn fold_constant(&mut self, expr: WithToken<Literal>) -> Result<Expr, E> {
        Ok(Expr::Constant(expr))
    }

    fn visit_unary_expr(&mut self, expr: expr::UnaryExpr) -> Result<Expr, E> {
        self.fold_unary_expr(expr)
    }

    fn fold_unary_expr(&mut self, expr: expr::UnaryExpr) -> Result<Expr, E> {
        let operand = Box::new(self.visit_expr(*expr.operand)?);
        Ok(Expr::Unary(expr::UnaryExpr {
            operator: expr.operator,
            operand,
            postfix: expr.postfix,
        }))
    }

    fn visit_variable(&mut self, var: WithToken<Symbol>) -> Result<Expr, E> {
        self.fold_variable(var)
    }

    fn fold_variable(&mut self, var: WithToken<Symbol>) -> Result<Expr, E> {
        Ok(Expr::Variable(var))
    }

    // ---------------- Statements ------------------------
    fn visit_stmt(&mut self, stmt: Stmt) -> Result<Stmt, E> {
        self.fold_stmt(stmt)
    }

    fn fold_stmt(&mut self, stmt: Stmt) -> Result<Stmt, E> {
        match stmt {
            Stmt::Break(stmt) => self.visit_break(stmt),
            Stmt::Case(stmt) => self.visit_case_stmt(stmt),
            Stmt::Compound(block) => self.visit_compound(block),
            Stmt::Continue(stmt) => self.visit_continue(stmt),
            Stmt::Default(stmt) => self.visit_default_stmt(stmt),
            Stmt::DoWhile(stmt) => self.visit_do_while_stmt(stmt),
            Stmt::Expr(expr) => self.visit_expr_stmt(expr),
            Stmt::For(stmt) => self.visit_for_stmt(stmt),
            Stmt::Goto(goto_stmt) => self.visit_goto_stmt(goto_stmt),
            Stmt::If(if_stmt) => self.visit_if_stmt(if_stmt),
            Stmt::Label(label_stmt) => self.visit_label_stmt(label_stmt),
            Stmt::Null => self.visit_null_stmt(),
            Stmt::Return(ret) => self.visit_return_stmt(ret),
            Stmt::Switch(stmt) => self.visit_switch_stmt(stmt),
            Stmt::While(stmt) => self.visit_while_stmt(stmt),
        }
    }

    fn visit_break(&mut self, stmt: stmt::BreakStmt) -> Result<Stmt, E> {
        self.fold_break(stmt)
    }

    fn fold_break(&mut self, stmt: stmt::BreakStmt) -> Result<Stmt, E> {
        Ok(Stmt::Break(stmt))
    }

    fn visit_case_stmt(&mut self, stmt: stmt::CaseStmt) -> Result<Stmt, E> {
        self.fold_case_stmt(stmt)
    }

    fn fold_case_stmt(&mut self, stmt: stmt::CaseStmt) -> Result<Stmt, E> {
        let value = stmt.value.map_transpose_result(|e| self.visit_expr(e))?;
        let next_stmt = Box::new(self.visit_stmt(*stmt.next_stmt)?);
        Ok(Stmt::Case(stmt::CaseStmt { value, next_stmt }))
    }

    fn visit_compound(&mut self, block: Block) -> Result<Stmt, E> {
        self.fold_compound(block)
    }

    fn fold_compound(&mut self, block: Block) -> Result<Stmt, E> {
        Ok(Stmt::Compound(self.visit_block(block)?))
    }

    fn visit_continue(&mut self, stmt: stmt::ContinueStmt) -> Result<Stmt, E> {
        self.fold_continue(stmt)
    }

    fn fold_continue(&mut self, stmt: stmt::ContinueStmt) -> Result<Stmt, E> {
        Ok(Stmt::Continue(stmt))
    }

    fn visit_default_stmt(&mut self, stmt: stmt::DefaultStmt) -> Result<Stmt, E> {
        self.fold_default_stmt(stmt)
    }

    fn fold_default_stmt(&mut self, stmt: stmt::DefaultStmt) -> Result<Stmt, E> {
        let next_stmt = Box::new(self.visit_stmt(*stmt.next_stmt)?);
        Ok(Stmt::Default(stmt::DefaultStmt {
            default_token: stmt.default_token,
            next_stmt,
        }))
    }

    fn visit_do_while_stmt(&mut self, stmt: stmt::DoWhileStmt) -> Result<Stmt, E> {
        self.fold_do_while_stmt(stmt)
    }

    fn fold_do_while_stmt(&mut self, stmt: stmt::DoWhileStmt) -> Result<Stmt, E> {
        let condition = stmt
            .condition
            .map_transpose_result(|c| self.visit_expr(c))?;
        let body = stmt
            .body
            .map_transpose_result(|s| self.visit_stmt(*s))?
            .into_boxed();

        Ok(Stmt::DoWhile(stmt::DoWhileStmt {
            loop_id: stmt.loop_id,
            condition,
            body,
        }))
    }

    fn visit_expr_stmt(&mut self, stmt: Expr) -> Result<Stmt, E> {
        self.fold_expr_stmt(stmt)
    }

    fn fold_expr_stmt(&mut self, stmt: Expr) -> Result<Stmt, E> {
        Ok(Stmt::Expr(self.visit_expr(stmt)?))
    }

    fn visit_for_stmt(&mut self, stmt: stmt::ForStmt) -> Result<Stmt, E> {
        self.fold_for_stmt(stmt)
    }

    fn fold_for_stmt(&mut self, stmt: stmt::ForStmt) -> Result<Stmt, E> {
        let initializer = stmt
            .initializer
            .map_option_transpose_result(|init| match init {
                stmt::ForStmtInit::Declaration(decls) => decls
                    .into_iter()
                    .map(|decl| self.visit_var_decl(decl))
                    .collect::<Result<Vec<_>, E>>()
                    .map(stmt::ForStmtInit::Declaration),
                stmt::ForStmtInit::Expression(expr) => {
                    Ok(stmt::ForStmtInit::Expression(self.visit_expr(expr)?))
                }
            })?;

        let condition = stmt
            .condition
            .map_option_transpose_result(|cond| self.visit_expr(cond))?;

        let step = stmt
            .step
            .map_option_transpose_result(|cond| self.visit_expr(cond))?;

        let body = Box::new(self.visit_stmt(*stmt.body)?);

        Ok(Stmt::For(stmt::ForStmt {
            loop_id: stmt.loop_id,
            initializer,
            condition,
            step,
            body,
        }))
    }

    fn visit_goto_stmt(&mut self, stmt: WithToken<String>) -> Result<Stmt, E> {
        self.fold_goto_stmt(stmt)
    }

    fn fold_goto_stmt(&mut self, stmt: WithToken<String>) -> Result<Stmt, E> {
        Ok(Stmt::Goto(stmt))
    }

    fn visit_if_stmt(&mut self, stmt: stmt::IfStmt) -> Result<Stmt, E> {
        self.fold_if_stmt(stmt)
    }

    fn fold_if_stmt(&mut self, stmt: stmt::IfStmt) -> Result<Stmt, E> {
        let condition = stmt
            .condition
            .map_transpose_result(|e| self.visit_expr(e))?;
        let then_stmt = Box::new(self.visit_stmt(*stmt.then_stmt)?);
        let else_stmt = stmt
            .else_stmt
            .map(|wt_stmt| wt_stmt.map_transpose_result(|s| self.visit_stmt(*s).map(Box::new)))
            .transpose()?;

        Ok(Stmt::If(stmt::IfStmt {
            condition,
            then_stmt,
            else_stmt,
        }))
    }

    fn visit_label_stmt(&mut self, stmt: stmt::Label) -> Result<Stmt, E> {
        self.fold_label_stmt(stmt)
    }

    fn fold_label_stmt(&mut self, stmt: stmt::Label) -> Result<Stmt, E> {
        Ok(Stmt::Label(stmt))
    }

    fn visit_null_stmt(&mut self) -> Result<Stmt, E> {
        self.fold_null_stmt()
    }

    fn fold_null_stmt(&mut self) -> Result<Stmt, E> {
        Ok(Stmt::Null)
    }

    fn visit_return_stmt(&mut self, stmt: WithToken<Option<Expr>>) -> Result<Stmt, E> {
        self.fold_return_stmt(stmt)
    }

    fn fold_return_stmt(&mut self, stmt: WithToken<Option<Expr>>) -> Result<Stmt, E> {
        stmt.map_option_transpose_result(|e| self.visit_expr(e))
            .map(Stmt::Return)
    }

    fn visit_switch_stmt(&mut self, stmt: stmt::SwitchStmt) -> Result<Stmt, E> {
        self.fold_switch_stmt(stmt)
    }

    fn fold_switch_stmt(&mut self, stmt: stmt::SwitchStmt) -> Result<Stmt, E> {
        let condition = stmt
            .condition
            .map_transpose_result(|e| self.visit_expr(e))?;
        let body = Box::new(self.visit_stmt(*stmt.body)?);
        Ok(Stmt::Switch(stmt::SwitchStmt {
            switch_id: stmt.switch_id,
            condition,
            body,
        }))
    }

    fn visit_while_stmt(&mut self, stmt: stmt::WhileStmt) -> Result<Stmt, E> {
        self.fold_while_stmt(stmt)
    }

    fn fold_while_stmt(&mut self, stmt: stmt::WhileStmt) -> Result<Stmt, E> {
        let condition = stmt
            .condition
            .map_transpose_result(|c| self.visit_expr(c))?;
        let body = Box::new(self.visit_stmt(*stmt.body)?);
        Ok(Stmt::While(stmt::WhileStmt {
            loop_id: stmt.loop_id,
            condition,
            body,
        }))
    }

    // -------------- Top-level AST Nodes ----------------
    fn visit_program(&mut self, program: Program) -> Result<Program, E> {
        self.fold_program(program)
    }

    fn fold_program(&mut self, program: Program) -> Result<Program, E> {
        let function_defs = program
            .function_defs
            .into_iter()
            .map(|def| self.visit_function_def(def))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Program { function_defs })
    }

    fn visit_function_def(&mut self, func_def: FunctionDef) -> Result<FunctionDef, E> {
        self.fold_function_def(func_def)
    }

    fn fold_function_def(&mut self, func_def: FunctionDef) -> Result<FunctionDef, E> {
        let body = func_def
            .body
            .map(|body| self.visit_block(body))
            .transpose()?;
        Ok(FunctionDef {
            name: func_def.name,
            body,
        })
    }

    fn visit_block_item(&mut self, item: BlockItem) -> Result<BlockItem, E> {
        self.fold_block_item(item)
    }

    fn fold_block_item(&mut self, item: BlockItem) -> Result<BlockItem, E> {
        match item {
            BlockItem::Stmt(stmt) => Ok(BlockItem::Stmt(self.visit_stmt(stmt)?)),
            BlockItem::Decl(decls) => Ok(BlockItem::Decl(
                decls
                    .into_iter()
                    .map(|decl| self.visit_var_decl(decl))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
        }
    }

    fn visit_block(&mut self, block: Block) -> Result<Block, E> {
        self.fold_block(block)
    }

    fn fold_block(&mut self, block: Block) -> Result<Block, E> {
        let body = block
            .body
            .into_iter()
            .map(|item| self.visit_block_item(item))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Block {
            body,
            block_begin: block.block_begin,
        })
    }

    fn visit_var_decl(&mut self, var_decl: VarDeclaration) -> Result<VarDeclaration, E> {
        self.fold_var_decl(var_decl)
    }

    fn fold_var_decl(&mut self, var_decl: VarDeclaration) -> Result<VarDeclaration, E> {
        let initializer = var_decl
            .initializer
            .map(|with_token| with_token.map_transpose_result(|e| self.visit_expr(e)))
            .transpose()?;
        Ok(VarDeclaration {
            name: var_decl.name,
            initializer,
        })
    }
}
