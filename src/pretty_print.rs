use crate::{
    ast::{
        ASTVisitor, BlockItem,
        expr::ExprVisitor,
        stmt::{Label, StmtVisitor},
    },
    symbol::Symbol,
    with_token::WithToken,
};

const INDENT: &str = "  ";

type GetSymbol<Table> = Box<dyn Fn(&Table, Symbol) -> Option<&str>>;

pub struct PrettyPrinter<'a, Table> {
    indent_level: usize,
    symbol_table: &'a Table,
    get_symbol: GetSymbol<Table>,
}

impl<'a, Table> PrettyPrinter<'a, Table> {
    pub fn new(symbol_table: &'a Table, get_symbol: GetSymbol<Table>) -> Self {
        PrettyPrinter {
            indent_level: 0,
            symbol_table,
            get_symbol,
        }
    }
}

impl<'a, Table> ExprVisitor<()> for PrettyPrinter<'a, Table> {
    fn visit_assignment_expr(&mut self, expr: &crate::ast::expr::AssignExpr) -> () {
        print!("(");
        self.visit_expr(&expr.target);
        print!(" = ");
        self.visit_expr(&expr.right);
        print!(")");
    }

    fn visit_binary_expr(&mut self, expr: &crate::ast::expr::BinaryExpr) -> () {
        print!("(");
        self.visit_expr(&expr.left);
        print!(" {} ", expr.operator.item);
        self.visit_expr(&expr.right);
        print!(")");
    }

    fn visit_conditional_expr(&mut self, expr: &crate::ast::expr::ConditionalExpr) -> () {
        print!("(");
        self.visit_expr(&expr.condition);
        print!(" ? ");
        self.visit_expr(&expr.then_expr);
        print!(" : ");
        self.visit_expr(&expr.else_expr);
        print!(")");
    }

    fn visit_constant(
        &mut self,
        expr: &crate::with_token::WithToken<crate::lexer::token::Literal>,
    ) -> () {
        match &expr.item {
            crate::lexer::token::Literal::Integer(value) => {
                print!("{}", value);
            }
            _ => {}
        }
    }

    fn visit_unary_expr(&mut self, expr: &crate::ast::expr::UnaryExpr) -> () {
        print!("(");
        print!("{}", expr.operator.item);
        self.visit_expr(&expr.operand);
        print!(")");
    }

    fn visit_variable(&mut self, var: &WithToken<crate::symbol::Symbol>) -> () {
        let name = (self.get_symbol)(&self.symbol_table, var.item).unwrap_or("<unknown>");
        print!("(var){}", name);
    }
}

impl<'a, Table> StmtVisitor<()> for PrettyPrinter<'a, Table> {
    fn visit_break(&mut self, _stmt: &crate::ast::stmt::BreakStmt) -> () {
        println!("{}BREAK", INDENT.repeat(self.indent_level))
    }

    fn visit_compound(&mut self, block: &crate::ast::Block) -> () {
        self.visit_block(block)
    }

    fn visit_continue(&mut self, _stmt: &crate::ast::stmt::ContinueStmt) -> () {
        println!("{}CONTINUE", INDENT.repeat(self.indent_level))
    }

    fn visit_do_while_stmt(&mut self, stmt: &crate::ast::stmt::DoWhileStmt) -> () {
        print!("{}DO ", INDENT.repeat(self.indent_level));
        self.visit_stmt(&stmt.body);
        print!("\n{}WHILE", INDENT.repeat(self.indent_level + 1));
        self.visit_expr(&stmt.condition);
        println!();
    }

    fn visit_for_stmt(&mut self, stmt: &crate::ast::stmt::ForStmt) -> () {
        print!("{}FOR ", INDENT.repeat(self.indent_level));
        if let Some(init) = &*stmt.initializer {
            match init {
                crate::ast::stmt::ForStmtInit::Expression(expr) => {
                    print!("(expr) ");
                    self.visit_expr(expr);
                }
                crate::ast::stmt::ForStmtInit::Declaration(decls) => {
                    print!("(decl) ");
                    for decl in decls {
                        self.visit_var_decl(decl);
                    }
                }
            }
        }
        print!("; ");
        if let Some(cond) = &*stmt.condition {
            self.visit_expr(cond);
        }
        print!("; ");
        if let Some(step) = &*stmt.step {
            self.visit_expr(step);
        }
        println!();
        self.indent_level += 1;
        self.visit_stmt(&stmt.body);
        self.indent_level -= 1;
    }

    fn visit_null_stmt(&mut self) -> () {
        println!("{}NULL", INDENT.repeat(self.indent_level));
    }

    fn visit_goto_stmt(&mut self, stmt: &WithToken<String>) -> () {
        let label = &stmt.item;
        println!("{}GOTO {}", INDENT.repeat(self.indent_level), label);
    }

    fn visit_if_stmt(&mut self, stmt: &crate::ast::stmt::IfStmt) -> () {
        print!("{}IF ", INDENT.repeat(self.indent_level));
        self.visit_expr(&stmt.condition.item);
        println!();
        self.indent_level += 1;
        self.visit_stmt(&stmt.then_stmt);
        self.indent_level -= 1;
        if let Some(else_stmt) = &stmt.else_stmt {
            println!("{}ELSE", INDENT.repeat(self.indent_level));
            self.indent_level += 1;
            self.visit_stmt(&else_stmt.item);
            self.indent_level -= 1;
        }
    }

    fn visit_label_stmt(&mut self, stmt: &Label) -> () {
        let label_name = &stmt.name.item;
        println!("{}LABEL {}:", INDENT.repeat(self.indent_level), label_name);
        self.indent_level += 1;
        self.visit_stmt(&stmt.next_stmt);
        self.indent_level -= 1;
    }

    fn visit_return_stmt(&mut self, stmt: &WithToken<Option<crate::ast::expr::Expr>>) -> () {
        print!("{}RETURN ", INDENT.repeat(self.indent_level));
        if let Some(expr) = &stmt.item {
            self.visit_expr(expr);
        }
        println!();
    }

    fn visit_while_stmt(&mut self, stmt: &crate::ast::stmt::WhileStmt) -> () {
        print!("{}IF ", INDENT.repeat(self.indent_level));
        self.visit_expr(&stmt.condition.item);
        println!();
        self.indent_level += 1;
        self.visit_stmt(&stmt.body);
        self.indent_level -= 1;
    }

    fn visit_expr_stmt(&mut self, stmt: &crate::ast::expr::Expr) -> () {
        print!("{}EXPR_STMT ", INDENT.repeat(self.indent_level));
        self.visit_expr(stmt);
        println!();
    }
}

impl<'a, Table> ASTVisitor for PrettyPrinter<'a, Table> {
    type ProgramResult = ();
    type FunctionDefResult = ();
    type StmtResult = ();
    type ExprResult = ();
    type BlockItemResult = ();
    type BlockResult = ();
    type VarDeclResult = ();

    fn visit_program(&mut self, program: &crate::ast::Program) -> Self::ProgramResult {
        for func_def in &program.function_defs {
            self.visit_function_def(func_def);
        }
    }

    fn visit_function_def(
        &mut self,
        func_def: &crate::ast::FunctionDef,
    ) -> Self::FunctionDefResult {
        println!("Function: {}", func_def.name.as_str());
        self.indent_level += 1;
        if let Some(body) = &func_def.body {
            self.visit_block(body)
        } else {
            println!("{}<declaration>", INDENT.repeat(self.indent_level));
        }
        self.indent_level -= 1;
    }

    fn visit_block_item(&mut self, item: &crate::ast::BlockItem) -> Self::BlockItemResult {
        match item {
            BlockItem::Decl(decls) => {
                decls.iter().for_each(|decl| self.visit_var_decl(decl));
            }
            BlockItem::Stmt(stmt) => self.visit_stmt(stmt),
        }
    }

    fn visit_block(&mut self, block: &crate::ast::Block) -> Self::BlockResult {
        println!("{}{{", INDENT.repeat(self.indent_level));
        self.indent_level += 1;
        for item in &block.body {
            self.visit_block_item(item);
        }
        self.indent_level -= 1;
        println!("{}}}", INDENT.repeat(self.indent_level));
    }

    fn visit_var_decl(&mut self, var_decl: &crate::ast::VarDeclaration) -> Self::VarDeclResult {
        let name = (self.get_symbol)(self.symbol_table, var_decl.name.item).unwrap_or("<unknown>");
        print!("{}VAR {} ", INDENT.repeat(self.indent_level), name);
        if let Some(init) = &var_decl.initializer {
            print!("= ");
            self.visit_expr(init);
        }
        println!();
    }
}
