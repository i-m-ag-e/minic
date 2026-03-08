use crate::{
    ast::{
        ASTRefVisitor, BlockItem,
        expr::{ExprRefVisitor, UnaryOp},
        stmt::StmtRefVisitor,
    },
    symbol::Symbol,
    with_token::WithToken,
};

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

impl<'a, Table> ExprRefVisitor<()> for PrettyPrinter<'a, Table> {
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

impl<'a, Table> StmtRefVisitor<()> for PrettyPrinter<'a, Table> {
    fn visit_null_stmt(&mut self) -> () {
        println!("{}NULL", "  ".repeat(self.indent_level));
    }

    fn visit_return_stmt(&mut self, stmt: &WithToken<Option<crate::ast::expr::Expr>>) -> () {
        print!("{}RETURN ", "  ".repeat(self.indent_level));
        if let Some(expr) = &stmt.item {
            self.visit_expr(expr);
        }
        println!();
    }

    fn visit_expr_stmt(&mut self, stmt: &crate::ast::expr::Expr) -> () {
        print!("{}EXPR_STMT ", "  ".repeat(self.indent_level));
        self.visit_expr(stmt);
        println!();
    }
}

impl<'a, Table> ASTRefVisitor for PrettyPrinter<'a, Table> {
    type ProgramResult = ();
    type FunctionDefResult = ();
    type StmtResult = ();
    type ExprResult = ();
    type BlockItemResult = ();
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
            for item in body {
                self.visit_block_item(item);
            }
        } else {
            println!("{}<declaration>", "  ".repeat(self.indent_level));
        }
        self.indent_level -= 1;
    }

    fn visit_block_item(&mut self, item: &crate::ast::BlockItem) -> Self::BlockItemResult {
        match item {
            BlockItem::Decl(decl) => self.visit_var_decl(decl),
            BlockItem::Stmt(stmt) => self.visit_stmt(stmt),
        }
    }

    fn visit_var_decl(&mut self, var_decl: &crate::ast::VarDeclaration) -> Self::VarDeclResult {
        let name = (self.get_symbol)(self.symbol_table, var_decl.name.item).unwrap_or("<unknown>");
        print!("{}VAR {} ", "  ".repeat(self.indent_level), name);
        if let Some(init) = &var_decl.initializer {
            print!("= ");
            self.visit_expr(init);
        }
        println!();
    }
}
