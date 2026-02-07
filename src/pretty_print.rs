use crate::ast::{
    ASTRefVisitor,
    expr::{BinaryOp, ExprRefVisitor, UnaryOp},
    stmt::StmtRefVisitor,
};

pub struct PrettyPrinter {
    indent_level: usize,
}

impl PrettyPrinter {
    pub fn new() -> Self {
        PrettyPrinter { indent_level: 0 }
    }
}

impl ExprRefVisitor<()> for PrettyPrinter {
    fn visit_binary_expr(&mut self, expr: &crate::ast::expr::BinaryExpr) -> () {
        print!("(");
        self.visit_expr(&expr.left);
        print!(
            " {} ",
            match expr.operator.item {
                BinaryOp::Add => "+",
                BinaryOp::And => "&&",
                BinaryOp::BitAnd => "&",
                BinaryOp::BitOr => "|",
                BinaryOp::BitXor => "^",
                BinaryOp::Divide => "/",
                BinaryOp::Equal => "==",
                BinaryOp::Greater => ">",
                BinaryOp::GreaterEqual => ">=",
                BinaryOp::LeftShift => "<<",
                BinaryOp::LessEqual => "<=",
                BinaryOp::LessThan => "<",
                BinaryOp::Modulus => "%",
                BinaryOp::Multiply => "*",
                BinaryOp::NotEqual => "!=",
                BinaryOp::Or => "||",
                BinaryOp::RightShift => ">>",
                BinaryOp::Subtract => "-",
            }
        );
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
        print!(
            "{}",
            match expr.operator.item {
                UnaryOp::Not => "!",
                UnaryOp::Negate => "-",
                UnaryOp::BitNot => "~",
            }
        );
        self.visit_expr(&expr.operand);
        print!(")");
    }
}

impl StmtRefVisitor<()> for PrettyPrinter {
    fn visit_return_stmt(&mut self, stmt: &Option<crate::ast::expr::Expr>) -> () {
        print!("{}RETURN", "  ".repeat(self.indent_level));
        if let Some(expr) = stmt {
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

impl ASTRefVisitor for PrettyPrinter {
    type ProgramResult = ();
    type FunctionDefResult = ();
    type StmtResult = ();
    type ExprResult = ();

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
            for stmt in body {
                self.visit_stmt(stmt);
            }
        } else {
            println!("{}<declaration>", "  ".repeat(self.indent_level));
        }
        self.indent_level -= 1;
    }
}
