use super::*;

use crate::ast::{ASTRefVisitor, expr};
use crate::ast::{expr::ExprRefVisitor, stmt::StmtRefVisitor};
use crate::lexer::{Lexer, LexerResult};
use crate::parser;
use crate::source_file::SourceFile;
use crate::symbol::SymbolTable;
use insta::assert_yaml_snapshot;
use serde_json::{Value, json};

struct JsonVisitor<'a> {
    source: &'a SourceFile,
    symbol_table: SymbolTable,
    tokens: Vec<Token>,
}

impl<'a> JsonVisitor<'a> {
    fn new(source: &'a SourceFile) -> Self {
        Self {
            source,
            symbol_table: SymbolTable::new(),
            tokens: Vec::new(),
        }
    }

    fn visit_token(&self, token: &Token) -> Value {
        json!({
            "token_type": format!("{:?}", token.token_type),
            "lexeme": self.source.content[token.begin.0..token.end.0 ].to_string(),
            "line_col_begin": self.source.line_col(token.begin.0),
            "line_col_end": self.source.line_col(token.end.0),
        })
    }

    fn visit_token_short(&self, token: &Token) -> Value {
        json!(format!(
            "(pos: {} - {}) ({}:{} - {}:{}) `{}`",
            token.begin.0,
            token.end.0,
            self.source.line_col(token.begin.0).0,
            self.source.line_col(token.begin.0).1,
            self.source.line_col(token.end.0).0,
            self.source.line_col(token.end.0).1,
            self.source.content[token.begin.0..token.end.0].to_string()
        ))
    }
}

impl<'a> ExprRefVisitor<Value> for JsonVisitor<'a> {
    fn visit_binary_expr(&mut self, expr: &expr::BinaryExpr) -> Value {
        json!({
            "type": "BinaryExpr",
            "left": self.visit_expr(&expr.left),
            "operator": format!("{:?}", expr.operator.token_type),
            "right": self.visit_expr(&expr.right),
        })
    }

    fn visit_constant(&mut self, expr: &WithToken<Literal>) -> Value {
        json!({
            "type": "Constant",
            "token": self.visit_token(expr.get_token(&self.tokens)),
        })
    }

    fn visit_unary_expr(&mut self, expr: &expr::UnaryExpr) -> Value {
        let op_token = self.visit_token_short(expr.operator.get_token(&self.tokens));
        json!({
            "type": "UnaryExpr",
            "operator": format!("{:?}", expr.operator.item),
            "operand": self.visit_expr(&expr.operand),
            "operator_token": op_token,
        })
    }
}

impl<'a> StmtRefVisitor<Value> for JsonVisitor<'a> {
    fn visit_expr_stmt(&mut self, stmt: &Expr) -> Value {
        json!({
            "type": "ExprStmt",
            "expr": self.visit_expr(stmt),
        })
    }

    fn visit_return_stmt(&mut self, stmt: &Option<Expr>) -> Value {
        json!({
            "type": "ReturnStmt",
            "expr": stmt.as_ref().map(|e| self.visit_expr(e)),
        })
    }
}

impl<'a> ASTRefVisitor for JsonVisitor<'a> {
    type ProgramResult = Value;
    type FunctionDefResult = Value;
    type StmtResult = Value;
    type ExprResult = Value;

    fn visit_program(&mut self, program: &Program) -> Self::ProgramResult {
        json!({
            "type": "Program",
            "function_defs": program.function_defs.iter().map(|f| self.visit_function_def(f)).collect::<Vec<_>>(),
        })
    }

    fn visit_function_def(&mut self, func_def: &FunctionDef) -> Self::FunctionDefResult {
        json!({
            "type": "FunctionDef",
            "name": self.visit_token(func_def.name.get_token(&self.tokens)),
            "body": func_def.body.as_ref().map(|stmts| stmts.iter().map(|s| self.visit_stmt(s)).collect::<Vec<_>>()),
        })
    }
}

fn test_string_success(s: &str) -> anyhow::Result<Value> {
    let input = SourceFile::new("test_input.c".to_owned(), s.to_owned());
    let mut json_visitor = JsonVisitor::new(&input);

    let lexer = Lexer::new(&input, &mut json_visitor.symbol_table);
    let tokens: Vec<Token> = lexer.collect::<LexerResult<Vec<_>>>()?;

    let mut used_tokens = vec![false; tokens.len()];
    let mut parser = parser::Parser::new(&tokens, &mut used_tokens);
    let prog = parser.parse()?;

    json_visitor.tokens = Parser::filter_saved_tokens(tokens, &mut used_tokens);

    let output = json_visitor.visit_program(&prog);
    Ok(output)
}

#[test]
fn test_parser_simple_fn() -> anyhow::Result<()> {
    assert_yaml_snapshot!(test_string_success("int main(void) { return 0; }")?);
    Ok(())
}

#[test]
fn test_parser_no_return() -> anyhow::Result<()> {
    assert_yaml_snapshot!(test_string_success("int main(void) { }")?);
    Ok(())
}

#[test]
fn test_parser_return_n() -> anyhow::Result<()> {
    assert_yaml_snapshot!(test_string_success("int main(void) { return 42; }")?);
    Ok(())
}

#[test]
fn test_unary_expr() -> anyhow::Result<()> {
    assert_yaml_snapshot!(test_string_success("int main(void) { return ~-~-1; }")?);
    Ok(())
}

#[test]
fn test_fn_different_name() -> anyhow::Result<()> {
    assert_yaml_snapshot!(test_string_success("int foo(void) { return 1; }")?);
    Ok(())
}
