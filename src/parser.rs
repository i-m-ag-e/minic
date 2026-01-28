pub mod parser_error;

use crate::ast::FunctionDef;
use crate::ast::expr::{Expr, UnaryExpr, UnaryOp};
use crate::ast::{Program, stmt::Stmt};
use crate::lexer::token::Literal;
use crate::source_file::SourcePosition;
use crate::symbol::Symbol;
use anyhow::Result;
use parser_error::{ParserError, ParserErrorType};

use crate::{
    lexer::token::{Token, TokenType},
    with_token::WithToken,
};

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    index: usize,
    used_tokens: &'a mut Vec<bool>,
    used_count: usize,
}

pub type ParseResult<T> = Result<T, ParserError>;

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token], used_tokens: &'a mut Vec<bool>) -> Self {
        Self {
            tokens,
            index: 0,
            used_tokens,
            used_count: 0,
        }
    }

    fn save_previous(&mut self) -> usize {
        let uc = self.used_count;
        if self.index > 0 && self.index - 1 < self.used_tokens.len() {
            self.used_tokens[self.index - 1] = true;
            self.used_count += 1;
        }
        uc
    }

    fn save_and_advance(&mut self) -> Option<usize> {
        let token = self.advance();
        if token.is_some() {
            Some(self.save_previous())
        } else {
            None
        }
    }

    pub fn filter_saved_tokens(tokens: Vec<Token>, used_tokens: &'a mut Vec<bool>) -> Vec<Token> {
        tokens
            .into_iter()
            .enumerate()
            .filter_map(|(i, tok)| if used_tokens[i] { Some(tok) } else { None })
            .collect()
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.tokens.len()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    fn peek_token_type(&self) -> Option<&TokenType> {
        self.peek().map(|token| &token.token_type)
    }

    fn peek_forward(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.index + n)
    }

    fn advance(&mut self) -> Option<&Token> {
        if self.is_at_end() {
            None
        } else {
            let token = &self.tokens[self.index];
            self.index += 1;
            Some(token)
        }
    }

    fn advance_if<F>(&mut self, condition: F) -> Option<&Token>
    where
        F: Fn(&Token) -> bool,
    {
        if let Some(token) = self.peek() {
            if condition(token) {
                return self.advance();
            }
        }
        None
    }

    fn consume_if<F, E>(&mut self, condition: F, error: E) -> ParseResult<&Token>
    where
        F: Fn(&TokenType) -> bool,
        E: Fn(&TokenType) -> ParserErrorType,
    {
        match self.peek_token_type() {
            None => Err(ParserError {
                err_type: ParserErrorType::UnexpectedEndOfInput,
                span: (
                    self.tokens
                        .last()
                        .map(|t| t.end)
                        .unwrap_or(SourcePosition(0)),
                    self.tokens
                        .last()
                        .map(|t| t.end)
                        .unwrap_or(SourcePosition(0)),
                ),
            }),
            Some(token_type) if condition(token_type) => {
                let token = self.advance().unwrap();
                Ok(token)
            }
            Some(token_type) => {
                let found_token = self.peek().unwrap();
                Err(ParserError {
                    err_type: error(token_type),
                    span: (found_token.begin, found_token.end),
                })
            }
        }
    }

    fn consume(&mut self, expected: &TokenType) -> ParseResult<&Token> {
        self.consume_if(
            |token_type| token_type == expected,
            |found| ParserErrorType::ExpectedAnother {
                expected: expected.clone(),
                found: found.clone(),
            },
        )
    }

    fn advance_if_eq(&mut self, expected: &TokenType) -> Option<&Token> {
        self.advance_if(|token| &token.token_type == expected)
    }

    fn advance_if_one_of(&mut self, expected: &[TokenType]) -> Option<&Token> {
        self.advance_if(|token| expected.contains(&token.token_type))
    }

    fn make_eof(&self) -> ParserError {
        ParserError {
            err_type: ParserErrorType::UnexpectedEndOfInput,
            span: (
                self.tokens
                    .last()
                    .map(|t| t.end)
                    .unwrap_or(SourcePosition(0)),
                self.tokens
                    .last()
                    .map(|t| t.end)
                    .unwrap_or(SourcePosition(0)),
            ),
        }
    }
}

impl<'a> Parser<'a> {
    pub fn parse(&mut self) -> ParseResult<Program> {
        self.parse_program()
    }

    fn parse_program(&mut self) -> ParseResult<Program> {
        let mut function_defs = Vec::new();
        while !self.is_at_end() {
            let func_def = self.parse_function_def()?;
            function_defs.push(func_def);
        }
        Ok(Program { function_defs })
    }

    fn parse_function_def(&mut self) -> ParseResult<FunctionDef> {
        self.consume(&TokenType::KInt)?;

        self.consume_if(
            |tt| matches!(tt, TokenType::Identifier(_)),
            |error| ParserErrorType::ExpectedAnother {
                expected: TokenType::Identifier(Symbol(0)),
                found: error.clone(),
            },
        )?;
        let func_name = self.save_previous();

        self.consume(&TokenType::LeftParen)?;
        self.consume(&TokenType::KVoid)?;
        self.consume(&TokenType::RightParen)?;
        self.consume(&TokenType::LeftBrace)?;

        let mut body = Vec::new();
        while self.advance_if_eq(&TokenType::RightBrace).is_none() {
            if self.is_at_end() {
                return Err(self.make_eof());
            }
            let stmt = self.parse_stmt()?;
            body.push(stmt);
        }

        Ok(FunctionDef {
            name: WithToken::new("main".to_string(), func_name),
            body: Some(body),
        })
    }
}

impl<'a> Parser<'a> {
    fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        match self.peek_token_type() {
            Some(TokenType::KReturn) => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_expr_stmt(&mut self) -> ParseResult<Stmt> {
        let expr = self.parse_expr()?;
        self.consume(&TokenType::Semicolon)?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_return_stmt(&mut self) -> ParseResult<Stmt> {
        self.consume(&TokenType::KReturn)?;
        if let Some(TokenType::Semicolon) = self.peek_token_type() {
            self.consume(&TokenType::Semicolon)?;
            Ok(Stmt::Return(None))
        } else {
            let expr = self.parse_expr()?;
            self.consume(&TokenType::Semicolon)?;
            Ok(Stmt::Return(Some(expr)))
        }
    }
}

impl<'a> Parser<'a> {
    fn parse_expr(&mut self) -> ParseResult<Expr> {
        if self.is_at_end() {
            return Err(self.make_eof());
        }

        match self.peek_token_type().unwrap() {
            TokenType::Literal(_) => self.parse_literal(),
            TokenType::Minus | TokenType::BitNot => self.parse_unary_expr(),
            TokenType::LeftParen => self.parse_grouped_expr(),
            tt => Err(ParserError {
                err_type: ParserErrorType::UnexpectedToken(tt.clone()),
                span: self.peek().map(|t| (t.begin, t.end)).unwrap(),
            }),
        }
    }

    fn parse_literal(&mut self) -> ParseResult<Expr> {
        if let Some(TokenType::Literal(lit @ Literal::Integer(_))) = self.peek_token_type() {
            let lit = lit.clone();
            let n = self.save_and_advance().unwrap();
            Ok(Expr::Constant(WithToken::new(lit, n)))
        } else {
            Err(ParserError {
                err_type: ParserErrorType::ExpectedAnother {
                    expected: TokenType::Literal(Literal::Integer(0)),
                    found: self.peek_token_type().cloned().unwrap_or(TokenType::Eof),
                },
                span: (
                    self.peek().map(|t| t.begin).unwrap_or(SourcePosition(0)),
                    self.peek().map(|t| t.end).unwrap_or(SourcePosition(0)),
                ),
            })
        }
    }

    fn unary_op_from_token(token: &Token) -> ParseResult<UnaryOp> {
        match &token.token_type {
            TokenType::Minus => Ok(UnaryOp::Negate),
            TokenType::BitNot => Ok(UnaryOp::BitNot),
            _ => Err(ParserError {
                err_type: ParserErrorType::UnexpectedToken(token.token_type.clone()),
                span: (token.begin, token.end),
            }),
        }
    }

    fn parse_unary_expr(&mut self) -> ParseResult<Expr> {
        let operator = Self::unary_op_from_token(self.peek().unwrap())?;
        let operator_token_index = self.save_and_advance().unwrap();

        let operand = self.parse_expr()?;
        Ok(Expr::Unary(UnaryExpr {
            operator: WithToken::new(operator, operator_token_index),
            operand: Box::new(operand),
        }))
    }

    fn parse_grouped_expr(&mut self) -> ParseResult<Expr> {
        self.consume(&TokenType::LeftParen)?;
        let expr = self.parse_expr()?;
        self.consume(&TokenType::RightParen)?;
        Ok(expr)
    }
}

#[cfg(test)]
mod parser_tests;
