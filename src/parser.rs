pub mod parser_error;

use crate::ast::expr::{
    AssignExpr, BinaryExpr, BinaryOp, ConditionalExpr, Expr, UnaryExpr, UnaryOp,
};
use crate::ast::stmt::{IfStmt, Label};
use crate::ast::{BlockItem, FunctionDef, VarDeclaration};
use crate::ast::{Program, stmt::Stmt};
use crate::lexer::token::{Literal, TokenID};
use crate::source_file::SourcePosition;
use crate::symbol::{Symbol, SymbolTable};
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
    symbol_table: &'a SymbolTable,
    used_tokens: &'a mut Vec<bool>,
    used_count: usize,
}

pub type ParseResult<T> = Result<T, ParserError>;

impl<'a> Parser<'a> {
    pub fn new(
        tokens: &'a [Token],
        used_tokens: &'a mut Vec<bool>,
        symbol_table: &'a SymbolTable,
    ) -> Self {
        Self {
            tokens,
            index: 0,
            symbol_table,
            used_tokens,
            used_count: 0,
        }
    }

    fn save_previous(&mut self) -> TokenID {
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

    fn peek(&self) -> Option<Token> {
        self.tokens.get(self.index).copied()
    }

    fn peek_next(&self) -> Option<Token> {
        self.tokens.get(self.index + 1).copied()
    }

    fn peek_token_type(&self) -> Option<TokenType> {
        self.peek().map(|token| token.token_type)
    }

    fn peek_next_token_type(&self) -> Option<TokenType> {
        self.peek_next().map(|token| token.token_type)
    }

    fn advance(&mut self) -> Option<Token> {
        if self.is_at_end() {
            None
        } else {
            let token = self.tokens[self.index];
            self.index += 1;
            Some(token)
        }
    }

    fn advance_if<F>(&mut self, condition: F) -> Option<Token>
    where
        F: Fn(&Token) -> bool,
    {
        if let Some(token) = self.peek() {
            if condition(&token) {
                return self.advance();
            }
        }
        None
    }

    fn consume_if<F, E>(&mut self, condition: F, error: E) -> ParseResult<Token>
    where
        F: Fn(TokenType) -> bool,
        E: Fn(TokenType) -> ParserErrorType,
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
                    span: found_token.span(),
                })
            }
        }
    }

    fn consume(&mut self, expected: TokenType) -> ParseResult<Token> {
        self.consume_if(
            |token_type| token_type == expected,
            |found| ParserErrorType::ExpectedAnother {
                expected: expected,
                found: found,
            },
        )
    }

    fn consume_unary_operator(&mut self) -> ParseResult<UnaryOp> {
        let op_token = self.peek().unwrap();
        UnaryOp::try_from(op_token.token_type).map_err(|_| ParserError {
            err_type: ParserErrorType::UnexpectedToken(op_token.token_type.clone()),
            span: (op_token.begin, op_token.end),
        })
    }

    fn advance_if_eq(&mut self, expected: &TokenType) -> Option<Token> {
        self.advance_if(|token| &token.token_type == expected)
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
        self.consume(TokenType::KInt)?;

        self.consume_if(
            |tt| matches!(tt, TokenType::Identifier(_)),
            |error| ParserErrorType::ExpectedAnother {
                expected: TokenType::Identifier(Symbol(0)),
                found: error.clone(),
            },
        )?;
        let func_name = self.save_previous();

        self.consume(TokenType::LeftParen)?;
        self.consume(TokenType::KVoid)?;
        self.consume(TokenType::RightParen)?;
        self.consume(TokenType::LeftBrace)?;

        let mut body = Vec::new();
        while self.advance_if_eq(&TokenType::RightBrace).is_none() {
            if self.is_at_end() {
                return Err(self.make_eof());
            }
            let stmt = self.parse_block_item()?;
            body.push(stmt);
        }

        Ok(FunctionDef {
            name: WithToken::new("main".to_string(), func_name),
            body: Some(body),
        })
    }

    fn parse_block_item(&mut self) -> ParseResult<BlockItem> {
        match self.peek_token_type() {
            Some(TokenType::KInt) => self.parse_declaration().map(BlockItem::Decl),
            _ => self.parse_stmt().map(BlockItem::Stmt),
        }
    }

    fn parse_declaration(&mut self) -> ParseResult<VarDeclaration> {
        self.consume(TokenType::KInt)?;

        let name = self.consume_if(
            |tt| matches!(tt, TokenType::Identifier(_)),
            |error| ParserErrorType::ExpectedAnotherString {
                expected: "<identifier>",
                found: error.clone(),
            },
        )?;
        let &TokenType::Identifier(name) = &name.token_type else {
            unreachable!()
        };
        let name_token_id = self.save_previous();

        let initializer = if let Some(TokenType::Equal) = self.peek_token_type() {
            let eq_token = self.save_and_advance().unwrap();
            Some(WithToken::new(self.parse_expr()?, eq_token))
        } else {
            None
        };

        self.consume(TokenType::Semicolon)?;

        Ok(VarDeclaration {
            name: WithToken::new(name, name_token_id),
            initializer,
        })
    }
}

impl<'a> Parser<'a> {
    fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        match self.peek_token_type() {
            Some(TokenType::KGoto) => self.parse_goto_stmt(),
            Some(TokenType::KIf) => self.parse_if_stmt(),
            Some(TokenType::KReturn) => self.parse_return_stmt(),
            Some(TokenType::Identifier(_))
                if self.peek_next_token_type() == Some(TokenType::Colon) =>
            {
                self.parse_label()
            }
            Some(TokenType::Semicolon) => {
                self.advance().unwrap();
                Ok(Stmt::Null)
            }
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_expr_stmt(&mut self) -> ParseResult<Stmt> {
        let expr = self.parse_expr()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_goto_stmt(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::KGoto)?;

        let label_token = self.consume_if(
            |tt| matches!(tt, TokenType::Identifier(_)),
            |error| ParserErrorType::ExpectedAnotherString {
                expected: "<label>",
                found: error.clone(),
            },
        )?;
        let TokenType::Identifier(label) = label_token.token_type else {
            unreachable!()
        };
        let label_name = self
            .symbol_table
            .resolve(label)
            .expect("goto label must have been interned during lexing")
            .to_string();
        let label_token_id = self.save_previous();

        self.consume(TokenType::Semicolon)?;

        Ok(Stmt::Goto(WithToken::new(label_name, label_token_id)))
    }

    fn parse_if_stmt(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::KIf)?;
        let if_token = self.save_previous();

        self.consume(TokenType::LeftParen)?;
        let condition = self.parse_expr()?;
        self.consume(TokenType::RightParen)?;

        let then_stmt = Box::new(self.parse_stmt()?);

        let else_stmt = if let Some(TokenType::KElse) = self.peek_token_type() {
            self.consume(TokenType::KElse)?;
            let else_token = self.save_previous();
            Some(WithToken::new(Box::new(self.parse_stmt()?), else_token))
        } else {
            None
        };

        Ok(Stmt::If(IfStmt {
            condition: WithToken::new(condition, if_token),
            then_stmt,
            else_stmt,
        }))
    }

    fn parse_label(&mut self) -> ParseResult<Stmt> {
        let Some(Token {
            token_type: TokenType::Identifier(sym),
            ..
        }) = self.advance()
        else {
            unreachable!()
        };
        let label_token_id = self.save_previous();
        self.consume(TokenType::Colon)?;

        let label_str = self
            .symbol_table
            .resolve(sym)
            .expect("label identifier must have been interned during lexing")
            .to_string();

        let stmt = self.parse_stmt()?;

        Ok(Stmt::Label(Label {
            name: WithToken::new(label_str, label_token_id),
            next_stmt: Box::new(stmt),
        }))
    }

    fn parse_return_stmt(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::KReturn)?;
        let ret_token = self.save_previous();

        if let Some(TokenType::Semicolon) = self.peek_token_type() {
            self.consume(TokenType::Semicolon)?;
            Ok(Stmt::Return(WithToken::new(None, ret_token)))
        } else {
            let expr = self.parse_expr()?;
            self.consume(TokenType::Semicolon)?;
            Ok(Stmt::Return(WithToken::new(Some(expr), ret_token)))
        }
    }
}

impl<'a> Parser<'a> {
    fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_expr_with_precedence(0)
    }

    fn parse_expr_with_precedence(&mut self, min_prec: u8) -> ParseResult<Expr> {
        let mut left = self.parse_factor()?;
        while let Ok(binop) = BinaryOp::try_from(self.peek_token_type().unwrap())
            && binop.precedence().unwrap_or(0) >= min_prec
        {
            let prec = binop.precedence().unwrap_or(0);

            if let Some(TokenType::Equal) = self.peek_token_type() {
                let eq = self.save_and_advance().unwrap();
                let right = self.parse_expr_with_precedence(prec)?; // not +1 since we want to keep consuming '='s
                left = Expr::Assignment(AssignExpr {
                    target: Box::new(left),
                    eq_token: WithToken::new((), eq),
                    right: Box::new(right),
                });
            }
            // Handle compound assignment operators like +=, -=, etc.
            // binop is the binary operator corresponding to the compound assignment (e.g., + for +=)
            else if let Some(binop) = binop.compound_assign_to_binop() {
                let eq = self.save_and_advance().unwrap();
                let right = self.parse_expr_with_precedence(prec)?; // again, not +1

                // right = left <op> right
                let right = Expr::Binary(BinaryExpr {
                    left: Box::new(left.clone()),
                    operator: WithToken::new(binop, eq),
                    right: Box::new(right),
                });

                left = Expr::Assignment(AssignExpr {
                    target: Box::new(left),
                    eq_token: WithToken::new((), eq),
                    right: Box::new(right),
                });
            } else if let Some(TokenType::QuestionMark) = self.peek_token_type() {
                let qm_token = self.save_and_advance().unwrap();
                let then_expr = self.parse_expr()?;
                self.consume(TokenType::Colon)?;
                let colon_token = self.save_previous();
                let else_expr = self.parse_expr_with_precedence(prec)?; // not +1 since ternary is right associative

                left = Expr::Conditional(ConditionalExpr {
                    condition: Box::new(left),
                    then_expr: WithToken::new(Box::new(then_expr), qm_token),
                    else_expr: WithToken::new(Box::new(else_expr), colon_token),
                });
            } else {
                let operator = binop;
                let operator_token_index = self.save_and_advance().unwrap();

                let right = self.parse_expr_with_precedence(prec + 1)?;
                let binary_expr = BinaryExpr {
                    left: Box::new(left),
                    operator: WithToken::new(operator, operator_token_index),
                    right: Box::new(right),
                };
                left = Expr::Binary(binary_expr);
            }
        }
        Ok(left)
    }

    fn parse_factor(&mut self) -> ParseResult<Expr> {
        if self.is_at_end() {
            return Err(self.make_eof());
        }

        let expr = match self.peek_token_type().unwrap() {
            TokenType::Literal(_) => self.parse_literal(),
            TokenType::LeftParen => self.parse_grouped_expr(),
            TokenType::Identifier(name) => {
                let token = self.save_and_advance().unwrap();
                Ok(Expr::Variable(WithToken::new(name, token)))
            }
            tt if UnaryOp::try_from(tt).is_ok() => self.parse_unary_expr(),
            tt => Err(ParserError {
                err_type: ParserErrorType::UnexpectedToken(tt),
                span: self.peek().map(|t| (t.begin, t.end)).unwrap(),
            }),
        }?;

        if let Some(TokenType::Decrement | TokenType::Increment) = self.peek_token_type() {
            let unary_op = self.consume_unary_operator()?;
            let operator_token_index = self.save_and_advance().unwrap();
            Ok(Expr::Unary(UnaryExpr {
                operator: WithToken::new(unary_op, operator_token_index),
                operand: Box::new(expr),
                postfix: true,
            }))
        } else {
            Ok(expr)
        }
    }

    fn parse_literal(&mut self) -> ParseResult<Expr> {
        if let Some(TokenType::Literal(lit @ Literal::Integer(_))) = self.peek_token_type() {
            let n = self.save_and_advance().unwrap();
            Ok(Expr::Constant(WithToken::new(lit, n)))
        } else {
            Err(ParserError {
                err_type: ParserErrorType::ExpectedAnother {
                    expected: TokenType::Literal(Literal::Integer(0)),
                    found: self.peek_token_type().unwrap_or(TokenType::Eof),
                },
                span: (
                    self.peek().map(|t| t.begin).unwrap_or(SourcePosition(0)),
                    self.peek().map(|t| t.end).unwrap_or(SourcePosition(0)),
                ),
            })
        }
    }

    fn parse_unary_expr(&mut self) -> ParseResult<Expr> {
        let operator = self.consume_unary_operator()?;
        let operator_token_index = self.save_and_advance().unwrap();

        let operand = self.parse_factor()?;
        Ok(Expr::Unary(UnaryExpr {
            operator: WithToken::new(operator, operator_token_index),
            operand: Box::new(operand),
            postfix: false,
        }))
    }

    fn parse_grouped_expr(&mut self) -> ParseResult<Expr> {
        self.consume(TokenType::LeftParen)?;
        let expr = self.parse_expr()?;
        self.consume(TokenType::RightParen)?;
        Ok(expr)
    }
}

#[cfg(test)]
mod parser_tests;
