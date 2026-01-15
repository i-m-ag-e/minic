mod lexer_error;
mod token;
use crate::lexer::lexer_error::LexerError;
use crate::source_file::SourceFile;
use crate::symbol::{Symbol, SymbolTable};
use anyhow;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::{iter::Peekable, str::Chars};
use token::{Literal, Token};

lazy_static! {
    static ref KW_LIST: Vec<(&'static str, Token)> = vec![
        ("true", Token::Literal(Literal::Bool(true))),
        ("false", Token::Literal(Literal::Bool(false))),
        ("if", Token::KIf),
        ("else", Token::KElse),
        ("while", Token::KWhile),
        ("for", Token::KFor),
        ("return", Token::KReturn),
    ];
    static ref KEYWORDS: HashMap<&'static str, Token> = HashMap::from_iter(KW_LIST.iter().cloned());
}

#[derive(Debug)]
struct Cursor<'a> {
    source: Peekable<Chars<'a>>,
    prev_char: Option<char>,
    pos: usize,
    line: usize,
    column: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(source: &'a SourceFile) -> Self {
        let mut source = source.chars().peekable();
        let first_char = source.peek().copied();
        Cursor {
            source,
            prev_char: first_char,
            pos: 0,
            line: 1,
            column: 1,
        }
    }

    fn is_at_end(&mut self) -> bool {
        self.prev_char.is_none()
    }

    fn peek(&mut self) -> Option<char> {
        self.prev_char
    }

    fn peek_next(&mut self) -> Option<char> {
        if self.is_at_end() {
            None
        } else {
            self.source.peek().copied()
        }
    }

    fn advance(&mut self) -> Option<char> {
        if self.is_at_end() {
            return None;
        }

        let current_char = self.prev_char;
        self.prev_char = self.source.next();
        self.pos += 1;
        if let Some(c) = current_char {
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
        current_char
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    cursor: Cursor<'a>,
    symbol_table: &'a mut SymbolTable,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a SourceFile, symbol_table: &'a mut SymbolTable) -> Self {
        Lexer {
            cursor: Cursor::new(source),
            symbol_table,
        }
    }
}

impl<'a> Lexer<'a> {
    fn skip_whitespace_and_comments(&mut self) {
        while let Some(c) = self.cursor.peek() {
            if c.is_whitespace() {
                self.cursor.advance();
            }
        }

        if let Some('/') = self.cursor.peek() {
            if let Some('/') = self.cursor.peek_next() {
                while let Some(c) = self.cursor.peek() {
                    if c == '\n' {
                        break;
                    }
                    self.cursor.advance();
                }
                self.skip_whitespace_and_comments();
            }
        }
    }

    fn scan_number_literal(&mut self) -> Token {
        let mut number_str = String::new();
        let mut is_float = false;
        while let Some(c) = self.cursor.peek() {
            if c.is_digit(10) || c == '.' {
                is_float = c == '.';
                number_str.push(c);
                self.cursor.advance();
            }
        }
        if is_float {
            let value: f64 = number_str.parse().unwrap();
            Token::Literal(Literal::Float(value))
        } else {
            let value: i64 = number_str.parse().unwrap();
            Token::Literal(Literal::Integer(value))
        }
    }

    fn scan_next_token(&mut self) -> anyhow::Result<Token, LexerError> {
        self.skip_whitespace_and_comments();
        Err(LexerError {
            line: self.cursor.line,
            column: self.cursor.column,
            err_type: lexer_error::LexerErrorType::UnexpectedCharacter(
                self.cursor.peek().unwrap_or('\0'),
            ),
        })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = anyhow::Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cursor.is_at_end() {
            return None;
        }
        Some(self.scan_next_token())
    }
}
