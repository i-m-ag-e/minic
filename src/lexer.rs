pub mod lexer_error;
pub mod token;

use crate::lexer::lexer_error::LexerError;
use crate::lexer::token::Token;
use crate::lexer_error::LexerErrorType;
use crate::source_file::{SourceFile, SourcePosition};
use crate::symbol::SymbolTable;
use anyhow;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::{iter::Peekable, str::Chars};
use token::{Literal, TokenType};

macro_rules! single_char_tok {
    ($self: ident, $t: expr) => {{
        $self.cursor.advance();
        Ok($self.make_token($t))
    }};
}

macro_rules! multi_char_tok {
    ( $self: ident, $orig: expr; $( $c: literal => $tt: expr $( =>= $c1: literal => $tt1: expr )? ),+ $(,)? ) => {
        match $self.cursor.peek() {
            $(
                Some($c) => {
                    $(
                        match $self.cursor.peek_next() {
                            Some($c1) => {
                                $self.cursor.advance();
                                $self.cursor.advance();
                                return Ok($self.make_token($tt1));
                            },
                            _ => {}
                        };
                    )?
                    $self.cursor.advance();
                    Ok($self.make_token($tt))
                }
            )+
            _ => Ok($self.make_token($orig))
        }
    };
}

lazy_static! {
    static ref KW_LIST: Vec<(&'static str, TokenType)> = vec![
        ("true", TokenType::Literal(Literal::Bool(true))),
        ("false", TokenType::Literal(Literal::Bool(false))),
        ("if", TokenType::KIf),
        ("else", TokenType::KElse),
        ("while", TokenType::KWhile),
        ("for", TokenType::KFor),
        ("return", TokenType::KReturn),
    ];
    static ref KEYWORDS: HashMap<&'static str, TokenType> =
        HashMap::from_iter(KW_LIST.iter().cloned());
}

#[derive(Debug)]
struct Cursor<'a> {
    source: &'a str,
    chars: Peekable<Chars<'a>>,
    prev_char: Option<char>,
    position: SourcePosition,
}

impl<'a> Cursor<'a> {
    pub fn new(source: &'a SourceFile) -> Self {
        let mut chars = source.chars().peekable();
        let first_char = chars.peek().copied();
        Cursor {
            source: source,
            chars,
            prev_char: first_char,
            position: SourcePosition::new(0, 1, 1),
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
            self.chars.peek().copied()
        }
    }

    fn advance(&mut self) -> Option<char> {
        if self.is_at_end() {
            return None;
        }

        let current_char = self.prev_char;
        self.prev_char = self.chars.next();
        self.position = self.position.increment(current_char.unwrap());
        current_char
    }

    fn substr(&self, start: usize, end: usize) -> &str {
        &self.source[start..end]
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    cursor: Cursor<'a>,
    symbol_table: &'a mut SymbolTable,
    token_start: SourcePosition,
}

pub type LexerResult<T> = anyhow::Result<T, LexerError>;

impl<'a> Lexer<'a> {
    pub fn new(source: &'a SourceFile, symbol_table: &'a mut SymbolTable) -> Self {
        Lexer {
            cursor: Cursor::new(source),
            symbol_table,
            token_start: SourcePosition::new(0, 1, 1),
        }
    }
}

impl<'a> Lexer<'a> {
    fn skip_whitespace_and_comments(&mut self) {
        while let Some(c) = self.cursor.peek() {
            if !c.is_whitespace() {
                break;
            }
            self.cursor.advance();
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

    fn make_token(&self, tok_type: TokenType) -> Token {
        Token {
            token_type: tok_type,
            begin: self.token_start,
            end: self.cursor.position,
        }
    }

    fn scan_number_literal(&mut self) -> LexerResult<Token> {
        let mut number_str = String::new();
        let mut is_float = false;
        while let Some(c) = self.cursor.peek() {
            if c.is_digit(10) || c == '.' {
                is_float = c == '.';
                number_str.push(c);
                self.cursor.advance();
            } else {
                break;
            }
        }

        // println!("number str: {:?}", number_str);

        if self
            .cursor
            .peek()
            .map_or(false, |c| c.is_alphabetic() || c == '_')
        {
            return Err(LexerError::new(
                self.token_start,
                self.cursor.position,
                LexerErrorType::InvalidNumberLiteral,
            ));
        }

        if is_float {
            let value: f64 = number_str.parse().unwrap();
            Ok(self.make_token(TokenType::Literal(Literal::Float(value))))
        } else {
            let value: i64 = number_str.parse().unwrap();
            Ok(self.make_token(TokenType::Literal(Literal::Integer(value))))
        }
    }

    fn scan_identifier_or_kw(&mut self) -> LexerResult<Token> {
        while let Some(c) = self.cursor.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.cursor.advance();
            } else {
                break;
            }
        }

        let ident = self
            .cursor
            .substr(self.token_start.pos, self.cursor.position.pos);

        if let "true" | "false" = ident {
            let value = ident == "true";
            return Ok(self.make_token(TokenType::Literal(Literal::Bool(value))));
        } else if let Some(tok_type) = KEYWORDS.get(ident) {
            Ok(self.make_token(tok_type.clone()))
        } else {
            let symbol = self.symbol_table.intern(ident);
            Ok(self.make_token(TokenType::Identifier(symbol)))
        }
    }

    fn scan_next_token(&mut self) -> LexerResult<Token> {
        self.skip_whitespace_and_comments();
        // println!("skipped ws");
        self.token_start = self.cursor.position;

        if self.cursor.is_at_end() {
            return Ok(self.make_token(TokenType::Eof));
        }

        let next_char = self.cursor.peek().unwrap();

        match next_char {
            ',' => single_char_tok!(self, TokenType::Comma),
            '.' => single_char_tok!(self, TokenType::Dot),
            '{' => single_char_tok!(self, TokenType::LeftBrace),
            '(' => single_char_tok!(self, TokenType::LeftParen),
            '[' => single_char_tok!(self, TokenType::LeftSquare),
            '}' => single_char_tok!(self, TokenType::RightBrace),
            ')' => single_char_tok!(self, TokenType::RightParen),
            ']' => single_char_tok!(self, TokenType::RightSquare),
            ';' => single_char_tok!(self, TokenType::Semicolon),

            '&' => multi_char_tok!(self, TokenType::BitAnd;
                '&' => TokenType::And,
                '=' => TokenType::BitAndAssign
            ),
            '-' => multi_char_tok!(self, TokenType::Minus;
                '=' => TokenType::MinusAssign,
                '>' => TokenType::Arrow,
                '-' => TokenType::Decrement
            ),
            '*' => multi_char_tok!(self, TokenType::Asterisk;
                '=' => TokenType::AsteriskAssign
            ),
            '|' => multi_char_tok!(self, TokenType::BitOr;
                '|' => TokenType::Or,
                '=' => TokenType::BitOrAssign
            ),
            '^' => multi_char_tok!(self, TokenType::BitXor;
                '=' => TokenType::BitXorAssign
            ),
            '=' => multi_char_tok!(self, TokenType::Equal;
                '=' => TokenType::DoubleEqual
            ),
            '>' => multi_char_tok!(self, TokenType::Greater;
                '=' => TokenType::GreaterEqual,
                '>' => TokenType::RightShift =>= '>' => TokenType::RightShiftAssign
            ),
            '<' => multi_char_tok!(self, TokenType::Less;
                '=' => TokenType::LessEqual,
                '<' => TokenType::LeftShift =>= '<' => TokenType::LeftShiftAssign
            ),
            '!' => multi_char_tok!(self, TokenType::Not;
                '=' => TokenType::NotEqual
            ),
            '%' => multi_char_tok!(self, TokenType::Percent;
                '=' => TokenType::PercentAssign
            ),
            '+' => multi_char_tok!(self, TokenType::Plus;
                '=' => TokenType::PlusAssign,
                '+' => TokenType::Increment
            ),
            '/' => multi_char_tok!(self, TokenType::Slash;
                '=' => TokenType::SlashAssign
            ),

            '0'..='9' => self.scan_number_literal(),

            c if c.is_alphabetic() || c == '_' => self.scan_identifier_or_kw(),

            _ => Err(LexerError::new(
                self.token_start,
                self.cursor.position,
                lexer_error::LexerErrorType::UnexpectedCharacter(
                    self.cursor.peek().unwrap_or('\0'),
                ),
            )),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = anyhow::Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let peek = self.cursor.peek();
        // println!(
        //     "next called at pos {:?} (peek: {:?})",
        //     self.cursor.position, peek
        // );
        if self.cursor.is_at_end() {
            return None;
        }
        Some(self.scan_next_token())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_file::SourceFile;

    macro_rules! token {
        // token!(<type>; <line1>::<col1>::<pos1> -> <line2>::<col2>::<pos2>)
        ($t: expr; $l1: literal :: $c1: literal :: $p1:  literal -> $l2: literal :: $c2: literal :: $p2: literal) => {
            Token {
                token_type: $t,
                begin: SourcePosition::new($p1, $l1, $c1),
                end: SourcePosition::new($p2, $l2, $c2),
            }
        };
    }

    #[test]
    fn test_punctuation() {
        let source = SourceFile::new(
            "test_punctuation".to_string(),
            ", . { ( [ \n } ) ] ;".to_string(),
        );
        let mut symbol_table = SymbolTable::new();

        let lexer = Lexer::new(&source, &mut symbol_table);
        let expected_tokens = vec![
            token!(TokenType::Comma; 1::1::0 -> 1::2::1),
            token!(TokenType::Dot; 1::3::2 -> 1::4::3),
            token!(TokenType::LeftBrace; 1::5::4 -> 1::6::5),
            token!(TokenType::LeftParen; 1::7::6 -> 1::8::7),
            token!(TokenType::LeftSquare; 1::9::8 -> 1::10::9),
            token!(TokenType::RightBrace; 2::1::10 -> 2::2::11),
            token!(TokenType::RightParen; 2::3::12 -> 2::4::13),
            token!(TokenType::RightSquare; 2::5::14 -> 2::6::15),
            token!(TokenType::Semicolon; 2::7::16 -> 2::8::17),
        ];

        let tokens = lexer.collect::<Result<Vec<_>, _>>();
        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();
        assert_eq!(tokens.len(), expected_tokens.len());
        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(&expected_tokens[i], token);
        }
    }
}
