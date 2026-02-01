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
    ( $self: ident, $orig: expr; $( $c: literal => $tt: expr $( =>= $c1: literal => $tt1: expr )? ),+ $(,)? ) => {{
        $self.cursor.advance();
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
    }};
}

lazy_static! {
    static ref KW_LIST: Vec<(&'static str, TokenType)> = vec![
        ("else", TokenType::KElse),
        ("false", TokenType::Literal(Literal::Bool(false))),
        ("for", TokenType::KFor),
        ("if", TokenType::KIf),
        ("int", TokenType::KInt),
        ("return", TokenType::KReturn),
        ("true", TokenType::Literal(Literal::Bool(true))),
        ("void", TokenType::KVoid),
        ("while", TokenType::KWhile),
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
        let first_char = chars.next();
        Cursor {
            source: source,
            chars,
            prev_char: first_char,
            position: SourcePosition(0),
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
        self.position = self.position + 1;
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
            token_start: SourcePosition(0),
        }
    }
}

impl<'a> Lexer<'a> {
    fn skip_whitespace_and_comments(&mut self) -> LexerResult<()> {
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
                self.skip_whitespace_and_comments()?;
            } else if let Some('*') = self.cursor.peek_next() {
                self.cursor.advance();
                self.cursor.advance();

                while let Some(c) = self.cursor.peek() {
                    if c == '*' {
                        if let Some('/') = self.cursor.peek_next() {
                            self.cursor.advance(); // consume '*'
                            self.cursor.advance(); // consume '/'
                            break;
                        }
                    }
                    self.cursor.advance();

                    if self.cursor.is_at_end() {
                        return Err(LexerError::new(
                            self.token_start,
                            self.cursor.position,
                            LexerErrorType::UnterminatedComment,
                        ));
                    }
                }
                self.skip_whitespace_and_comments()?;
            }
        }

        Ok(())
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
                is_float = is_float || c == '.';
                number_str.push(c);
                self.cursor.advance();
            } else {
                break;
            }
        }

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
            .substr(self.token_start.0, self.cursor.position.0);

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
        self.skip_whitespace_and_comments()?;
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
            '~' => multi_char_tok!(self, TokenType::BitNot;
                '=' => TokenType::BitNotAssign
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
                '>' => TokenType::RightShift =>= '=' => TokenType::RightShiftAssign
            ),
            '<' => multi_char_tok!(self, TokenType::Less;
                '=' => TokenType::LessEqual,
                '<' => TokenType::LeftShift =>= '=' => TokenType::LeftShiftAssign
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
        // let peek = self.cursor.peek();
        // println!(
        //     "next called at pos {:?} (peek: {:?})",
        //     self.cursor.position, peek
        // );
        if self.cursor.is_at_end() {
            return None;
        }
        let tok = self.scan_next_token();
        if let Some(TokenType::Eof) = tok.as_ref().ok().map(|t| &t.token_type) {
            None
        } else {
            Some(tok)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_file::SourceFile;

    macro_rules! token {
        // token!(<type>; <line1>::<col1>::<pos1> -> <line2>::<col2>::<pos2>)
        // -> (Token, line1, col1, line2, col2)
        ($t: expr; $l1: literal :: $c1: literal :: $p1:  literal -> $l2: literal :: $c2: literal :: $p2: literal) => {
            (
                Token {
                    token_type: $t,
                    begin: SourcePosition($p1),
                    end: SourcePosition($p2),
                },
                $l1,
                $c1,
                $l2,
                $c2,
            )
        };
    }

    fn test_tokens(
        expected: Vec<(Token, usize, usize, usize, usize)>,
        source: &SourceFile,
        symbol_table: &mut SymbolTable,
        print_tokens: bool,
    ) {
        let lexer = Lexer::new(source, symbol_table);
        let tokens = lexer.collect::<Result<Vec<_>, _>>();
        assert!(tokens.is_ok());
        if print_tokens {
            println!("Tokens: {:#?}", tokens);
        }
        let tokens = tokens.unwrap();
        assert_eq!(tokens.len(), expected.len());
        for (i, token) in tokens.iter().enumerate() {
            println!("i: {}, token: {:?}", i, token);
            assert_eq!(token, &expected[i].0);
            let line_col_start = source.line_col(token.begin.0);
            let line_col_end = source.line_col(token.end.0);
            assert_eq!(line_col_start.0, expected[i].1);
            assert_eq!(line_col_start.1, expected[i].2);
            assert_eq!(line_col_end.0, expected[i].3);
            assert_eq!(line_col_end.1, expected[i].4);
        }
    }

    #[test]
    fn test_punctuation() {
        let source = SourceFile::new(
            "test_punctuation".to_string(),
            ", . { ( [ \n } ) ] ;".to_string(),
        );
        let mut symbol_table = SymbolTable::new();

        let expected_tokens = vec![
            token!(TokenType::Comma; 1::1::0 -> 1::2::1),
            token!(TokenType::Dot; 1::3::2 -> 1::4::3),
            token!(TokenType::LeftBrace; 1::5::4 -> 1::6::5),
            token!(TokenType::LeftParen; 1::7::6 -> 1::8::7),
            token!(TokenType::LeftSquare; 1::9::8 -> 1::10::9),
            token!(TokenType::RightBrace; 2::2::12 -> 2::3::13),
            token!(TokenType::RightParen; 2::4::14 -> 2::5::15),
            token!(TokenType::RightSquare; 2::6::16 -> 2::7::17),
            token!(TokenType::Semicolon; 2::8::18 -> 2::9::19),
        ];

        test_tokens(expected_tokens, &source, &mut symbol_table, false);
    }

    #[test]
    fn test_operators() {
        let ops = [
            "&& -> * *= ",
            "& &= | |= ^ ^= ",
            "-- == = ",
            "> >= ++ << <<= < <=",
            "- -= ! != || % %=",
            "+ += / >> >>= / /= ",
            "~ ~=",
        ];
        let source = SourceFile::new("test_operators".to_string(), ops.join("\n").to_string());
        let mut symbol_table = SymbolTable::new();

        let expected_tokens = vec![
            token!(TokenType::And; 1::1::0 -> 1::3::2),
            token!(TokenType::Arrow; 1::4::3 -> 1::6::5),
            token!(TokenType::Asterisk; 1::7::6 -> 1::8::7),
            token!(TokenType::AsteriskAssign; 1::9::8 -> 1::11::10),
            token!(TokenType::BitAnd; 2::1::12 -> 2::2::13),
            token!(TokenType::BitAndAssign; 2::3::14 -> 2::5::16),
            token!(TokenType::BitOr; 2::6::17 -> 2::7::18),
            token!(TokenType::BitOrAssign; 2::8::19 -> 2::10::21),
            token!(TokenType::BitXor; 2::11::22 -> 2::12::23),
            token!(TokenType::BitXorAssign; 2::13::24 -> 2::15::26),
            token!(TokenType::Decrement; 3::1::28 -> 3::3::30),
            token!(TokenType::DoubleEqual; 3::4::31 -> 3::6::33),
            token!(TokenType::Equal; 3::7::34 -> 3::8::35),
            token!(TokenType::Greater; 4::1::37 -> 4::2::38),
            token!(TokenType::GreaterEqual; 4::3::39 -> 4::5::41),
            token!(TokenType::Increment; 4::6::42 -> 4::8::44),
            token!(TokenType::LeftShift; 4::9::45 -> 4::11::47),
            token!(TokenType::LeftShiftAssign; 4::12::48 -> 4::15::51),
            token!(TokenType::Less; 4::16::52 -> 4::17::53),
            token!(TokenType::LessEqual; 4::18::54 -> 4::20::56),
            token!(TokenType::Minus; 5::1::57 -> 5::2::58),
            token!(TokenType::MinusAssign; 5::3::59 -> 5::5::61),
            token!(TokenType::Not; 5::6::62 -> 5::7::63),
            token!(TokenType::NotEqual; 5::8::64 -> 5::10::66),
            token!(TokenType::Or; 5::11::67 -> 5::13::69),
            token!(TokenType::Percent; 5::14::70 -> 5::15::71),
            token!(TokenType::PercentAssign; 5::16::72 -> 5::18::74),
            token!(TokenType::Plus; 6::1::75 -> 6::2::76),
            token!(TokenType::PlusAssign; 6::3::77 -> 6::5::79),
            token!(TokenType::Slash; 6::6::80 -> 6::7::81),
            token!(TokenType::RightShift; 6::8::82 -> 6::10::84),
            token!(TokenType::RightShiftAssign; 6::11::85 -> 6::14::88),
            token!(TokenType::Slash; 6::15::89 -> 6::16::90),
            token!(TokenType::SlashAssign; 6::17::91 -> 6::19::93),
            token!(TokenType::BitNot; 7::1::95 -> 7::2::96),
            token!(TokenType::BitNotAssign; 7::3::97 -> 7::5::99),
        ];

        test_tokens(expected_tokens, &source, &mut symbol_table, true);
    }

    #[test]
    fn test_number_literals() {
        let source = SourceFile::new(
            "test_number_literals".to_string(),
            "123 45.67 0.001 890".to_string(),
        );
        let mut symbol_table = SymbolTable::new();
        let expected_tokens = vec![
            token!(TokenType::Literal(Literal::Integer(123)); 1::1::0 -> 1::4::3),
            token!(TokenType::Literal(Literal::Float(45.67)); 1::5::4 -> 1::10::9),
            token!(TokenType::Literal(Literal::Float(0.001)); 1::11::10 -> 1::16::15),
            token!(TokenType::Literal(Literal::Integer(890)); 1::17::16 -> 1::20::19),
        ];
        test_tokens(expected_tokens, &source, &mut symbol_table, true);
    }

    #[test]
    fn test_identifiers_and_keywords() {
        let source = SourceFile::new(
            "test_identifiers_and_keywords".to_string(),
            "if else myVar another_var true false return".to_string(),
        );
        let mut symbol_table = SymbolTable::new();
        let expected_tokens = vec![
            token!(TokenType::KIf; 1::1::0 -> 1::3::2),
            token!(TokenType::KElse; 1::4::3 -> 1::8::7),
            token!(TokenType::Identifier(symbol_table.intern("myVar")); 1::9::8 -> 1::14::13),
            token!(TokenType::Identifier(symbol_table.intern("another_var")); 1::15::14 -> 1::26::25),
            token!(TokenType::Literal(Literal::Bool(true)); 1::27::26 -> 1::31::30),
            token!(TokenType::Literal(Literal::Bool(false)); 1::32::31 -> 1::37::36),
            token!(TokenType::KReturn; 1::38::37 -> 1::44::43),
        ];
        test_tokens(expected_tokens, &source, &mut symbol_table, true);
    }
}
