use core::fmt;
use std::borrow::Cow;

use miette::{Error, LabeledSpan, miette};

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'a> {
    token_type: TokenType,
    original: &'a str,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    BandEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Bang,
    Slash,
    Equal,
    String,
    Number(f64),
    Identifier,
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let original = self.original;
        match self.token_type {
            TokenType::LeftParen => write!(f, "LEFT_PAREN {original} null"),
            TokenType::RightParen => write!(f, "RIGHT_PAREN {original} null"),
            TokenType::LeftBrace => write!(f, "LEFT_BRACE {original} null"),
            TokenType::RightBrace => write!(f, "RIGHT_BRACE {original} null"),
            TokenType::Comma => write!(f, "COMMA {original} null"),
            TokenType::Dot => write!(f, "DOT {original} null"),
            TokenType::Minus => write!(f, "MINUS {original} null"),
            TokenType::Plus => write!(f, "PLUS {original} null"),
            TokenType::Semicolon => write!(f, "SEMICOLON {original} null"),
            TokenType::Star => write!(f, "STAR {original} null"),
            TokenType::String => write!(f, "STRING {original} {}", Token::unescpaed(original)),
            TokenType::Number(n) => write!(f, "NUMBER {original} {n}"),
            TokenType::Identifier => write!(f, "IDENTIFIER {original} null"),
            TokenType::And => write!(f, "And {original} null"),
            TokenType::Class => write!(f, "CLASS {original} null"),
            TokenType::Else => write!(f, "ELSE {original} null"),
            TokenType::False => write!(f, "FALSE {original} null"),
            TokenType::For => write!(f, "FOR for {original}"),
            TokenType::Fun => write!(f, "FUN {original} null"),
            TokenType::If => write!(f, "IF {original} null"),
            TokenType::Nil => write!(f, "NIL {original} null"),
            TokenType::Or => write!(f, "OR {original} null"),
            TokenType::Return => write!(f, "RETURN {original} null"),
            TokenType::Super => write!(f, "SUPER {original} null"),
            TokenType::This => write!(f, "THIS {original} null"),
            TokenType::True => write!(f, "TRUE {original} null"),
            TokenType::Var => write!(f, "VAR {original} null"),
            TokenType::While => write!(f, "WHILE {original} null"),
            TokenType::BandEqual => write!(f, "BANDEQUAL {original} null"),
            TokenType::EqualEqual => write!(f, "EQUALEQUAL {original} null"),
            TokenType::LessEqual => write!(f, "LESSEQUAL {original} null"),
            TokenType::GreaterEqual => write!(f, "GREATEREQUAL {original} null"),
            TokenType::Less => write!(f, "Less {original} null"),
            TokenType::Greater => write!(f, "Greater {original} null"),
            TokenType::Slash => write!(f, "SLASH {original} null"),
            TokenType::Bang => write!(f, "BANG {original} null"),
            TokenType::Equal => write!(f, "EQUAL {original} null"),
        }
    }
}

impl Token<'_> {
    fn unescpaed<'a>(s: &'a str) -> Cow<'a, str> {
        todo!()
    }
}

pub struct Lexer<'a> {
    whole: &'a str,
    rest: &'a str,
    byte: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            whole: input,
            rest: input,
            byte: 0,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let c_str = &self.rest[..c.len_utf8()];
            let c_onwards = self.rest;
            self.byte += c.len_utf8();
            self.rest = chars.as_str();

            let get_token = |token_type: TokenType| {
                Some(Ok(Token {
                    token_type,
                    original: c_str,
                }))
            };

            enum Started {
                String,
                Number,
                Identifier,
                IfEqualElse(TokenType, TokenType),
            }

            let started = match c {
                '(' => return get_token(TokenType::LeftParen),
                ')' => return get_token(TokenType::RightParen),
                '{' => return get_token(TokenType::LeftBrace),
                '}' => return get_token(TokenType::RightBrace),
                ',' => return get_token(TokenType::Comma),
                '.' => return get_token(TokenType::Dot),
                '-' => return get_token(TokenType::Minus),
                '+' => return get_token(TokenType::Plus),
                ';' => return get_token(TokenType::Semicolon),
                '*' => return get_token(TokenType::Star),
                '/' => return get_token(TokenType::Slash),
                '"' => Started::String,
                '<' => Started::IfEqualElse(TokenType::LessEqual, TokenType::Equal),
                '>' => Started::IfEqualElse(TokenType::GreaterEqual, TokenType::Equal),
                '=' => Started::IfEqualElse(TokenType::EqualEqual, TokenType::Equal),
                '!' => Started::IfEqualElse(TokenType::BandEqual, TokenType::Bang),
                '0'..='9' => Started::Number,
                'a'..='z' | '_' => Started::Identifier,
                c if c.is_whitespace() => continue,
                _ => {
                    return Some(Err(miette::miette!(
                        labels = vec![LabeledSpan::at(
                            self.byte - c.len_utf8()..self.byte + c.len_utf8(),
                            "this character"
                        ),],
                        "Unexpected TokenType '{c}' in input",
                    )
                    .with_source_code(self.whole.to_string())));
                }
            };

            break match started {
                Started::String => todo!(),
                Started::Number => todo!(),
                Started::Identifier => todo!(),
                Started::IfEqualElse(yes, no) => {
                    self.rest = self.rest.trim_start();
                    let trimmed = c_onwards.len() - self.rest.len() - 1;
                    self.byte += trimmed;

                    if self.rest.starts_with("=") {
                        let span = &c_onwards[..c.len_utf8() + trimmed + 1];
                        self.rest = &self.rest[1..];
                        self.byte += 1;
                        Some(Ok(Token {
                            token_type: yes,
                            original: span,
                        }))
                    } else {
                        Some(Ok(Token {
                            token_type: no,
                            original: c_str,
                        }))
                    }
                }
            };
        }
    }
}
