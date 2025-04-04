use core::fmt;
use miette::{Diagnostic, Error, LabeledSpan, SourceSpan};
use std::borrow::Cow;
use thiserror::Error;

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected token {token}")]
pub struct TokenError {
    // The `Source` that miette will use.
    #[source_code]
    src: String,

    pub token: char,

    #[label = "This character"]
    err_span: SourceSpan,
}

impl TokenError {
    pub fn line(&self) -> usize {
        self.src[..=self.err_span.offset()].lines().count()
    }
}

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
            TokenType::And => write!(f, "AND {original} null"),
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
            TokenType::Less => write!(f, "LESS {original} null"),
            TokenType::Greater => write!(f, "GREATER {original} null"),
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
                'a'..='z' | 'A'..='Z' | '_' => Started::Identifier,
                c if c.is_whitespace() => continue,
                _ => {
                    return Some(Err(TokenError {
                        src: self.whole.to_string(),
                        token: c,
                        err_span: SourceSpan::from(self.byte - c.len_utf8()..self.byte),
                    }
                    .into()));
                }
            };

            break match started {
                Started::String => todo!(),
                Started::Number => {
                    let first_non_digit = c_onwards
                        .find(|c| !matches!(c, '.' | '0'..='9'))
                        .unwrap_or_else(|| c_onwards.len());

                    let mut literal_str = &c_onwards[..first_non_digit];

                    let mut dotted = literal_str.splitn(3, '.');

                    match (dotted.next(), dotted.next(), dotted.next()) {
                        (Some(one), Some(two), Some(_)) => {
                            literal_str = &literal_str[..one.len() + 1 + two.len()];
                        }
                        (Some(one), Some(two), None) if two.is_empty() => {
                            literal_str = &literal_str[..one.len()];
                        }
                        _ => {}
                    }

                    let bytes = literal_str.len() - c.len_utf8();
                    self.byte += bytes;
                    self.rest = &self.rest[bytes..];

                    let val = match literal_str.parse() {
                        Ok(val) => val,
                        Err(e) => {
                            return Some(Err(miette::miette!(
                                labels = vec![LabeledSpan::at(
                                    self.byte - literal_str.len()..self.byte,
                                    "this numeric literal"
                                ),],
                                "{e:?}",
                            )
                            .with_source_code(self.whole.to_string())));
                        }
                    };

                    return Some(Ok(Token {
                        token_type: TokenType::Number(val),
                        original: literal_str,
                    }));
                }
                Started::Identifier => {
                    let first_non_ident = c_onwards
                        .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
                        .unwrap_or_else(|| c_onwards.len());

                    let literal_str = &c_onwards[..first_non_ident];

                    let bytes = literal_str.len() - c.len_utf8();
                    self.byte += bytes;
                    self.rest = &self.rest[bytes..];

                    let token_type = match literal_str {
                        "and" => TokenType::And,
                        "class" => TokenType::Class,
                        "else" => TokenType::Else,
                        "false" => TokenType::False,
                        "for" => TokenType::For,
                        "fun" => TokenType::Fun,
                        "if" => TokenType::If,
                        "nil" => TokenType::Nil,
                        "or" => TokenType::Or,
                        "return" => TokenType::Return,
                        "super" => TokenType::Super,
                        "this" => TokenType::This,
                        "true" => TokenType::True,
                        "var" => TokenType::Var,
                        "while" => TokenType::While,
                        _ => TokenType::Identifier,
                    };

                    return Some(Ok(Token {
                        token_type,
                        original: literal_str,
                    }));
                }
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
