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

#[derive(Diagnostic, Debug, Error)]
#[error("Unterminated string")]
pub struct StringUnterminatedError {
    // The `Source` that miette will use.
    #[source_code]
    src: String,

    #[label = "This string"]
    err_span: SourceSpan,
}

impl StringUnterminatedError {
    pub fn line(&self) -> usize {
        self.src[..=self.err_span.offset()].lines().count()
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unterminated string")]
pub struct Eof;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub offset: usize,
    pub original: &'a str,
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
    BangEqual,
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
    Print,
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
            TokenType::Number(n) => {
                if n.trunc() == n {
                    write!(f, "NUMBER {original} {n}.0")
                } else {
                    write!(f, "NUMBER {original} {n}")
                }
            }
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
            TokenType::Print => write!(f, "PRINT {original} null"),
            TokenType::Return => write!(f, "RETURN {original} null"),
            TokenType::Super => write!(f, "SUPER {original} null"),
            TokenType::This => write!(f, "THIS {original} null"),
            TokenType::True => write!(f, "TRUE {original} null"),
            TokenType::Var => write!(f, "VAR {original} null"),
            TokenType::While => write!(f, "WHILE {original} null"),
            TokenType::BangEqual => write!(f, "BangEqual {original} null"),
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
    pub fn unescpaed<'a>(s: &'a str) -> Cow<'a, str> {
        Cow::Borrowed(s.trim_matches('"'))
    }
}

pub struct Lexer<'a> {
    whole: &'a str,
    rest: &'a str,
    byte: usize,
    peak: Option<Result<Token<'a>, miette::Error>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            whole: input,
            rest: input,
            byte: 0,
            peak: None,
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn expect(
        &mut self,
        expected: TokenType,
        unexpected: &str,
    ) -> Result<Token<'a>, miette::Error> {
        self.expect_where(|next| next.token_type == expected, unexpected)
    }

    pub fn expect_where(
        &mut self,
        mut check: impl FnMut(&Token<'a>) -> bool,
        unexpected: &str,
    ) -> Result<Token<'a>, miette::Error> {
        match self.next() {
            Some(Ok(token)) if check(&token) => Ok(token),
            Some(Ok(token)) => {
                return Err(miette::miette!(
                    labels = vec![
                        LabeledSpan::at(token.offset..token.offset + token.original.len(), "here"),
                        help = "Exptected {next:?}",
                        "{unexpected}"
                    ],
                )
                .with_source_code(self.whole.to_string()));
            }
            Some(Err(e)) => Err(e),
            None => Err(Eof.into()),
        }
    }

    pub fn peek(&mut self) -> Option<&Result<Token<'a>, miette::Error>> {
        if self.peak.is_some() {
            return self.peak;
        }

        self.peak = self.next();

        self.peak.as_ref()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.peak = None;

        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let c_str = &self.rest[..c.len_utf8()];
            let c_at = self.byte;
            let c_onwards = self.rest;
            self.byte += c.len_utf8();
            self.rest = chars.as_str();

            let get_token = |token_type: TokenType| {
                Some(Ok(Token {
                    token_type,
                    offset: c_at,
                    original: c_str,
                }))
            };

            enum Started {
                String,
                Number,
                Identifier,
                Slash,
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
                '/' => Started::Slash,
                '"' => Started::String,
                '<' => Started::IfEqualElse(TokenType::LessEqual, TokenType::Equal),
                '>' => Started::IfEqualElse(TokenType::GreaterEqual, TokenType::Equal),
                '=' => Started::IfEqualElse(TokenType::EqualEqual, TokenType::Equal),
                '!' => Started::IfEqualElse(TokenType::BangEqual, TokenType::Bang),
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
                Started::String => {
                    if let Some(end_quote) = self.rest.find('"') {
                        self.byte += end_quote + 1;
                        self.rest = &self.rest[end_quote + 1..];

                        Some(Ok(Token {
                            token_type: TokenType::String,
                            offset: c_at,
                            original: &c_onwards[..end_quote + 2], // 2 because we want to include starting and terminating ones,
                        }))
                    } else {
                        let unterm_err = StringUnterminatedError {
                            src: self.whole.to_string(),
                            err_span: SourceSpan::from(self.byte - c.len_utf8()..self.whole.len()),
                        };

                        self.byte += self.rest.len();
                        self.rest = &self.rest[self.rest.len()..];
                        return Some(Err(unterm_err.into()));
                    }
                }
                Started::Slash => {
                    if self.rest.starts_with('/') {
                        // This line is a comment
                        let eol = self.rest.find('\n').unwrap_or_else(|| self.rest.len());
                        self.byte += eol;
                        self.rest = &self.rest[eol..];
                        continue;
                    } else {
                        Some(Ok(Token {
                            token_type: TokenType::Slash,
                            offset: c_at,
                            original: c_str,
                        }))
                    }
                }
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
                        offset: c_at,
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
                        "print" => TokenType::Print,
                        "super" => TokenType::Super,
                        "this" => TokenType::This,
                        "true" => TokenType::True,
                        "var" => TokenType::Var,
                        "while" => TokenType::While,
                        _ => TokenType::Identifier,
                    };

                    return Some(Ok(Token {
                        token_type,
                        offset: c_at,
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
                            offset: c_at,
                            original: span,
                        }))
                    } else {
                        Some(Ok(Token {
                            token_type: no,
                            offset: c_at,
                            original: c_str,
                        }))
                    }
                }
            };
        }
    }
}
