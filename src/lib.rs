use core::fmt;
use std::borrow::Cow;

use miette::{miette, LabeledSpan, Error};

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'a> {
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
    String(&'a str),
    Number(&'a str, f64),
    Identifier(&'a str),
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
            match self {
                Token::LeftParen=> write!(f,"LEFT_PAREN ) null"),
                Token::RightParen=> write!(f, "RIGHT_PAREN ( null"),
                Token::LeftBrace=> write!(f, "LEFT_BRACE {{ null"),
                Token::RightBrace=> write!(f, "RIGHT_BRACE }} null"),
                Token::Comma=> write!(f, "COMMA , null"),
                Token::Dot=> write!(f, "DOT . null"),
                Token::Minus=> write!(f, "MINUS - null"),
                Token::Plus=> write!(f, "PLUS + null"),
                Token::Semicolon=> write!(f, "SEMICOLON ; null"),
                Token::Star=> write!(f, "STAR * null"),
                Token::String(s)=> write!(f,"STRING \"{s}\" {}",Token::unescpaed(s)),
                Token::Number(lit, i) => write!(f, "NUMBER {lit} {i}"),
                Token::Identifier(n) => write!(f, "IDENTIFIER {n} null"),
                Token::And => write!(f, "And And null"),
                Token::Class => write!(f, "CLASS class null"),
                Token::Else => write!(f, "ELSE else null"),
                Token::False => write!(f, "FALSE false null"),
                Token::For => write!(f, "FOR for null"),
                Token::Fun => write!(f, "FUN fun null"),
                Token::If => write!(f, "IF if null"),
                Token::Nil => write!(f, "NIL nil null"),
                Token::Or => write!(f, "OR or null"),
                Token::Return => write!(f, "RETURN return null"),
                Token::Super => write!(f, "SUPER super null"),
                Token::This => write!(f, "THIS this null"),
                Token::True => write!(f, "TRUE true null"),
                Token::Var => write!(f, "VAR var null"),
                Token::While => write!(f, "WHILE while null"),
                Token::BandEqual => write!(f, "BANDEQUAL != null"),
                Token::EqualEqual => write!(f, "EQUALEQUAL == null"),
                Token::LessEqual => write!(f, "LESSEQUAL <= null"),
                Token::GreaterEqual => write!(f, "GREATEREQUAL >= null"),
                Token::Less => write!(f, "Less < null"),
                Token::Greater => write!(f, "Greater > null"),
                Token::Slash => write!(f, "SLASH / null"),
                Token::Bang => write!(f, "BANG ! null"),
                Token::Equal => write!(f, "EQUAL = null"),
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
            byte:0,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut chars = self.rest.chars();        
        let c = chars.next()?;
        self.byte += c.len_utf8();
        self.rest = chars.as_str();

        enum Started<'a> {
            String,
            Number,
            Identifier,
            IfEqualElse(Token<'a>, Token<'a>),
        }

        let started = match c {
            '('=> return Some(Ok(Token::LeftParen)),
            ')'=> return Some(Ok(Token::RightParen)),
            '{'=> return Some(Ok(Token::LeftBrace)),
            '}'=> return Some(Ok(Token::RightBrace)),
            ','=> return Some(Ok(Token::Comma)),
            '.'=> return Some(Ok(Token::Dot)),
            '-'=> return Some(Ok(Token::Minus)),
            '+'=> return Some(Ok(Token::Plus)),
            ';'=> return Some(Ok(Token::Semicolon)),
            '*'=> return Some(Ok(Token::Star)), 
            '"'=> Started::String,
            '<' => Started::IfEqualElse(Token::LessEqual, Token::Equal),
            '>' => Started::IfEqualElse(Token::GreaterEqual, Token::Equal),
            '=' => Started::IfEqualElse(Token::EqualEqual, Token::Equal), 
            '!' => Started::IfEqualElse(Token::BandEqual, Token::Bang),
            '0'..='9' => Started::Number,
            'a'..='z' | '_' => Started::Identifier,
            _ => return Some(Err(miette::miette!(
                labels = vec![
                    LabeledSpan::at(self.byte - c.len_utf8()..self.byte + c.len_utf8(), "this character"),
                ],
                "Unexpected token '{c}' in input",
            ).with_source_code(self.whole.to_string())))
        };

        match started {
            Started::String => todo!(),
            Started::Number => todo!(),
            Started::Identifier => todo!(),
            Started::IfEqualElse(yes, no) => {
                if self.rest.starts_with("=") {
                    self.rest = &self.rest[1..];
                    self.byte += 1;
                    return Some(Ok(yes));
                }
                else {
                    return Some(Ok(no));
                }
            }
        }
        
    }

}