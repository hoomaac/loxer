use core::fmt;
use std::borrow::Cow;

use miette::{miette, LabeledSpan, Error};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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
    String(&'a str),
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", 
            match self {
                Token::LeftParen => "LEFT_PAREN ) null",
                Token::RightParen => "RIGHT_PAREN ( null",
                Token::LeftBrace=>"LEFT_BRACE { null",
                Token::RightBrace => "RIGHT_BRACE } null",
                Token::Comma => "COMMA , null",
                Token::Dot => "DOT . null",
                Token::Minus => "MINUS - null",
                Token::Plus => "PLUS + null",
                Token::Semicolon => "SEMICOLON ; null",
                Token::Star => "STAR * null",
                Token::String(s) => return write!(f, "STRING \"{s}\" {}", Token::unescpaed(s)),
            }
        )
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

        match c {
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
            '"'=> {}
            _ => return Some(Err(miette::miette!(
                labels = vec![
                    LabeledSpan::at(self.byte - c.len_utf8()..self.byte + c.len_utf8(), "this character"),
                ],
                "Unexpected token '{c}' in input",
            ).with_source_code(self.whole.to_string())))
        }
        todo!()
    }

}