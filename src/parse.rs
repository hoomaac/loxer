use std::borrow::Cow;

use miette::{Context, Error, WrapErr};

use crate::{
    Lexer,
    lex::{Token, TokenType},
};

pub struct Parser<'a> {
    whole: &'a str,
    lexer: std::iter::Peekable<Lexer<'a>>,
}

pub struct Ast;

#[derive(Diagnostic, Debug, Error)]
#[error("Unterminated string")]
pub struct Eof;

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            whole: input,
            lexer: Lexer::new(input).peekable(),
        }
    }

    pub fn parse(mut self) -> Result<TokenTree<'a>, Error> {
        self.parse_internal(0)
    }

    pub fn parse_internal(
        &mut self,
        looking_for: Option<(Op, usize)>,
        min_bp: u8,
    ) -> Result<Option<TokenTree<'a>>, Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(TokenTree::Atom(Atom::Nil)),
            Some(Err(e)) => {
                let msg = if let Some((op, arg)) = looking_for {
                    format!("looking for argument #{arg} for {op:?}")
                } else {
                    "looking for a statement".to_string()
                };

                return Err(e).wrap_err(msg);
            }
        };

        let mut lhs = match lhs {
            Token {
                token_type: TokenType::String,
                original,
            } => TokenTree::Atom(Atom::String(Token::unescpaed(original))),
            Token::Atom(it) => TokenTree::Atom(it),
            Token::Op('(') => {
                let lhs = self.parse_internal(0);
                assert_eq!(lexer.next(), Token::Op(')'));
                lhs.wrap_err("group")?
            }
            Token::Op(op) => {
                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self.parse_internal(r_bp).wrap_err("parse RHS")?;
                TokenTree::Cons(op, vec![rhs])
            }
            t => panic!("bad token: {:?}", t),
        };
        loop {
            let op = self.lexer.peek();
            let op = match op {
                None => break,
                Some(Token {
                    token_type: Token::Op(op),
                }) => op,
                t => panic!("bad token: {:?}", t),
            };
            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                lexer.next();
                lhs = if op == '[' {
                    let rhs = expr_bp(lexer, 0);
                    assert_eq!(lexer.next(), Token::Op(']'));
                    TokenTree::Cons(op, vec![lhs, rhs])
                } else {
                    TokenTree::Cons(op, vec![lhs])
                };
                continue;
            }
            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                lexer.next();
                lhs = if op == '?' {
                    let mhs = self.parse_internal(0);
                    assert_eq!(lexer.next(), Token::Op(':'));
                    let rhs = self.parse_internal(r_bp);
                    TokenTree::Cons(op, vec![lhs, mhs, rhs])
                } else {
                    let rhs = self.parse_internal(r_bp);
                    TokenTree::Cons(op, vec![lhs, rhs])
                };
                continue;
            }
            break;
        }
        Ok(lhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'a> {
    String(Cow<'a, str>),
    Number(f64),
    Nil,
    Bool(bool),
    Ident(&'a str),
    This,
    Super,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    And,
    Bang,
    BangEqual,
    Calss,
    Equal,
    EqualEqual,
    Field,
    For,
    Fun,
    Greater,
    GreaterEqual,
    If,
    Less,
    LessEqual,
    Minus,
    Plus,
    Print,
    Return,
    Star,
    Super,
    This,
    Var,
    While,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Delimiter {
    Paren,
    Brace,
}

#[derive(Debug, Clone, PartialEq)]
enum TokenTree<'a> {
    Atom(Atom<'a>),
    Cons(Op, Vec<TokenTree<'a>>),
}
impl fmt::Display for TokenTree<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenTree::Atom(i) => write!(f, "{}", i),
            TokenTree::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for token_tree in rest {
                    write!(f, " {}", token_tree)?
                }
                write!(f, ")")
            }
        }
    }
}

fn expr_bp(lexer: &mut Lexer, min_bp: u8) -> TokenTree {}
fn prefix_binding_power(op: char) -> ((), u8) {
    match op {
        '+' | '-' => ((), 9),
        _ => panic!("bad op: {:?}", op),
    }
}
fn postfix_binding_power(op: char) -> Option<(u8, ())> {
    let res = match op {
        '!' => (11, ()),
        '[' => (11, ()),
        _ => return None,
    };
    Some(res)
}
fn infix_binding_power(op: char) -> Option<(u8, u8)> {
    let res = match op {
        '=' => (2, 1),
        '?' => (4, 3),
        '+' | '-' => (5, 6),
        '*' | '/' => (7, 8),
        '.' => (14, 13),
        _ => return None,
    };
    Some(res)
}
