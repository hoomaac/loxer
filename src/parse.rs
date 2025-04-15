use std::{borrow::Cow, vec};

use miette::{Context, Error, WrapErr};

use crate::{
    Lexer,
    lex::{Token, TokenType},
};

pub struct Parser<'a> {
    whole: &'a str,
    lexer: Lexer<'a>,
}

pub struct Ast;

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            whole: input,
            lexer: Lexer::new(input),
        }
    }

    pub fn parse(mut self) -> Result<TokenTree<'a>, Error> {
        self.parse_internal(0)
    }

    pub fn parse_internal(&mut self, min_bp: u8) -> Result<Option<TokenTree<'a>>, Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(Some(TokenTree::Atom(Atom::Nil))),
            Some(Err(e)) => {
                return Err(e).wrap_err("on left-hand side");
            }
        };

        let mut lhs = match lhs {
            Token {
                token_type: TokenType::String,
                original,
                ..
            } => TokenTree::Atom(Atom::String(Token::unescpaed(original))),
            Token {
                token_type: TokenType::Number(n),
                original,
                ..
            } => TokenTree::Atom(Atom::Number(n)),
            Token {
                token_type: TokenType::True,
                original,
                ..
            } => TokenTree::Atom(Atom::Bool(true)),
            Token {
                token_type: TokenType::False,
                original,
                ..
            } => TokenTree::Atom(Atom::Bool(false)),
            Token {
                token_type: TokenType::Nil,
                original,
                ..
            } => TokenTree::Atom(Atom::Nil),
            Token {
                token_type: TokenType::Identifier,
                original,
                ..
            } => TokenTree::Atom(Atom::Ident(original)),
            Token {
                token_type: TokenType::LeftParen | TokenType::LeftBrace,
                ..
            } => {
                let terminator = match lhs.token_type {
                    TokenType::LeftParen => TokenType::RightParen,
                    TokenType::LeftBrace => TokenType::RightBrace,
                    _ => unreachable!("by the outer match arm pattern"),
                };

                let lhs = self.parse_internal(0).wrap_err("in bracketed expression")?;
                self.lexer
                    .expect(terminator, "Unexpected bracketed expression")
                    .wrap_err_with("after bracketed expression")?;
                lhs
            }
            // unary prefix expression
            Token {
                token_type:
                    TokenType::Bang | TokenType::Print | TokenType::Minus | TokenType::Return,
                ..
            } => {
                let op = match lhs.token_type {
                    TokenType::Bang => Op::Bang,
                    Tokentype::Print => Op::Print,
                    Tokentype::Minus => Op::Minus,
                    Tokentype::Return => Op::Return,
                    _ => unreachable!("by the outer match arm pattern"),
                };

                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self.parse_internal(r_bp).wrap_err("in right-hand side")?;
                TokenTree::Cons(op, vec![rhs])
            }
            // prefix, two arguments
            Token {
                token_type: TokenType::Class | TokenType::Var,
                ..
            } => {
                let op = match lhs.token_type {
                    TokenType::Class => Op::Calss,
                    TokenType::Var => Op::Var,
                    _ => unreachable!("by the outer match arm pattern"),
                };

                let first = self
                    .parse_internal(0)
                    .wrap_err_with(|| format!("in the first argument of {op:?}"))?;

                if lhs.token_type == TokenType::Var {
                    if !matches!(first, TokenTree::Atom(Atom::Ident(_))) {
                        todo!()
                    }

                    self.lexer
                        .expect(TokenType::Equal, "missing =")
                        .wrap_err_with("in variable assignment")?;
                }

                let second = self
                    .parse_internal(0)
                    .wrap_err_with(|| format!("in the second argument of {op:?}"))?;

                TokenTree::Cons(op, vec![first, second])
            }
            Token {
                token_type: TokenType::For,
                ..
            } => {
                self.lexer
                    .expect(TokenType::LeftParen, "missing (")
                    .wrap_err("in for loop condition")?;

                let init = self
                    .parse_internal(0)
                    .wrap_err_with(|| format!("in the init condition of for loop"))?;

                self.lexer
                    .expect(TokenType::Semicolon, "missing ;")
                    .wrap_err("in for loop condition")?;

                let cond = self
                    .parse_internal(0)
                    .wrap_err_with(|| format!("in the loop condition of for loop"))?;

                self.lexer
                    .expect(TokenType::Semicolon, "missing ;")
                    .wrap_err("in for loop condition")?;

                let inc = self
                    .parse_internal(0)
                    .wrap_err_with(|| format!("in the incremental conidtion of for loop"))?;

                self.lexer
                    .expect(TokenType::RightParen, "missing )")
                    .wrap_err("in for loop condition")?;

                self.lexer
                    .expect(TokenType::LeftBrace, "missing {")
                    .wrap_err("in block of for loop")?;

                let block = self
                    .parse_internal(0)
                    .wrap_err_with(|| format!("in block of for loop"))?;

                self.lexer
                    .expect(TokenType::RightBrace, "missing }")
                    .wrap_err("in the block of for loop")?;

                TokenTree::Cons(Op::For, vec![init, cond, inc, block])
            }
            Token {
                token_type: TokenType::While,
                ..
            } => {
                self.lexer
                    .expect(TokenType::LeftParen, "missing (")
                    .wrap_err_with("in while loop condition")?;

                let cond = self.parse_internal(0).wrap_err("in while loop condition")?;

                self.lexer
                    .expect(TokenType::RightParen, "missing )")
                    .wrap_err_with("in while loop condition")?;

                self.lexer
                    .expect(TokenType::LeftBrace, "missing {")
                    .wrap_err_with("in block of for loop")?;

                let block = self.parse_internal(0).wrap_err("in block of for loop")?;

                self.lexer
                    .expect(TokenType::RightBrace, "missing }")
                    .wrap_err_with("in the block of for loop")?;

                TokenTree::Cons(Op::While, vec![cond, block])
            }
            // prefix, three arguments
            Token {
                token_type: TokenType::Class | TokenType::Var,
                ..
            } => {
                let op = match lhs.token_type {
                    TokenType::Var => Op::Var,
                    TokenType::Class => Op::Calss,
                    _ => unreachable!("by the outer match arm pattern"),
                };

                let token = self
                    .lexer
                    .expect(TokenType::Equal, "missing =")
                    .wrap_err_with(|| format!("in name of {op:?}"))?;

                assert_eq!(token.token_type, TokenType::Identifier);
                let ident = TokenTree::Atom(Atom::Ident(token.original));

                if token.token_type == TokenType::Var {
                    self.lexer
                        .expect(TokenType::Equal, "missing =")
                        .wrap_err("in variable assignment");
                }

                let second = self
                    .parse_internal(0)
                    .wrap_err_with(|| format!("in the second argument of {op:?}"))?;

                TokenTree::Cons(op, vec![ident, second])
            }
            Token {
                token_type: TokenType::Fun,
                ..
            } => {
                let token = self
                    .lexer
                    .expect(TokenType::Equal, "missing =")
                    .wrap_err("in name of function")?;

                assert_eq!(token.token_type, TokenType::Identifier);
                let name = token.original;
                let ident = TokenTree::Atom(Atom::Ident(token.original));

                let mut params = Vec::new();

                self.lexer
                    .expect(TokenType::LeftParen, "missing (")
                    .wrap_err("in function parameter list");

                while self.lexer.peek().map_or(false, |next| {
                    next.map_or(false, |next| next.token_type != TokenType::RightParen)
                }) {
                    let param = self.parse_internal(0).wrap_err_with(|| {
                        format!("in parameter #{} of function {name}", params.len() + 1)
                    })?;
                }

                let first = self
                    .parse_internal(0)
                    .wrap_err_with(|| format!("in first argument of {op:?}"))?;
                let second = self
                    .parse_internal(0)
                    .wrap_err_with(|| format!("in second argument of {op:?}"))?;
                let third = self
                    .parse_internal(0)
                    .wrap_err_with(|| format!("in third argument of {op:?}"))?;

                TokenTree::Cons(op, vec![first, second, third])
            }
            // prefix, four arguments
            Token {
                token_type: TokenType::For | TokenType::If,
                ..
            } => {
                let op = match lhs.token_type {
                    TokenType::Var => Op::Var,
                    TokenType::Fun => Op::Fun,
                    _ => unreachable!("by the outer match arm pattern"),
                };

                let first = self
                    .parse_internal(0)
                    .wrap_err_with(|| format!("in {op:?} expression"))?;
                let second = self
                    .parse_internal(0)
                    .wrap_err_with(|| format!("in {op:?} expression"))?;
                let third = self
                    .parse_internal(0)
                    .wrap_err_with(|| format!("in {op:?} expression"))?;

                TokenTree::Cons(op, vec![first, second, third])
            }
            t => panic!("bad token: {:?}", t),
        };
        loop {
            let op = self.lexer.peek();

            if op.map_or(false, |op| op.is_err()) {
                return Err(self
                    .lexer
                    .next()
                    .expect("checked some above")
                    .expect_err("checked some above"))
                .wrap_err_with(looking_for_msg());
            }

            let op = match op.map(|res| res.expect("handled Err above")) {
                None => break,
                Some(Token {
                    token_type:
                        TokenType::LeftParen
                        | TokenType::Dot
                        | TokenType::Minus
                        | TokenType::Plus
                        | TokenType::Star
                        | TokenType::BangEqual
                        | TokenType::EqualEqual
                        | TokenType::LessEqual
                        | TokenType::GreaterEqual
                        | TokenType::Less
                        | TokenType::Greater
                        | TokenType::Slash
                        | TokenType::And
                        | TokenType::Or,
                    ..
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
        lhs
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
fn prefix_binding_power(op: Op) -> ((), u8) {
    match op {
        Op::Bang | Op::Minus | Op::Print | Op::Return => ((), 9),
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
