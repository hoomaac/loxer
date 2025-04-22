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

    pub fn parse_block(&mut self) -> Result<TokenTree<'a>, Error> {
        self.lexer.expect(TokenType::LeftBrace, "missing {")?;

        let block = self.parse_statement_internal(0)?;

        self.lexer.expect(TokenType::RightBrace, "missing }")?;

        Ok(block)
    }

    pub fn parse_statement_internal(&mut self, min_bp: u8) -> Result<Option<TokenTree<'a>>, Error> {
        let first = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(Some(TokenTree::Atom(Atom::Nil))),
            Some(Err(e)) => {
                return Err(e).wrap_err("on left-hand side");
            }
        };

        match first {
            Token {
                token_type: TokenType::Identifier,
                original,
                ..
            } => TokenTree::Atom(Atom::Ident(original)),
            Token {
                token_type: TokenType::LeftParen,
                ..
            } => {
                let lhs = self
                    .parse_expression_internal(0)
                    .wrap_err("in bracketed expression")?;
                self.lexer
                    .expect(TokenType::RightParen, "Unexpected bracketed expression")
                    .wrap_err("after bracketed expression")?;
                lhs
            }
            Token {
                token_type: TokenType::Print | TokenType::Return,
                ..
            } => {
                let op = match lhs.token_type {
                    Tokentype::Print => Op::Print,
                    Tokentype::Return => Op::Return,
                    _ => unreachable!("by the outer match arm pattern"),
                };

                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self
                    .parse_expression_internal(r_bp)
                    .wrap_err("in right-hand side")?;
                TokenTree::Cons(op, vec![rhs])
            }
            Token {
                token_type: TokenType::Class,
                ..
            } => {
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
                token_type: TokenType::Var,
                ..
            } => {
                let token = self
                    .lexer
                    .expect(TokenType::Identifier, "expected identifier")
                    .wrap_err_with("in variable assignment")?;

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
                    .parse_expression_internal(0)
                    .wrap_err_with(|| format!("in the init condition of for loop"))?;

                self.lexer
                    .expect(TokenType::Semicolon, "missing ;")
                    .wrap_err("in for loop condition")?;

                let cond = self
                    .parse_expression_internal(0)
                    .wrap_err_with(|| format!("in the loop condition of for loop"))?;

                self.lexer
                    .expect(TokenType::Semicolon, "missing ;")
                    .wrap_err("in for loop condition")?;

                let inc = self
                    .parse_expression_internal(0)
                    .wrap_err_with(|| format!("in the incremental conidtion of for loop"))?;

                self.lexer
                    .expect(TokenType::RightParen, "missing )")
                    .wrap_err("in for loop condition")?;

                let block = self.parse_block().wrap_err("in for loop")?;

                TokenTree::Cons(Op::For, vec![init, cond, inc, block])
            }
            Token {
                token_type: TokenType::While,
                ..
            } => {
                self.lexer
                    .expect(TokenType::LeftParen, "missing (")
                    .wrap_err_with("in while loop condition")?;

                let cond = self
                    .parse_expression_internal(0)
                    .wrap_err("in while loop condition")?;

                self.lexer
                    .expect(TokenType::RightParen, "missing )")
                    .wrap_err_with("in while loop condition")?;

                let block = self.parse_block().wrap_err("in while loop")?;

                TokenTree::Cons(Op::While, vec![cond, block])
            }
            Token {
                token_type: TokenType::Class,
                ..
            } => {
                let token = self
                    .lexer
                    .expect(TokenType::Identifier, "expected identifier")
                    .wrap_err("in class name")?;

                assert_eq!(token.token_type, TokenType::Identifier);
                let ident = TokenTree::Atom(Atom::Ident(token.original));

                if token.token_type == TokenType::Var {
                    self.lexer
                        .expect(TokenType::Equal, "missing =")
                        .wrap_err("in variable assignment");
                }

                let block = self.parse_block().wrap_err("in class definition body")?;

                TokenTree::Cons(Op::Calss, vec![ident, block])
            }
            Token {
                token_type: TokenType::Var,
                ..
            } => {
                let token = self
                    .lexer
                    .expect(TokenType::Identifier, "expected identifier")
                    .wrap_err("in variable assignment")?;

                assert_eq!(token.token_type, TokenType::Identifier);
                let ident = TokenTree::Atom(Atom::Ident(token.original));

                self.lexer
                    .expect(TokenType::Equal, "missing =")
                    .wrap_err("in variable assignment");

                let second = self
                    .parse_expression_internal(0)
                    .wrap_err("in variable assignment expression")?;

                TokenTree::Cons(Op::Var, vec![ident, second])
            }
            Token {
                token_type: TokenType::Fun,
                ..
            } => {
                let token = self
                    .lexer
                    .expect(TokenType::Identifier, "expected identifier")
                    .wrap_err("in function name declaration")?;

                assert_eq!(token.token_type, TokenType::Identifier);
                let name = token.original;
                let ident = TokenTree::Atom(Atom::Ident(token.original));

                let mut params = Vec::new();

                self.lexer
                    .expect(TokenType::LeftParen, "missing (")
                    .wrap_err_with(|| format!("in parameter list of function {name}"));

                if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        token_type: TokenType::RightParen,
                        ..
                    }))
                ) {
                } else {
                    loop {
                        let param = self
                            .lexer
                            .expect_where(TokenType::Identifier, "unexpcted token")
                            .wrap_err_with(|| {
                                format!("in parameter #{} of function {name}", params.len() + 1)
                            })?;

                        params.push(param);

                        let token = self
                            .lexer
                            .expect_where(
                                |token| {
                                    matches!(
                                        token.token_type,
                                        TokenType::RightParen | TokenType::Comma
                                    )
                                },
                                "continuing parameter list",
                            )
                            .wrap_err_with(|| format!("in parameter list of function {name}"))?;

                        if token.token_type == TokenType::RightParen {
                            break;
                        }
                    }
                }

                let block = self
                    .parse_block()
                    .wrap_err_with(|| format!("for body of function {name}"))?;

                TokenTree::Fun(ident, params, block)
            }
            Token {
                token_type: TokenType::If,
                ..
            } => {
                self.lexer
                    .expect(TokenType::LeftParen, "missing (")
                    .wrap_err_with("in while loop condition")?;

                let cond = self
                    .parse_expression_internal(0)
                    .wrap_err("in while loop condition")?;

                self.lexer
                    .expect(TokenType::RightParen, "missing )")
                    .wrap_err_with("in while loop condition")?;

                let block = self.parse_block().wrap_err("in body of if")?;

                let mut otherwise = None;
                if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        token_type: TokenType::Else,
                        ..
                    }))
                ) {
                    self.lexer.next();
                    otherwise = Some(self.parse_block().wrap_err("in blody of else")?);
                }

                TokenTree::If(cond, block, otherwise)
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
                .wrap_err_with();
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

    pub fn parse_expression_internal(
        &mut self,
        min_bp: u8,
    ) -> Result<Option<TokenTree<'a>>, Error> {
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

                let lhs = self
                    .parse_expression_internal(0)
                    .wrap_err("in bracketed expression")?;
                self.lexer
                    .expect(terminator, "Unexpected bracketed expression")
                    .wrap_err_with("after bracketed expression")?;
                lhs
            }
            Token {
                token_type: TokenType::Bang | TokenType::Minus,
                ..
            } => {
                let op = match lhs.token_type {
                    TokenType::Bang => Op::Bang,
                    Tokentype::Minus => Op::Minus,
                    _ => unreachable!("by the outer match arm pattern"),
                };

                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self.parse_internal(r_bp).wrap_err("in right-hand side")?;
                TokenTree::Cons(op, vec![rhs])
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
                .wrap_err_with();
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
    Fun(Atom<'a>, Vec<Token<'a>>, TokenTree<'a>),
    If(TokenTree<'a>, TokenTree<'a>, Option<TokenTree<'a>>),
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
