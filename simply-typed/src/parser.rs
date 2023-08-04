use crate::builtins::BUILTINS;
use crate::scanner::{Scanner, Token, TokenType};
use crate::term::{abs, app, var, Term};
use crate::types::Type;

#[derive(Debug)]
pub struct Parser<'a> {
    current: usize,
    tokens: &'a Vec<Token>,
}

impl<'a> From<&'a Scanner> for Parser<'a> {
    fn from(scanner: &'a Scanner) -> Self {
        let tokens = &scanner.tokens;
        Parser::new(tokens)
    }
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a Vec<Token>) -> Self {
        Self { current: 0, tokens }
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    /// return the previous token (or current if at the first token)
    fn previous(&mut self) -> Token {
        if self.current == 0 {
            self.peek()
        } else {
            self.tokens[self.current - 1].clone()
        }
    }

    /// check if all statements have been parsed
    fn is_end(&mut self) -> bool {
        self.peek().token == TokenType::Eof
    }

    /// return the current token and advance the parser one token
    fn advance(&mut self) -> Token {
        if !self.is_end() {
            self.current += 1;
        }
        self.previous()
    }

    /// check if the current token matches
    fn check(&mut self, t: TokenType) -> bool {
        if self.is_end() {
            return false;
        }
        self.peek().token == t
    }

    fn expect(&mut self, tt: TokenType) -> Result<Token, String> {
        if self.check(tt) {
            Ok(self.advance())
        } else {
            Err(format!("Expected {:?}, got {:?}", tt, self.peek().token))
        }
    }

    fn dtype(&mut self) -> Result<Type, String> {
        let mut types: Vec<Type> = Vec::new();

        loop {
            match self.advance() {
                Token {
                    token: TokenType::ListType,
                    ..
                } => {
                    self.expect(TokenType::LeftBracket)?;
                    let dtype = self.dtype()?;
                    types.push(Type::List(box dtype));
                }
                Token {
                    dtype: Some(dtype), ..
                } => types.push(dtype),
                Token {
                    token: TokenType::LeftParen,
                    ..
                } => {
                    let dtype = self.dtype()?;
                    types.push(dtype);
                }
                Token {
                    token: TokenType::Arrow,
                    ..
                } => (),
                Token {
                    token:
                        TokenType::Dot
                        | TokenType::RightParen
                        | TokenType::RightBracket
                        | TokenType::LeftBracket
                        | TokenType::Equal,
                    ..
                } => break,
                Token { token, .. } => return Err(format!("Invalid token {:?} in type", token)),
            }
        }

        types = types.into_iter().rev().collect();
        let (head, tail) = types.split_first().unwrap();
        Ok(tail.iter().fold(head.clone(), |acc, t| {
            Type::Con(box t.clone(), box acc.clone())
        }))
    }

    pub fn parse(&mut self) -> Result<Term, String> {
        self.expr()
    }

    pub fn expr(&mut self) -> Result<Term, String> {
        let mut terms: Vec<Term> = Vec::new();

        while !self.is_end()
            && !self.check(TokenType::RightParen)
            && !self.check(TokenType::Then)
            && !self.check(TokenType::Else)
            && !self.check(TokenType::In)
        {
            terms.push(self.primary()?);
        }

        let head = &terms[0];
        let tail = &terms[1..];
        Ok(tail.iter().fold(head.clone(), |acc, t| app(acc, t.clone())))
    }

    fn primary(&mut self) -> Result<Term, String> {
        match self.advance() {
            Token {
                token: token @ (TokenType::IsNil | TokenType::Head | TokenType::Tail),
                ..
            } => {
                let t1 = self.expr()?;

                let term = match token {
                    TokenType::IsNil => Term::IsNil(box t1),
                    TokenType::Head => Term::Head(box t1),
                    TokenType::Tail => Term::Tail(box t1),
                    _ => unreachable!(),
                };

                Ok(term)
            }
            // sugar for lists, requires a type annotation
            Token {
                token: TokenType::ListSugar,
                ..
            } => {
                self.expect(TokenType::LeftBracket)?;
                let dtype = self.dtype()?;
                self.expect(TokenType::LeftBracket)?;
                let mut terms: Vec<Term> = Vec::new();

                loop {
                    if self.check(TokenType::RightBracket) {
                        self.advance();
                        break;
                    } else {
                        terms.push(self.primary()?);
                        if !self.check(TokenType::RightBracket) {
                            self.expect(TokenType::Comma)?;
                        }
                    }
                }

                terms = terms.into_iter().rev().collect();

                Ok(terms.iter().fold(Term::Nil(dtype.clone()), |acc, t| {
                    Term::Cons(box t.clone(), box acc)
                }))
            }
            Token {
                token: TokenType::Nil,
                ..
            } => {
                self.expect(TokenType::LeftBracket)?;
                let dtype = self.dtype()?;
                Ok(Term::Nil(dtype))
            }
            Token {
                token: TokenType::Cons,
                ..
            } => {
                let t1 = self.primary()?;
                let t2 = self.primary()?;
                Ok(Term::Cons(box t1, box t2))
            }
            // TODO make this a term instead of a derived form
            Token {
                token: TokenType::Let,
                ..
            } => {
                let binding = self.expect(TokenType::Name)?;
                self.expect(TokenType::Colon)?;
                let dtype = self.dtype()?;
                let e1 = self.expr()?;
                self.expect(TokenType::In)?;
                let e2 = self.expr()?;
                if let Some(name) = binding.name {
                    Ok(app(abs(name, dtype, e2), e1))
                } else {
                    Err("let missing binding".into())
                }
            }
            Token {
                token: TokenType::Lambda,
                ..
            } => {
                let binding = self.expect(TokenType::Name)?;
                self.expect(TokenType::Colon)?;
                let dtype = self.dtype()?;
                if let Some(name) = binding.name {
                    Ok(abs(&name, dtype, self.expr()?))
                } else {
                    Err("lambda missing binding".into())
                }
            }
            Token {
                token: TokenType::If,
                ..
            } => {
                let cond = self.expr()?;
                self.expect(TokenType::Then)?;
                let consq = self.expr()?;
                self.expect(TokenType::Else)?;
                let alt = self.expr()?;
                Ok(Term::If(box cond, box consq, box alt))
            }
            Token {
                token: TokenType::IsZero,
                ..
            } => Ok(Term::IsZero(box self.expr()?)),
            Token {
                token: TokenType::Pred,
                ..
            } => Ok(Term::Pred(box self.expr()?)),
            Token {
                token: TokenType::Succ,
                ..
            } => Ok(Term::Succ(box self.expr()?)),
            Token {
                token: TokenType::Fix,
                ..
            } => Ok(Term::Fix(box self.expr()?)),
            Token {
                token: TokenType::LeftParen,
                ..
            } => {
                let e = self.expr()?;
                self.expect(TokenType::RightParen)?;
                Ok(e)
            }
            Token {
                token: TokenType::Name,
                name: Some(name),
                ..
            } => {
                if let Some(builtin) = BUILTINS.get(name.as_str()) {
                    Ok(builtin.clone())
                } else {
                    Ok(var(name))
                }
            }
            Token {
                term: Some(term), ..
            } => Ok(term),
            _ => Err(format!(
                "empty program or primary failure at {:?}",
                self.peek()
            )),
        }
    }
}
