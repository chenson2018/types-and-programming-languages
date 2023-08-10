use std::collections::HashMap;

use crate::error::LcError;
use crate::scanner::{Scanner, Token, TokenType};
use crate::term::{abs, app, var, Term};
use crate::types::Type;

#[derive(Debug)]
pub struct Parser<'a> {
    current: usize,
    tokens: &'a Vec<Token>,
    type_bindings: HashMap<String, Type>,
}

impl<'a> From<&'a Scanner> for Parser<'a> {
    fn from(scanner: &'a Scanner) -> Self {
        let tokens = &scanner.tokens;
        Parser::new(tokens)
    }
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            current: 0,
            tokens,
            type_bindings: HashMap::new(),
        }
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

    fn check_raw(&mut self, t: TokenType) -> bool {
        self.peek().token == t
    }

    fn expect(&mut self, tt: TokenType) -> Result<Token, LcError> {
        if self.check(tt) {
            Ok(self.advance())
        } else {
            let peek = self.peek();
            let msg = format!("Expected {:?}, got {:?}", tt, peek.token);
            Err(LcError::new(&msg, peek.range))
        }
    }

    fn match_any<T>(&mut self, types: T) -> bool
    where
        T: IntoIterator<Item = TokenType>,
    {
        for t in types {
            if self.check_raw(t) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check_any<T>(&mut self, types: T) -> bool
    where
        T: IntoIterator<Item = TokenType>,
    {
        for t in types {
            if self.check_raw(t) {
                return true;
            }
        }
        false
    }

    fn dtype(&mut self) -> Result<Type, LcError> {
        let mut types: Vec<Type> = Vec::new();

        loop {
            // these are tokens that mark the start or end boundary
            if !self.check_any([
                TokenType::Lt,
                TokenType::Gt,
                TokenType::NatType,
                TokenType::BoolType,
                TokenType::UnitType,
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::Arrow,
                TokenType::LeftBracket,
                TokenType::RightBracket,
                TokenType::Equal,
                TokenType::Comma,
                TokenType::Eof,
                TokenType::Dot,
                TokenType::LeftBrace,
                TokenType::RightBrace,
                TokenType::Name,
                TokenType::In,
                TokenType::Mu,
            ]) {
                let peek = self.peek();
                let msg = format!("unexpected {:?} in type", peek.token);
                return Err(LcError::new(&msg, peek.range));
            };

            let current = self.peek();

            // these are tokens that mark the end boundary
            // this ensures the caller matches the boundary, as opposed to consuming it here
            match current.token {
                TokenType::RightBracket
                | TokenType::Equal
                | TokenType::RightParen
                | TokenType::Dot
                | TokenType::Comma
                | TokenType::Eof
                | TokenType::RightBrace
                | TokenType::In => (),
                _ => {
                    self.advance();
                }
            };

            match current {
                Token {
                    token: TokenType::Mu,
                    ..
                } => {
                    let name = self.expect(TokenType::Name)?.name.expect("missing name");
                    self.type_bindings
                        .insert(name.clone(), Type::TyVar(name.clone()));
                    self.expect(TokenType::Dot)?;
                    let dtype = self.dtype()?;
                    self.type_bindings.remove(&name);
                    types.push(Type::Rec(name, box dtype));
                }
                Token {
                    token: TokenType::Name,
                    name: Some(name),
                    range,
                    ..
                } => {
                    if let Some(dtype) = self.type_bindings.get(&name) {
                        types.push(dtype.clone());
                    } else {
                        return Err(LcError::new(&"undefined name in type", range));
                    }
                }
                Token {
                    token: TokenType::LeftBrace,
                    ..
                } => {
                    let mut records = Vec::new();
                    let mut counter = 0;

                    loop {
                        let (record_name, dtype) = if self.check(TokenType::Name) {
                            let name = self.expect(TokenType::Name)?.name.expect("missing name");

                            if let Some(dtype) = self.type_bindings.get(&name) {
                                (counter.to_string(), dtype.clone())
                            } else {
                                self.expect(TokenType::Colon)?;
                                (name, self.dtype()?)
                            }
                        } else {
                            (counter.to_string(), self.dtype()?)
                        };

                        records.push((record_name, dtype));

                        if self.peek().token == TokenType::RightBrace {
                            self.advance();
                            break;
                        } else {
                            self.expect(TokenType::Comma)?;
                            counter += 1;
                        }
                    }
                    types.push(Type::Record(records));
                }
                Token {
                    token: TokenType::Lt,
                    ..
                } => {
                    let mut variant = Vec::new();

                    loop {
                        let name = self.expect(TokenType::Name)?.name.expect("missing name");
                        self.expect(TokenType::Colon)?;
                        variant.push((name, self.dtype()?));
                        if self.previous().token == TokenType::Gt {
                            break;
                        } else {
                            self.expect(TokenType::Comma)?;
                        }
                    }
                    types.push(Type::Variant(variant));
                }
                Token {
                    dtype: Some(dtype), ..
                } => types.push(dtype),
                Token {
                    token: TokenType::LeftParen,
                    ..
                } => {
                    let dtype = self.dtype()?;
                    self.expect(TokenType::RightParen)?;
                    types.push(dtype);
                }
                Token {
                    token: TokenType::Arrow,
                    ..
                } => (),
                _ => break,
            }
        }

        types = types.into_iter().rev().collect();
        let (head, tail) = types.split_first().unwrap();
        Ok(tail.iter().fold(head.clone(), |acc, t| {
            Type::Con(box t.clone(), box acc.clone())
        }))
    }

    pub fn parse(&mut self) -> Result<Term, LcError> {
        let term = self.expr()?;
        if self.is_end() {
            Ok(term)
        } else {
            let range = self.peek().range;
            Err(LcError::new(&"unterminated/invalid expression", range))
        }
    }

    pub fn expr(&mut self) -> Result<Term, LcError> {
        let mut terms: Vec<Term> = Vec::new();

        let (left, _) = self.peek().range;

        while !self.is_end()
            && !self.check(TokenType::RightParen)
            && !self.check(TokenType::Then)
            && !self.check(TokenType::Else)
            && !self.check(TokenType::In)
            && !self.check(TokenType::Gt)
            && !self.check(TokenType::Of)
            && !self.check(TokenType::VertBar)
            && !self.check(TokenType::Comma)
            && !self.check(TokenType::RightBrace)
            && !self.check(TokenType::Dot)
        {
            terms.push(self.primary()?);
        }

        let (_, mut right) = self.peek().range;

        let head = &terms[0];
        let tail = &terms[1..];
        let mut term = tail
            .iter()
            .fold(head.clone(), |acc, t| app(acc, t.clone(), (left, right)));

        // check if we are accessing a record from this term, possibly multiple times
        while self.match_any([TokenType::Dot]) {
            let accessor = self
                .expect(TokenType::Name)?
                .name
                .expect("missing record accessor");
            (_, right) = self.previous().range;
            term = Term::Proj(box term, accessor, (left, right));
        }

        Ok(term)
    }

    fn primary(&mut self) -> Result<Term, LcError> {
        match self.advance() {
            Token {
                token: tt @ (TokenType::Fold | TokenType::UnFold),
                ..
            } => {
                let (left, _) = self.previous().range;
                self.expect(TokenType::LeftBracket)?;
                let dtype = self.dtype()?;
                self.expect(TokenType::RightBracket)?;
                let (_, right) = self.previous().range;
                let range = (left, right);

                match tt {
                    TokenType::Fold => Ok(Term::Fold(dtype, range)),
                    TokenType::UnFold => Ok(Term::UnFold(dtype, range)),
                    _ => unreachable!(),
                }
            }
            Token {
                token: TokenType::Lt,
                ..
            } => {
                let (left, _) = self.previous().range;
                let var_name = self.expect(TokenType::Name)?.name.expect("missing name");
                self.expect(TokenType::Equal)?;
                let term = self.expr()?;
                self.expect(TokenType::Gt)?;
                self.expect(TokenType::As)?;
                let dtype = self.dtype()?;
                let (_, right) = self.previous().range;
                let range = (left, right);
                Ok(Term::Tag(var_name, box term, dtype, range))
            }
            Token {
                token: TokenType::LeftBrace,
                ..
            } => {
                let (left, _) = self.previous().range;
                let mut records: Vec<(String, Term)> = Vec::new();
                let mut counter = 0;

                while !self.check(TokenType::RightBrace) {
                    // encodes tuples as records with a numeric name
                    // peek one ahead to see if there is an equal sign, indicating a named record

                    let rec_name = if &self.tokens[self.current + 1].token == &TokenType::Equal {
                        let name = self.expect(TokenType::Name)?.name.expect("missing name");
                        self.expect(TokenType::Equal)?;
                        name
                    } else {
                        counter.to_string()
                    };

                    let term = self.expr()?;
                    self.match_any([TokenType::Comma]);
                    records.push((rec_name, term));
                    counter += 1;
                }

                self.expect(TokenType::RightBrace)?;
                let (_, right) = self.previous().range;
                let range = (left, right);

                Ok(Term::Record(records, range))
            }
            Token {
                token: TokenType::Case,
                ..
            } => {
                let (left, _) = self.previous().range;
                let case_term = self.expr()?;
                self.expect(TokenType::Of)?;
                let mut cases: Vec<(String, String, Term)> = Vec::new();

                while self.match_any([TokenType::Lt]) {
                    let var_name = self.expect(TokenType::Name)?.name.expect("missing name");
                    self.expect(TokenType::Equal)?;
                    let binding = self.expect(TokenType::Name)?.name.expect("missing name");
                    self.expect(TokenType::Gt)?;
                    self.expect(TokenType::Arrow)?;
                    let term = self.expr()?;
                    cases.push((var_name, binding, term));

                    if !self.match_any([TokenType::VertBar]) {
                        break;
                    }
                }
                let (_, right) = self.previous().range;
                let range = (left, right);
                Ok(Term::Case(box case_term, cases, range))
            }
            Token {
                token: TokenType::Let,
                ..
            } => {
                let (left, _) = self.previous().range;
                let name = self.expect(TokenType::Name)?.name.expect("missing name");
                self.expect(TokenType::Equal)?;
                let t1 = self.expr()?;
                self.expect(TokenType::In)?;
                let t2 = self.expr()?;
                let (right, _) = self.peek().range;
                let range = (left, right);
                Ok(Term::Let(name, box t1, box t2, range))
            }
            Token {
                token: TokenType::Lambda,
                ..
            } => {
                let (left, _) = self.previous().range;
                let name = self.expect(TokenType::Name)?.name.expect("missing name");
                self.expect(TokenType::Colon)?;
                let dtype = self.dtype()?;
                self.expect(TokenType::Dot)?;
                let t1 = self.expr()?;
                let (_, right) = self.peek().range;
                Ok(abs(&name, dtype, t1, (left, right)))
            }
            Token {
                token: TokenType::If,
                ..
            } => {
                let (left, _) = self.previous().range;
                let cond = self.expr()?;
                self.expect(TokenType::Then)?;
                let consq = self.expr()?;
                self.expect(TokenType::Else)?;
                let alt = self.expr()?;
                let (_, right) = self.peek().range;
                let range = (left, right);
                Ok(Term::If(box cond, box consq, box alt, range))
            }
            Token {
                token: tt @ (TokenType::IsZero | TokenType::Pred | TokenType::Succ),
                ..
            } => {
                let range = self.previous().range;
                let term = self.expr()?;

                let term = match tt {
                    TokenType::IsZero => Term::IsZero(box term, range),
                    TokenType::Pred => Term::Pred(box term, range),
                    TokenType::Succ => Term::Succ(box term, range),
                    _ => unreachable!(),
                };

                Ok(term)
            }
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
                if self.match_any([TokenType::Equal]) {
                    // parser-level type alias
                    let dtype = self.dtype()?;
                    self.expect(TokenType::In)?;
                    self.type_bindings.insert(name, dtype);
                    self.primary()
                } else {
                    Ok(var(name, self.previous().range))
                }
            }
            Token {
                term: Some(term), ..
            } => Ok(term),
            _ => Err(LcError::new(&"invalid expression", self.previous().range)),
        }
    }
}
