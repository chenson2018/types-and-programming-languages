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

    fn check_raw(&mut self, t: TokenType) -> bool {
        self.peek().token == t
    }

    fn expect(&mut self, tt: TokenType) -> Result<Token, String> {
        if self.check(tt) {
            Ok(self.advance())
        } else {
            Err(format!("Expected {:?}, got {:?}", tt, self.peek().token))
        }
    }

    #[allow(dead_code)]
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

    fn dtype(&mut self) -> Result<Type, String> {
        let mut types: Vec<Type> = Vec::new();

        loop {
            // these are tokens that mark the start or end boundary
            if !self.check_any([
                TokenType::Lt,
                TokenType::Gt,
                TokenType::ListType,
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
            ]) {
                return Err(format!("unexpected {:?} in type", self.peek()));
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
                | TokenType::RightBrace => (),
                _ => {
                    self.advance();
                }
            };

            match current {
                Token {
                    token: TokenType::LeftBrace,
                    ..
                } => {
                    let mut records = Vec::new();
                    let mut counter = 0;

                    loop {
                        let name = if self.check(TokenType::Name) {
                            let name = self.expect(TokenType::Name)?.name.expect("missing name");
                            self.expect(TokenType::Colon)?;
                            name
                        } else {
                            counter.to_string()
                        };

                        records.push((name, self.dtype()?));

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
                // TODO this might need some work for nested variants
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
                    token: TokenType::ListType,
                    ..
                } => {
                    self.expect(TokenType::LeftBracket)?;
                    let dtype = self.dtype()?;
                    self.expect(TokenType::RightBracket)?;
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
            && !self.check(TokenType::Gt)
            && !self.check(TokenType::Of)
            && !self.check(TokenType::VertBar)
            && !self.check(TokenType::Comma)
            && !self.check(TokenType::RightBrace)
            && !self.check(TokenType::Dot)
        {
            terms.push(self.primary()?);
        }

        let head = &terms[0];
        let tail = &terms[1..];
        let mut term = tail.iter().fold(head.clone(), |acc, t| app(acc, t.clone()));

        // check if we are accessing a record from this term, possibly multiple times
        while self.check(TokenType::Dot) {
            self.expect(TokenType::Dot)?;
            let accessor = self
                .expect(TokenType::Name)?
                .name
                .expect("missing record accessor");
            term = Term::Proj(box term, accessor);
        }

        Ok(term)
    }

    fn primary(&mut self) -> Result<Term, String> {
        match self.advance() {
            Token {
                token: TokenType::Lt,
                ..
            } => {
                let var_name = self.expect(TokenType::Name)?.name.expect("missing name");
                self.expect(TokenType::Equal)?;
                let term = self.expr()?;
                self.expect(TokenType::Gt)?;
                self.expect(TokenType::As)?;
                let dtype = self.dtype()?;
                Ok(Term::Tag(var_name, box term, dtype))
            }
            Token {
                token: TokenType::LeftBrace,
                ..
            } => {
                let mut records: Vec<(String, Term)> = Vec::new();
                let mut counter = 0;

                while !self.check(TokenType::RightBrace) {
                    // encodes tuples as records with a numeric name
                    let rec_name = if self.check(TokenType::Name) {
                        let name = self.expect(TokenType::Name)?.name.expect("missing name");
                        self.expect(TokenType::Equal)?;
                        name
                    } else {
                        counter.to_string()
                    };

                    let term = self.expr()?;

                    if self.check(TokenType::Comma) {
                        self.advance();
                    }

                    records.push((rec_name, term));
                    counter += 1;
                }

                self.expect(TokenType::RightBrace)?;

                Ok(Term::Record(records))
            }
            Token {
                token: TokenType::Case,
                ..
            } => {
                // TODO this might fail for more complex expressions
                let case_term = self.expr()?;
                self.expect(TokenType::Of)?;
                let mut cases: Vec<(String, String, Term)> = Vec::new();

                while self.check(TokenType::Lt) {
                    self.expect(TokenType::Lt)?;
                    let var_name = self.expect(TokenType::Name)?.name.expect("missing name");
                    self.expect(TokenType::Equal)?;
                    let binding = self.expect(TokenType::Name)?.name.expect("missing name");
                    self.expect(TokenType::Gt)?;
                    self.expect(TokenType::Arrow)?;
                    let term = self.expr()?;
                    cases.push((var_name, binding, term));

                    if self.check(TokenType::VertBar) {
                        self.advance();
                    } else {
                        break;
                    }
                }
                Ok(Term::Case(box case_term, cases))
            }
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
                self.expect(TokenType::RightBracket)?;

                // start of the list
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
                self.expect(TokenType::RightBracket)?;
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
            Token {
                token: TokenType::Let,
                ..
            } => {
                let binding = self.expect(TokenType::Name)?;
                //                self.expect(TokenType::Colon)?;
                //                let dtype = self.dtype()?;
                self.expect(TokenType::Equal)?;
                let t1 = self.expr()?;
                self.expect(TokenType::In)?;
                let t2 = self.expr()?;
                if let Some(name) = binding.name {
                    Ok(Term::Let(name, box t1, box t2))
                //                    Ok(app(abs(name, dtype, e2), e1))
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
                self.expect(TokenType::Dot)?;
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
                self.previous()
            )),
        }
    }
}
