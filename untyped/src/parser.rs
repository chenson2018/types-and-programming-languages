use crate::scanner::{Scanner, Token, TokenType};
use crate::term::{abs, app, Term};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Parser<'a> {
    current: usize,
    tokens: &'a Vec<Token>,
    env: HashMap<Term, Term>,
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
            env: Term::env(),
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

    fn match_any<T>(&mut self, types: T) -> bool
    where
        T: IntoIterator<Item = TokenType>,
    {
        for t in types {
            if self.check(t) {
                self.advance();
                return true;
            }
        }
        false
    }

    // TODO a raw body needs parens to work....
    pub fn parse(&mut self) -> Result<Term, &'static str> {
        match self.peek().token {
            TokenType::Lambda => self.abs(),
            TokenType::LeftParen => {
                self.advance();
                self.body()
            }
            _ => self.primary(),
        }
    }

    fn abs(&mut self) -> Result<Term, &'static str> {
        self.advance();
        let binding = self.advance();

        if let Some(Term::Var(name)) = binding.term {
            let body = self.body()?;
            Ok(abs(&name, body))
        } else {
            Err("lambda without binding")
        }
    }

    // TODO check termination???
    fn body(&mut self) -> Result<Term, &'static str> {
        let mut term = self.parse()?;
        while !self.check(TokenType::RightParen) && !self.is_end() {
            let e = self.parse()?;
            term = app(term, e);
        }
        self.advance();
        Ok(term)
    }

    fn primary(&mut self) -> Result<Term, &'static str> {
        if self.match_any([TokenType::Nat, TokenType::Name]) {
            if let Some(term) = self.previous().term {
                if let Some(lookup) = self.env.get(&term) {
                    Ok(lookup.clone())
                } else {
                    Ok(term)
                }
            } else {
                Err("Scanner created a literal without a value.")
            }
        } else {
            Err("empty program")
        }
    }
}
