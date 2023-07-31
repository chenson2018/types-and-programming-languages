use crate::term::Term;
use std::collections::HashMap;

lazy_static! {
    static ref TOKENS_SINGLE: HashMap<char, TokenType> = {
        let mut m = HashMap::new();
        m.insert('(', TokenType::LeftParen);
        m.insert(')', TokenType::RightParen);
        m.insert('λ', TokenType::Lambda);
        m.insert('\\', TokenType::Lambda);
        m
    };
    // leaving this as a reference for later expansion
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let m = HashMap::new();
        m
    };
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    Lambda,
    Name,
    LeftParen,
    RightParen,

    Eof,

    // constants that are church encoded
    Nat,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub(crate) token: TokenType,
    pub(crate) term: Option<Term>,
}

pub struct Scanner {
    source: Vec<char>,
    pub(crate) tokens: Vec<Token>,
    current: usize,
    start: usize,
}

impl Scanner {
    pub fn new(s: &str) -> Self {
        Scanner {
            source: s.chars().collect::<Vec<char>>(),
            tokens: Vec::new(),
            current: 0,
            start: 0,
        }
    }

    pub fn borrow_tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1]
    }

    fn is_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn peek(&mut self) -> char {
        if self.is_end() {
            '\0'
        } else {
            self.source[self.current]
        }
    }

    fn add_token(&mut self, token: TokenType) {
        self.tokens.push(Token { token, term: None });
    }

    fn add_term_token(&mut self, token: TokenType, term: Term) {
        self.tokens.push(Token {
            token,
            term: Some(term),
        });
    }

    fn lexeme(&self) -> String {
        self.source[self.start..self.current]
            .iter()
            .collect::<String>()
    }

    fn identifier(&mut self) {
        while self.peek().is_alphabetic() {
            self.advance();
        }

        let binding = self.lexeme();
        let lexeme = binding.as_str();

        if let Some(keyword_tt) = KEYWORDS.get(&lexeme) {
            match keyword_tt {
                _ => self.add_token(*keyword_tt),
            }
        } else {
            self.add_term_token(TokenType::Name, Term::Var(binding));
        };
    }

    fn nat(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        let nat_term: Term = self.lexeme().parse::<usize>().unwrap().into();
        self.add_term_token(TokenType::Nat, nat_term);
    }

    fn scan_token(&mut self) -> Result<(), String> {
        let c = self.advance();

        if self.is_end() {
            self.add_token(TokenType::Eof);
            Ok(())
        } else if let Some(single_tt) = TOKENS_SINGLE.get(&c) {
            self.add_token(*single_tt);
            Ok(())
        } else {
            match c {
                ' ' | '\r' | '\t' | '.' => Ok(()),
                _ => {
                    if c.is_alphabetic() {
                        self.identifier();
                        Ok(())
                    } else if c.is_ascii_digit() {
                        self.nat();
                        Ok(())
                    } else {
                        Err(format!("unexpected character {c}"))
                    }
                }
            }
        }
    }

    pub fn scan(&mut self) -> Result<(), String> {
        while !self.is_end() {
            self.start = self.current;
            if let Err(err) = self.scan_token() {
                return Err(err);
            }
        }
        Ok(())
    }
}
