use crate::term::Term;
use crate::types::Type;

use std::collections::HashMap;

lazy_static! {
    static ref TOKENS_SINGLE: HashMap<char, TokenType> = {
        HashMap::from([
            ('(', TokenType::LeftParen),
            (')', TokenType::RightParen),
            ('λ', TokenType::Lambda),
            ('\\', TokenType::Lambda),
            (':', TokenType::Colon),
            ('→', TokenType::Arrow),
            ('=', TokenType::Equal),
            (';', TokenType::In),
            ('.', TokenType::Dot),
        ])
    };
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        HashMap::from([
            ("succ", TokenType::Succ),
            ("pred", TokenType::Pred),
            ("iszero", TokenType::IsZero),
            ("if", TokenType::If),
            ("then", TokenType::Then),
            ("else", TokenType::Else),
            ("fix", TokenType::Fix),
            ("let", TokenType::Let),
        ])
    };
    static ref TYPES: HashMap<&'static str, (TokenType, Type)> = {
        HashMap::from([
            ("Bool", (TokenType::BoolType, Type::Bool)),
            ("Nat", (TokenType::NatType, Type::Nat)),
        ])
    };
    static ref VALUES: HashMap<&'static str, (TokenType, Term)> = {
        HashMap::from([
            ("false", (TokenType::False, Term::False)),
            ("true", (TokenType::True, Term::True)),
        ])
    };
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    Lambda,
    Name,
    LeftParen,
    RightParen,
    Dot,
    Eof,

    // if expressions
    If,
    Then,
    Else,

    // base terms/types
    Nat,
    Succ,
    False,
    True,
    IsZero,
    Pred,

    // for types
    BoolType,
    NatType,
    Colon,
    Arrow,

    // fix
    Fix,

    // let bindings
    Let,
    Equal,
    In,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub(crate) token: TokenType,
    pub(crate) term: Option<Term>,
    pub(crate) dtype: Option<Type>,
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
        self.tokens.push(Token {
            token,
            term: None,
            dtype: None,
        });
    }

    fn add_term_token(&mut self, token: TokenType, term: Term) {
        self.tokens.push(Token {
            token,
            term: Some(term),
            dtype: None,
        });
    }

    fn add_type_token(&mut self, token: TokenType, dtype: Type) {
        self.tokens.push(Token {
            token,
            term: None,
            dtype: Some(dtype),
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
            self.add_token(*keyword_tt)
        } else if let Some((tt, dtype)) = TYPES.get(&lexeme) {
            self.add_type_token(*tt, dtype.clone())
        } else if let Some((tt, value)) = VALUES.get(&lexeme) {
            self.add_term_token(*tt, value.clone())
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
                ' ' | '\r' | '\t' | '\n' => Ok(()),
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
