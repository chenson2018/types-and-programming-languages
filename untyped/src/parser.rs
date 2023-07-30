//#[derive(Debug)]
//pub struct Parser<'a> {
//    current: usize,
//    tokens: &'a Vec<Token>,
//}
//
//impl<'a> From<&'a Scanner> for Parser<'a> {
//    fn from(scanner: &'a Scanner) -> Self {
//        let tokens = &scanner.tokens;
//        Parser::new(tokens)
//    }
//}
//
//impl<'a> Parser<'a> {
//    fn new(tokens: &'a Vec<Token>) -> Self {
//        Self { current: 0, tokens }
//    }
//
//    fn peek(&self) -> Token {
//        self.tokens[self.current].clone()
//    }
//
//    /// return the previous token (or current if at the first token)
//    fn previous(&mut self) -> Token {
//        if self.current == 0 {
//            self.peek()
//        } else {
//            self.tokens[self.current - 1].clone()
//        }
//    }
//
//    /// check if all statements have been parsed
//    fn is_end(&mut self) -> bool {
//        self.peek().token == TokenType::Eof
//    }
//
//    /// return the current token and advance the parser one token
//    fn advance(&mut self) -> Token {
//        if !self.is_end() {
//            self.current += 1;
//        }
//        self.previous()
//    }
//
//    /// check if the current token matches
//    fn check(&mut self, t: TokenType) -> bool {
//        if self.is_end() {
//            return false;
//        }
//        self.peek().token == t
//    }
//
//    fn match_any<T>(&mut self, types: T) -> bool
//    where
//        T: IntoIterator<Item = TokenType>,
//    {
//        for t in types {
//            if self.check(t) {
//                self.advance();
//                return true;
//            }
//        }
//        false
//    }
//
//}
//
