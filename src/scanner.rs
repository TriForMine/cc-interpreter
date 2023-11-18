use anyhow::{bail, Result};
use std::fmt::Display;
use std::hash::Hash;
use std::iter::Iterator;

pub struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            source,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan(&mut self) -> Result<Vec<Token>> {
        let mut errors = vec![];

        while !self.is_at_end() {
            self.start = self.current;

            match self.scan_token() {
                Ok(_) => (),
                Err(e) => errors.push(e),
            }
        }

        self.tokens
            .push(Token::new(TokenType::Eof, "".to_string(), None, self.line));

        if errors.len() > 0 {
            let joined = errors
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join("\n");

            bail!(joined);
        }

        Ok(self.tokens.clone())
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(self: &mut Self) -> Result<()> {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => match self.peek() {
                '=' => {
                    self.advance();
                    self.add_token(TokenType::PlusEqual)
                }
                _ => self.add_token(TokenType::Plus),
            },
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '!' => {
                let token_type = if self.match_char('=') {
                    self.advance();
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };

                self.add_token(token_type)
            }
            '=' => {
                let token_type = if self.match_char('=') {
                    self.advance();
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };

                self.add_token(token_type)
            }
            '<' => {
                let token_type = if self.match_char('=') {
                    self.advance();
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };

                self.add_token(token_type)
            }
            '>' => {
                let token_type = if self.match_char('=') {
                    self.advance();
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };

                self.add_token(token_type)
            }
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                    Ok(())
                } else {
                    self.add_token(TokenType::Slash)
                }
            }
            '|' => {
                let token_type = if self.match_char('|') {
                    self.advance();
                    TokenType::Or
                } else if self.match_char('>') {
                    self.advance();
                    TokenType::Pipe
                } else {
                    bail!("Unexpected character {}", c)
                };

                self.add_token(token_type)
            }
            '&' => {
                let token_type = if self.match_char('&') {
                    self.advance();
                    TokenType::And
                } else {
                    bail!("Unexpected character {}", c)
                };

                self.add_token(token_type)
            }
            ' ' | '\r' | '\t' => Ok(()),
            '\n' => {
                self.line += 1;
                Ok(())
            }
            '"' => self.string(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
            _ => bail!("Unexpected character {}", c),
        }?;

        Ok(())
    }

    fn identifier(&mut self) -> Result<()> {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let text = &self.source[self.start..self.current];

        match text {
            "and" => self.add_token(TokenType::And),
            "class" => self.add_token(TokenType::Class),
            "else" => self.add_token(TokenType::Else),
            "false" => {
                self.add_token_literal(TokenType::False, Some(Literal::False), "false".to_string())
            }
            "for" => self.add_token(TokenType::For),
            "fun" => self.add_token(TokenType::Fun),
            "if" => self.add_token(TokenType::If),
            "nil" => self.add_token_literal(TokenType::Nil, Some(Literal::Nil), "nil".to_string()),
            "or" => self.add_token(TokenType::Or),
            "print" => self.add_token(TokenType::Print),
            "return" => self.add_token(TokenType::Return),
            "super" => self.add_token(TokenType::Super),
            "this" => self.add_token(TokenType::This),
            "true" => {
                self.add_token_literal(TokenType::True, Some(Literal::True), "true".to_string())
            }
            "var" => self.add_token(TokenType::Var),
            "while" => self.add_token(TokenType::While),
            _ => self.add_token(TokenType::Identifier),
        }
    }

    fn number(&mut self) -> Result<()> {
        // The "1." should be recognized as a float, not an int, if there is a digit or nothing after the decimal point

        while self.peek().is_digit(10) {
            self.advance();
        }

        if self.peek() == '.' && (self.peek_next().is_digit(10)) {
            self.advance();

            while self.peek().is_digit(10) {
                self.advance();
            }
        }

        // 1. + 1. should be recognized as two floats, not two ints

        if self.peek() == '.' && (self.peek_next().is_whitespace() || self.peek_next() == '\0') {
            self.advance();
        }

        let value = &self.source[self.start..self.current];

        if value.contains('.') {
            let float_value = value.parse::<f64>().unwrap();
            self.add_token_literal(
                TokenType::Float,
                Some(Literal::FloatValue(float_value)),
                value.to_string(),
            )
        } else {
            let int_value = value.parse::<isize>().unwrap();
            self.add_token_literal(
                TokenType::Int,
                Some(Literal::IntValue(int_value)),
                value.to_string(),
            )
        }
    }

    fn string(&mut self) -> Result<()> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            bail!("Unterminated string");
        }

        self.advance();

        let value = self.source[self.start + 1..self.current - 1].to_string();
        self.add_token_literal(
            TokenType::String,
            Some(Literal::String(value.clone())),
            value,
        )
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }

        self.source.chars().nth(self.current + 1).unwrap()
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        self.source.chars().nth(self.current).unwrap()
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.source.chars().nth(self.current).unwrap() != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn advance(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        self.current += 1;
        self.source
            .chars()
            .nth(self.current - 1)
            .expect("Unexpected end of file")
    }

    fn add_token(&mut self, token_type: TokenType) -> Result<()> {
        let lexeme = self.source[self.start..self.current].to_string();
        self.add_token_literal(token_type, None, lexeme)
    }

    fn add_token_literal(
        &mut self,
        token_type: TokenType,
        literal: Option<Literal>,
        lexeme: String,
    ) -> Result<()> {
        self.tokens
            .push(Token::new(token_type, lexeme, literal, self.line));
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    PlusEqual,
    Pipe,

    // Literals
    Identifier,
    String,
    Int,
    Float,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // End of file
    Eof,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    IntValue(isize),
    FloatValue(f64),
    True,
    False,
    Nil,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Literal::String(s) => write!(f, "{}", s),
            Literal::IntValue(i) => write!(f, "{}", i),
            Literal::FloatValue(fl) => write!(f, "{}", fl),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

impl Eq for Literal {}

impl Hash for Literal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Literal::String(s) => s.hash(state),
            Literal::IntValue(i) => i.hash(state),
            Literal::FloatValue(fl) => fl.to_string().hash(state),
            Literal::True => "true".hash(state),
            Literal::False => "false".hash(state),
            Literal::Nil => "nil".hash(state),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<Literal>,
    pub line: usize,
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        literal: Option<Literal>,
        line: usize,
    ) -> Token {
        Token {
            token_type,
            lexeme,
            literal,
            line,
        }
    }

    pub fn to_string(&self) -> String {
        format!(
            "{} {} {}",
            self.token_type,
            self.lexeme,
            self.literal.as_ref().unwrap_or(&Literal::Nil)
        )
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.token_type.hash(state);
        self.lexeme.hash(state);
        self.literal.hash(state);
        self.line.hash(state);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_char_tokens() {
        let mut scanner = Scanner::new("(( )){};,+-*/");
        let tokens = scanner.scan().unwrap();

        assert_eq!(tokens.len(), 13);

        assert_eq!(tokens[0].token_type, TokenType::LeftParen);
        assert_eq!(tokens[1].token_type, TokenType::LeftParen);
        assert_eq!(tokens[2].token_type, TokenType::RightParen);
        assert_eq!(tokens[3].token_type, TokenType::RightParen);
        assert_eq!(tokens[4].token_type, TokenType::LeftBrace);
        assert_eq!(tokens[5].token_type, TokenType::RightBrace);
        assert_eq!(tokens[6].token_type, TokenType::Semicolon);
        assert_eq!(tokens[7].token_type, TokenType::Comma);
        assert_eq!(tokens[8].token_type, TokenType::Plus);
        assert_eq!(tokens[9].token_type, TokenType::Minus);
        assert_eq!(tokens[10].token_type, TokenType::Star);
        assert_eq!(tokens[11].token_type, TokenType::Slash);
        assert_eq!(tokens[12].token_type, TokenType::Eof);
    }

    #[test]
    fn test_multi_char_tokens() {
        let mut scanner = Scanner::new("!= == <= >=");
        let tokens = scanner.scan().unwrap();

        assert_eq!(tokens.len(), 5);

        assert_eq!(tokens[0].token_type, TokenType::BangEqual);
        assert_eq!(tokens[1].token_type, TokenType::EqualEqual);
        assert_eq!(tokens[2].token_type, TokenType::LessEqual);
        assert_eq!(tokens[3].token_type, TokenType::GreaterEqual);
        assert_eq!(tokens[4].token_type, TokenType::Eof);
    }

    #[test]
    fn test_comments() {
        let mut scanner = Scanner::new("+// comment\n+");
        let tokens = scanner.scan().unwrap();

        assert_eq!(tokens.len(), 3);

        assert_eq!(tokens[0].token_type, TokenType::Plus);
        assert_eq!(tokens[1].token_type, TokenType::Plus);
        assert_eq!(tokens[2].token_type, TokenType::Eof);
    }

    #[test]
    fn test_strings() {
        let mut scanner = Scanner::new("\"hello world\"");
        let tokens = scanner.scan().unwrap();

        assert_eq!(tokens.len(), 2);

        assert_eq!(tokens[0].token_type, TokenType::String);
        assert_eq!(
            tokens[0].literal,
            Some(Literal::String("hello world".parse().unwrap()))
        );
        assert_eq!(tokens[1].token_type, TokenType::Eof);
    }

    #[test]
    fn test_string_unterminated() {
        let mut scanner = Scanner::new("\"hello world");
        let tokens = scanner.scan();

        assert!(tokens.is_err());

        match tokens {
            Err(_) => (),
            Ok(_) => panic!("Expected an error"),
        }
    }

    #[test]
    fn test_string_multiline() {
        let mut scanner = Scanner::new("\"hello\nworld\"");
        let tokens = scanner.scan().unwrap();

        assert_eq!(tokens.len(), 2);

        assert_eq!(tokens[0].token_type, TokenType::String);
        assert_eq!(
            tokens[0].literal,
            Some(Literal::String("hello\nworld".parse().unwrap()))
        );
        assert_eq!(tokens[1].token_type, TokenType::Eof);
    }

    #[test]
    fn test_numbers() {
        let mut scanner = Scanner::new("123 123.456 1.0 1.");
        let tokens = scanner.scan().unwrap();

        assert_eq!(tokens.len(), 5);

        assert_eq!(tokens[0].token_type, TokenType::Int);
        assert_eq!(tokens[0].literal, Some(Literal::IntValue(123)));
        assert_eq!(tokens[1].token_type, TokenType::Float);
        assert_eq!(tokens[1].literal, Some(Literal::FloatValue(123.456)));
        assert_eq!(tokens[2].token_type, TokenType::Float);
        assert_eq!(tokens[2].literal, Some(Literal::FloatValue(1.0)));
        assert_eq!(tokens[3].token_type, TokenType::Float);
        assert_eq!(tokens[3].literal, Some(Literal::FloatValue(1.0)));
        assert_eq!(tokens[4].token_type, TokenType::Eof);
    }

    #[test]
    fn test_identifier() {
        let mut scanner = Scanner::new("test = 123");
        let tokens = scanner.scan().unwrap();

        assert_eq!(tokens.len(), 4);

        assert_eq!(tokens[0].token_type, TokenType::Identifier);
        assert_eq!(tokens[1].token_type, TokenType::Equal);
        assert_eq!(tokens[2].token_type, TokenType::Int);
        assert_eq!(tokens[3].token_type, TokenType::Eof);
    }

    #[test]
    fn test_keywords() {
        let mut scanner = Scanner::new(
            "and class else false for fun if nil or print return super this true var while",
        );
        let tokens = scanner.scan().unwrap();

        assert_eq!(tokens.len(), 17);

        assert_eq!(tokens[0].token_type, TokenType::And);
        assert_eq!(tokens[1].token_type, TokenType::Class);
        assert_eq!(tokens[2].token_type, TokenType::Else);
        assert_eq!(tokens[3].token_type, TokenType::False);
        assert_eq!(tokens[4].token_type, TokenType::For);
        assert_eq!(tokens[5].token_type, TokenType::Fun);
        assert_eq!(tokens[6].token_type, TokenType::If);
        assert_eq!(tokens[7].token_type, TokenType::Nil);
        assert_eq!(tokens[8].token_type, TokenType::Or);
        assert_eq!(tokens[9].token_type, TokenType::Print);
        assert_eq!(tokens[10].token_type, TokenType::Return);
        assert_eq!(tokens[11].token_type, TokenType::Super);
        assert_eq!(tokens[12].token_type, TokenType::This);
        assert_eq!(tokens[13].token_type, TokenType::True);
        assert_eq!(tokens[14].token_type, TokenType::Var);
        assert_eq!(tokens[15].token_type, TokenType::While);
        assert_eq!(tokens[16].token_type, TokenType::Eof);
    }

    #[test]
    fn test_self_addition() {
        let mut scanner = Scanner::new("a += 2");
        let tokens = scanner.scan().unwrap();

        assert_eq!(tokens.len(), 4);

        assert_eq!(tokens[0].token_type, TokenType::Identifier);
        assert_eq!(tokens[1].token_type, TokenType::PlusEqual);
        assert_eq!(tokens[2].token_type, TokenType::Int);
        assert_eq!(tokens[3].token_type, TokenType::Eof);
    }
}
