use crate::expr::{Expr, LiteralValue};
use crate::scanner::{Token, TokenType};
use crate::stmt::Stmt;
use std::borrow::Cow;

pub struct Parser<'a> {
    tokens: &'a [Token<'a>],
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token<'_>]) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, std::io::Error> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    errors.push(e);
                    self.synchronize();
                }
            }
        }

        if errors.len() == 0 {
            Ok(statements)
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                errors
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join("\n"),
            ))
        }
    }

    fn declaration(&mut self) -> Result<Stmt, std::io::Error> {
        if self.match_token(vec![TokenType::Var]) {
            match self.var_declaration() {
                Ok(stmt) => Ok(stmt),
                Err(e) => {
                    self.synchronize();
                    Err(e)
                }
            }
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt, std::io::Error> {
        let name = self.consume(TokenType::Identifier, "Expect variable name.")?;

        let initializer = if self.match_token(vec![TokenType::Equal]) {
            self.expression()?
        } else {
            &Expr::new_literal(&LiteralValue::Nil)
        };

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;

        Ok(Stmt::Var { name, initializer })
    }

    fn statement(&mut self) -> Result<Stmt, std::io::Error> {
        if self.match_token(vec![TokenType::Print]) {
            return self.print_statement();
        }

        self.expression_statement()
    }

    fn print_statement(&mut self) -> Result<Stmt, std::io::Error> {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Print { expression: value })
    }

    fn expression_statement(&mut self) -> Result<Stmt, std::io::Error> {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Expression { expression: value })
    }

    fn expression(&mut self) -> Result<&Expr, std::io::Error> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<&Expr, std::io::Error> {
        let expr = self.equality()?;

        if self.match_token(vec![TokenType::Equal]) {
            let value = self.assignment()?;

            match expr {
                Expr::Variable { name } => Ok(&Expr::new_assign(name, value)),
                _ => Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    "Invalid assignment target.",
                )),
            }
        } else {
            Ok(expr)
        }
    }

    fn equality(&mut self) -> Result<&Expr, std::io::Error> {
        let mut expr = self.comparison()?;

        while self.match_token(vec![TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = &Expr::new_binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<&Expr, std::io::Error> {
        let mut expr = self.term()?;

        while self.match_token(vec![
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous();
            let right = self.term()?;
            expr = &Expr::new_binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn match_token(&mut self, types: Vec<TokenType>) -> bool {
        for token_type in types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().token_type == token_type
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn term(&mut self) -> Result<&Expr, std::io::Error> {
        let mut expr = self.factor()?;

        while self.match_token(vec![TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous();
            let right = self.factor()?;
            expr = &Expr::new_binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<&Expr, std::io::Error> {
        let mut expr = self.unary()?;

        while self.match_token(vec![TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = &Expr::new_binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<&Expr, std::io::Error> {
        if self.match_token(vec![TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.unary()?;
            return Ok(&Expr::new_unary(operator, right));
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<&Expr, std::io::Error> {
        match self.peek().token_type {
            TokenType::False => {
                self.advance();
                Ok(&Expr::new_literal(&LiteralValue::Boolean(false)))
            }
            TokenType::True => {
                self.advance();
                Ok(&Expr::new_literal(&LiteralValue::Boolean(true)))
            }
            TokenType::Nil => {
                self.advance();
                Ok(&Expr::new_literal(&LiteralValue::Nil))
            }
            TokenType::Int => {
                self.advance();
                Ok(&Expr::new_literal(&LiteralValue::Integer(
                    self.previous().lexeme.parse().unwrap(),
                )))
            }
            TokenType::Float => {
                self.advance();
                Ok(&Expr::new_literal(&LiteralValue::Float(
                    self.previous().lexeme.parse().unwrap(),
                )))
            }
            TokenType::String => {
                self.advance();
                Ok(&Expr::new_literal(&LiteralValue::String(Cow::from(
                    self.previous().lexeme,
                ))))
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
                Ok(&Expr::new_grouping(expr))
            }
            TokenType::Identifier => {
                self.advance();
                Ok(&Expr::new_variable(self.previous()))
            }
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "Expect expression.",
            )),
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, std::io::Error> {
        if self.check(token_type) {
            self.advance();
            return Ok(self.previous());
        }

        Err(std::io::Error::new(std::io::ErrorKind::Other, message))
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            match self.peek().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => (),
            }

            self.advance();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner::TokenType;

    #[test]
    fn test_parser() {
        let tokens = &[
            Token::new(TokenType::Int, "123", None, 1),
            Token::new(TokenType::Star, "*", None, 1),
            Token::new(TokenType::LeftParen, "(", None, 1),
            Token::new(TokenType::Float, "45.67", None, 1),
            Token::new(TokenType::Minus, "-", None, 1),
            Token::new(TokenType::Int, "89", None, 1),
            Token::new(TokenType::RightParen, ")", None, 1),
            Token::new(TokenType::Eof, "", None, 1),
        ];

        let mut parser = Parser::new(tokens);
        let expr = parser.expression().unwrap();

        assert_eq!(expr.to_string(), "(* 123 (group (- 45.67 89)))");
    }

    #[test]
    fn test_comparison() {
        let tokens = &[
            Token::new(TokenType::Int, "123", None, 1),
            Token::new(TokenType::Greater, ">", None, 1),
            Token::new(TokenType::Int, "89", None, 1),
            Token::new(TokenType::Eof, "", None, 1),
        ];

        let mut parser = Parser::new(tokens);
        let expr = parser.expression().unwrap();

        assert_eq!(expr.to_string(), "(> 123 89)");
    }
}
