use crate::expr::{Expr, LiteralValue};
use crate::scanner::{Token, TokenType};
use crate::stmt::Stmt;
use std::fmt::Display;
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

enum FunctionKind {
    Function,
    //Method,
}

impl Display for FunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionKind::Function => write!(f, "function")
        }
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
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
        } else if self.match_token(vec![TokenType::Fun]) {
            self.function(FunctionKind::Function)
        } else {
            self.statement()
        }
    }

    fn function(&mut self, kind: FunctionKind) -> Result<Stmt, std::io::Error> {
        let name = self.consume(TokenType::Identifier, &format!("Expect {} name.", kind))?;
        self.consume(
            TokenType::LeftParen,
            &format!("Expect '(' after {} name.", kind),
        )?;
        let mut params = Vec::new();

        if !self.check(TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        "Can't have more than 255 parameters.",
                    ));
                }

                params.push(self.consume(TokenType::Identifier, "Expect parameter name.")?);

                if !self.match_token(vec![TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;
        self.consume(
            TokenType::LeftBrace,
            &format!("Expect '{{' before {} body.", kind),
        )?;
        let body = match self.block_statement() {
            Ok(Stmt::Block { statements }) => statements,
            _ => {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    "Expect function body.",
                ))
            }
        };

        Ok(Stmt::Function { name, params, body })
    }

    fn var_declaration(&mut self) -> Result<Stmt, std::io::Error> {
        let name = self.consume(TokenType::Identifier, "Expect variable name.")?;

        let initializer = if self.match_token(vec![TokenType::Equal]) {
            self.expression()?
        } else {
            Expr::new_literal(LiteralValue::Nil)
        };

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;

        Ok(Stmt::Var {
            name: name.clone(),
            initializer: initializer,
        })
    }

    fn statement(&mut self) -> Result<Stmt, std::io::Error> {
        if self.match_token(vec![TokenType::Print]) {
            self.print_statement()
        } else if self.match_token(vec![TokenType::LeftBrace]) {
            self.block_statement()
        } else if self.match_token(vec![TokenType::While]) {
            self.while_statement()
        } else if self.match_token(vec![TokenType::For]) {
            self.for_statement()
        } else if self.match_token(vec![TokenType::If]) {
            self.if_statement()
        } else if self.match_token(vec![TokenType::Return]) {
            self.return_statement()
        } else {
            self.expression_statement()
        }
    }

    fn return_statement(&mut self) -> Result<Stmt, std::io::Error> {
        let keyword = self.previous();
        let value = if !self.check(TokenType::Semicolon) {
            self.expression()?
        } else {
            Expr::new_literal(LiteralValue::Nil)
        };

        self.consume(TokenType::Semicolon, "Expect ';' after return value.")?;

        Ok(Stmt::Return {
            keyword,
            value: Some(value),
        })
    }

    fn for_statement(&mut self) -> Result<Stmt, std::io::Error> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;

        let initializer = if self.match_token(vec![TokenType::Semicolon]) {
            None
        } else if self.match_token(vec![TokenType::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if !self.check(TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenType::Semicolon, "Expect ';' after loop condition.")?;

        let increment = if !self.check(TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Stmt::Block {
                statements: vec![
                    Box::new(body),
                    Box::new(Stmt::Expression {
                        expression: increment,
                    }),
                ],
            };
        }

        if let Some(condition) = condition {
            body = Stmt::While {
                condition,
                body: Box::new(body),
            };
        }

        if let Some(initializer) = initializer {
            body = Stmt::Block {
                statements: vec![Box::new(initializer), Box::new(body)],
            };
        }

        Ok(body)
    }

    fn while_statement(&mut self) -> Result<Stmt, std::io::Error> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;
        let body = Box::new(self.statement()?);

        Ok(Stmt::While { condition, body })
    }

    fn if_statement(&mut self) -> Result<Stmt, std::io::Error> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;

        let then_branch = Box::new(self.statement()?);

        let else_branch = if self.match_token(vec![TokenType::Else]) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn block_statement(&mut self) -> Result<Stmt, std::io::Error> {
        let mut statements = Vec::new();

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            statements.push(Box::new(self.declaration()?));
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;

        Ok(Stmt::Block { statements })
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

    fn expression(&mut self) -> Result<Expr, std::io::Error> {
        self.assignment()
    }

    fn function_expression(&mut self) -> Result<Expr, std::io::Error> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'fun'.")?;
        let mut params = Vec::new();

        if !self.check(TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        "Can't have more than 255 parameters.",
                    ));
                }

                params.push(self.consume(TokenType::Identifier, "Expect parameter name.")?);

                if !self.match_token(vec![TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;
        self.consume(
            TokenType::LeftBrace,
            "Expect '{' before function body.",
        )?;
        let body = match self.block_statement() {
            Ok(Stmt::Block { statements }) => statements,
            _ => {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    "Expect function body.",
                ))
            }
        };

        Ok(Expr::new_function(params, body))
    }

    fn assignment(&mut self) -> Result<Expr, std::io::Error> {
        let expr = self.or()?;

        if self.match_token(vec![TokenType::Equal]) {
            let value = self.assignment()?;

            match expr {
                Expr::Variable { name } => Ok(Expr::new_assign(name, value)),
                _ => Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    "Invalid assignment target.",
                )),
            }
        } else if self.match_token(vec![TokenType::PlusEqual]) {
            let value = self.assignment()?;

            match expr {
                Expr::Variable { name } => Ok(Expr::new_assign(
                    name.clone(),
                    Expr::new_binary(
                        Expr::new_variable(name),
                        Token::new(TokenType::Plus, "+".to_string(), None, 1),
                        value,
                    ),
                )),
                _ => Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    "Invalid assignment target.",
                )),
            }
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> Result<Expr, std::io::Error> {
        let mut expr = self.and()?;

        while self.match_token(vec![TokenType::Or]) {
            let operator = self.previous();
            let right = self.and()?;
            expr = Expr::new_logical(expr, operator, right);
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, std::io::Error> {
        let mut expr = self.equality()?;

        while self.match_token(vec![TokenType::And]) {
            let operator = self.previous();
            let right = self.equality()?;
            expr = Expr::new_logical(expr, operator, right);
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, std::io::Error> {
        let mut expr = self.comparison()?;

        while self.match_token(vec![TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expr::new_binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, std::io::Error> {
        let mut expr = self.term()?;

        while self.match_token(vec![
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous();
            let right = self.term()?;
            expr = Expr::new_binary(expr, operator, right);
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

    fn advance(&mut self) -> Token {
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

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn term(&mut self) -> Result<Expr, std::io::Error> {
        let mut expr = self.factor()?;

        while self.match_token(vec![TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous();
            let right = self.factor()?;
            expr = Expr::new_binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, std::io::Error> {
        let mut expr = self.unary()?;

        while self.match_token(vec![TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expr::new_binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, std::io::Error> {
        if self.match_token(vec![TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.unary()?;
            return Ok(Expr::new_unary(operator, right));
        }

        self.call()
    }

    fn call(&mut self) -> Result<Expr, std::io::Error> {
        let mut expr = self.primary()?;

        loop {
            if self.match_token(vec![TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, std::io::Error> {
        let mut arguments = Vec::new();

        if !self.check(TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        "Can't have more than 255 arguments.",
                    ));
                }

                arguments.push(self.expression()?);

                if !self.match_token(vec![TokenType::Comma]) {
                    break;
                }
            }
        }

        let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments.")?;

        Ok(Expr::new_call(callee, paren, arguments))
    }

    fn primary(&mut self) -> Result<Expr, std::io::Error> {
        match self.peek().token_type {
            TokenType::False => {
                self.advance();
                Ok(Expr::new_literal(LiteralValue::Boolean(false)))
            }
            TokenType::True => {
                self.advance();
                Ok(Expr::new_literal(LiteralValue::Boolean(true)))
            }
            TokenType::Nil => {
                self.advance();
                Ok(Expr::new_literal(LiteralValue::Nil))
            }
            TokenType::Int => {
                self.advance();
                Ok(Expr::new_literal(LiteralValue::Integer(
                    self.previous().lexeme.parse().unwrap(),
                )))
            }
            TokenType::Float => {
                self.advance();
                Ok(Expr::new_literal(LiteralValue::Float(
                    self.previous().lexeme.parse().unwrap(),
                )))
            }
            TokenType::String => {
                self.advance();
                Ok(Expr::new_literal(LiteralValue::String(
                    self.previous().lexeme,
                )))
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
                Ok(Expr::new_grouping(expr))
            }
            TokenType::Identifier => {
                self.advance();
                Ok(Expr::new_variable(self.previous()))
            }
            TokenType::Fun => {
                self.advance();
                self.function_expression()
            }
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "Expect expression.",
            )),
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<Token, std::io::Error> {
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
        let tokens = vec![
            Token::new(TokenType::Int, "123".to_string(), None, 1),
            Token::new(TokenType::Star, "*".to_string(), None, 1),
            Token::new(TokenType::LeftParen, "(".to_string(), None, 1),
            Token::new(TokenType::Float, "45.67".to_string(), None, 1),
            Token::new(TokenType::Minus, "-".to_string(), None, 1),
            Token::new(TokenType::Int, "89".to_string(), None, 1),
            Token::new(TokenType::RightParen, ")".to_string(), None, 1),
            Token::new(TokenType::Eof, "".to_string(), None, 1),
        ];

        let mut parser = Parser::new(tokens);
        let expr = parser.expression().unwrap();

        assert_eq!(expr.to_string(), "(* 123 (group (- 45.67 89)))");
    }

    #[test]
    fn test_comparison() {
        let tokens = vec![
            Token::new(TokenType::Int, "123".to_string(), None, 1),
            Token::new(TokenType::Greater, ">".to_string(), None, 1),
            Token::new(TokenType::Int, "89".to_string(), None, 1),
            Token::new(TokenType::Eof, "".to_string(), None, 1),
        ];

        let mut parser = Parser::new(tokens);
        let expr = parser.expression().unwrap();

        assert_eq!(expr.to_string(), "(> 123 89)");
    }

    #[test]
    fn test_if() {
        let tokens = vec![
            Token::new(TokenType::If, "if".to_string(), None, 1),
            Token::new(TokenType::LeftParen, "(".to_string(), None, 1),
            Token::new(TokenType::True, "true".to_string(), None, 1),
            Token::new(TokenType::RightParen, ")".to_string(), None, 1),
            Token::new(TokenType::Print, "print".to_string(), None, 1),
            Token::new(TokenType::Int, "123".to_string(), None, 1),
            Token::new(TokenType::Semicolon, ":".to_string(), None, 1),
            Token::new(TokenType::Eof, "".to_string(), None, 1),
        ];

        let mut parser = Parser::new(tokens);
        let stmt = parser.statement().unwrap();

        assert_eq!(
            stmt.to_string(),
            "if (true) print 123;"
        );
    }
}
