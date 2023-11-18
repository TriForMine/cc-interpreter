use std::collections::HashMap;
use crate::expr::Expr;
use crate::interpreter::Interpreter;
use crate::scanner::Token;
use crate::stmt::Stmt;
use anyhow::{Result};

pub struct Resolver {
    interpreter: Interpreter,
    scopes: Vec<HashMap<String, bool>>
}

impl Resolver {
    pub fn new(interpreter: Interpreter) -> Self {
        Self {
            interpreter,
            scopes: Vec::new()
        }
    }

    pub fn resolve(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Block { statements: _ } => self.resolve_block(stmt)?,
            Stmt::Var { name: _, initializer: _ } => self.resolve_var(stmt)?,
            Stmt::Function { name: _, params: _, body: _ } => self.resolve_function(stmt)?,
            Stmt::Expression { expression } => self.resolve_expr(expression)?,
            Stmt::If { condition: _, then_branch: _, else_branch: _ } => self.resolve_if(stmt)?,
            Stmt::Print { expression } => self.resolve_expr(expression)?,
            Stmt::Return { keyword: _, value: None } => (),
            Stmt::Return { keyword: _, value: Some(value) } => self.resolve_expr(value)?,
            Stmt::While { condition, body } => {
                self.resolve_expr(condition)?;
                self.resolve(body)?;
            }
        }

        Ok(())
    }

    fn resolve_statements(&mut self, statements: &Vec<Box<Stmt>>) -> Result<()> {
        for stmt in statements {
            self.resolve(stmt.as_ref())?;
        }

        Ok(())
    }

    fn resolve_block(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Block { statements } => {
                self.begin_scope();
                self.resolve_statements(statements)?;
                self.end_scope();
            }
            _ => panic!("Expected Stmt::Block, got {:?}", stmt)
        }

        Ok(())
    }

    fn resolve_var(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Var { name, initializer } => {
                self.declare(name);
                self.resolve_expr(initializer)?;
                self.define(name);
            }
            _ => panic!("Expected Stmt::Var, got {:?}", stmt)
        }

        Ok(())
    }

    fn resolve_function(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Function { name, params, body } => {
                self.declare(name);
                self.define(name);

                self.resolve_function_helper(params, body)?;
            }
            _ => panic!("Expected Stmt::Function, got {:?}", stmt)
        }

        Ok(())
    }

    fn resolve_if(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::If { condition, then_branch, else_branch } => {
                self.resolve_expr(condition)?;
                self.resolve(then_branch)?;

                if let Some(else_branch) = else_branch {
                    self.resolve(else_branch)?;
                }
            }
            _ => panic!("Expected Stmt::If, got {:?}", stmt)
        }

        Ok(())
    }

    fn resolve_function_helper(&mut self,  params: &Vec<Token>, body: &Vec<Box<Stmt>>) -> Result<()> {
        self.begin_scope();
        self.resolve_function_params(params)?;
        self.resolve_statements(body)?;
        self.end_scope();

        Ok(())
    }

    fn resolve_function_params(&mut self, params: &Vec<Token>) -> Result<()> {
        for param in params {
            self.declare(param);
            self.define(param);
        }

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop().expect("Stack overflow");
    }

    fn declare(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                panic!("Variable {} already declared in this scope", name.lexeme);
            }
            scope.insert(name.lexeme.clone(), false);
        }
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true);
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Variable { name : _} => self.resolve_expr_var(expr),
            Expr::Assign { name: _, value: _ } => self.resolve_expr_assign(expr),
            Expr::Binary { left, operator: _, right } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            }
            Expr::Call { callee, arguments, .. } => {
                self.resolve_expr(callee)?;
                for argument in arguments {
                    self.resolve_expr(argument)?;
                }
                Ok(())
            }
            Expr::Grouping { expression } => self.resolve_expr(expression),
            Expr::Literal { value: _ } => Ok(()),
            Expr::Logical { left, operator: _, right } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            }
            Expr::Unary { operator: _, right } => self.resolve_expr(right),
            Expr::AnonFunction { parameters, body } => self.resolve_function_helper(parameters, body)
        }
    }

    fn resolve_expr_var(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Variable { name } => {
                if let Some(scope) = self.scopes.last() {
                    if scope.get(&name.lexeme) == Some(&false) {
                        anyhow::bail!("Cannot read local variable in its own initializer");
                    }

                    self.resolve_local(expr, name)?;
                }
            }
            _ => panic!("Expected Expr::Variable, got {:?}", expr)
        }

        Ok(())
    }

    fn resolve_local(&mut self, expr: &Expr, name: &Token) -> Result<()> {
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&name.lexeme) {
                self.interpreter.resolve(expr, i)?;
                return Ok(());
            }
        }

        Ok(())
    }

    fn resolve_expr_assign(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Assign { name, value } => {
                self.resolve_expr(value)?;
                self.resolve_local(expr, name)?;
            }
            _ => panic!("Expected Expr::Assign, got {:?}", expr)
        }

        Ok(())
    }
}