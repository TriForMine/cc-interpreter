use crate::expr::Expr;
use crate::scanner::Token;
use crate::stmt::Stmt;
use anyhow::Result;
use std::collections::HashMap;

#[derive(Copy, Clone, PartialEq)]
enum FunctionType {
    None,
    Function,
    Method,
}

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType,
    locals: HashMap<usize, usize>,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            current_function: FunctionType::None,
            locals: HashMap::new(),
        }
    }

    pub fn resolve(&mut self, stmt: &Vec<&Stmt>) -> Result<HashMap<usize, usize>> {
        self.resolve_statements(stmt)?;
        Ok(self.locals.clone())
    }

    pub fn resolve_internal(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Block { statements: _ } => self.resolve_block(stmt)?,
            Stmt::Var {
                name: _,
                initializer: _,
            } => self.resolve_var(stmt)?,
            Stmt::Class {
                name,
                methods,
                superclass,
                attributes,
            } => {
                if let Some(superclass) = superclass {
                    if let Expr::Variable {
                        name: super_name, ..
                    } = superclass
                    {
                        if name.lexeme == super_name.lexeme {
                            anyhow::bail!("A class cannot inherit from itself");
                        }
                    }

                    self.resolve_expr(superclass)?;
                    self.begin_scope();
                    self.scopes
                        .last_mut()
                        .expect("Stack overflow")
                        .insert("super".to_string(), true);
                }

                self.declare(name)?;
                self.define(name)?;

                self.begin_scope();
                self.scopes
                    .last_mut()
                    .expect("Stack overflow")
                    .insert("this".to_string(), true);

                for method in methods {
                    let declaration = FunctionType::Method;
                    self.resolve_function(&method, declaration)?;
                }

                self.resolve_statements(&attributes.iter().collect())?;

                self.end_scope();

                if superclass.is_some() {
                    self.end_scope();
                }
            }
            Stmt::Function {
                name: _,
                params: _,
                body: _,
            } => self.resolve_function(stmt, FunctionType::Function)?,
            Stmt::CmdFunction { name: _, cmd: _ } => self.resolve_var(stmt)?,
            Stmt::Expression { expression } => self.resolve_expr(expression)?,
            Stmt::If {
                condition: _,
                then_branch: _,
                else_branch: _,
            } => self.resolve_if(stmt)?,
            Stmt::Print { expression } => self.resolve_expr(expression)?,
            Stmt::Return { keyword: _, value } => {
                if self.current_function == FunctionType::None {
                    anyhow::bail!("Cannot return from top-level code");
                }

                if let Some(value) = value {
                    self.resolve_expr(value)?
                }
            }
            Stmt::While { condition, body } => {
                self.resolve_expr(condition)?;
                self.resolve_internal(body.as_ref())?;
            }
        }

        Ok(())
    }

    pub(crate) fn resolve_statements(&mut self, statements: &Vec<&Stmt>) -> Result<()> {
        for stmt in statements {
            self.resolve_internal(stmt)?;
        }

        Ok(())
    }

    fn resolve_block(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Block { statements } => {
                self.begin_scope();
                self.resolve_statements(&statements.iter().map(|b| b.as_ref()).collect())?;
                self.end_scope();
            }
            _ => panic!("Expected Stmt::Block, got {:?}", stmt),
        }

        Ok(())
    }

    fn resolve_var(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Var { name, initializer } => {
                self.declare(name)?;
                self.resolve_expr(initializer)?;
                self.define(name)?;
            }
            Stmt::CmdFunction { name, cmd: _ } => {
                self.declare(name)?;
                self.define(name)?;
            }
            _ => panic!("Expected Stmt::Var, got {:?}", stmt),
        }

        Ok(())
    }

    fn resolve_function(&mut self, stmt: &Stmt, function_type: FunctionType) -> Result<()> {
        match stmt {
            Stmt::Function { name, params, body } => {
                self.declare(name)?;
                self.define(name)?;

                self.resolve_function_helper(
                    params,
                    &body.iter().map(|b| b.as_ref()).collect(),
                    function_type,
                )?;
            }
            _ => panic!("Expected Stmt::Function, got {:?}", stmt),
        }

        Ok(())
    }

    fn resolve_if(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(condition)?;
                self.resolve_internal(then_branch)?;

                if let Some(else_branch) = else_branch {
                    self.resolve_internal(else_branch)?;
                }
            }
            _ => panic!("Expected Stmt::If, got {:?}", stmt),
        }

        Ok(())
    }

    fn resolve_function_helper(
        &mut self,
        params: &Vec<Token>,
        body: &Vec<&Stmt>,
        function_type: FunctionType,
    ) -> Result<()> {
        let enclosing_function = self.current_function;
        self.current_function = function_type;

        self.begin_scope();
        self.resolve_function_params(params)?;
        self.resolve_statements(body)?;
        self.end_scope();

        self.current_function = enclosing_function;

        Ok(())
    }

    fn resolve_function_params(&mut self, params: &Vec<Token>) -> Result<()> {
        for param in params {
            self.declare(param)?;
            self.define(param)?;
        }

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop().expect("Stack overflow");
    }

    fn declare(&mut self, name: &Token) -> Result<()> {
        if let Some(scope) = self.scopes.last_mut() {
            // Check if the variable is already declared in the current scope
            // If it is, get the line it was declared on

            if scope.contains_key(&name.lexeme) {
                anyhow::bail!(
                    "Line {}: Variable with this name already declared in this scope",
                    name.line
                );
            }

            scope.insert(name.lexeme.clone(), false);
        }

        Ok(())
    }

    fn define(&mut self, name: &Token) -> Result<()> {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true);
        }

        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Variable { .. } => self.resolve_expr_var(expr),
            Expr::Assign { .. } => self.resolve_expr_assign(expr),
            Expr::Binary { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            }
            Expr::Call {
                callee, arguments, ..
            } => {
                self.resolve_expr(callee)?;
                for argument in arguments {
                    self.resolve_expr(argument)?;
                }
                Ok(())
            }
            Expr::Get { object, .. } => self.resolve_expr(object),
            Expr::Grouping { expression, .. } => self.resolve_expr(expression),
            Expr::Literal { .. } => Ok(()),
            Expr::Logical { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            }
            Expr::Set { object, value, .. } => {
                self.resolve_expr(value)?;
                self.resolve_expr(object)
            }
            Expr::This { keyword, .. } => {
                if self.current_function != FunctionType::Method {
                    anyhow::bail!("Cannot use 'this' outside of a class");
                }

                self.resolve_local(keyword, expr.get_id())
            }
            Expr::TypeOf { expression, .. } => self.resolve_expr(expression),
            Expr::Super { keyword, .. } => {
                if self.current_function != FunctionType::Method {
                    anyhow::bail!("Cannot use 'super' outside of a class");
                }

                if self.scopes.len() < 3 || self.scopes[self.scopes.len() - 3].get("super") == None
                {
                    anyhow::bail!("Cannot use 'super' in a class with no superclass");
                }

                self.resolve_local(keyword, expr.get_id())
            }
            Expr::Unary { right, .. } => self.resolve_expr(right),
            Expr::AnonFunction {
                arguments: parameters,
                body,
                ..
            } => {
                let enclosing_function = self.current_function;
                self.current_function = FunctionType::Function;
                self.resolve_function_helper(
                    parameters,
                    &body.iter().map(|b| b.as_ref()).collect(),
                    FunctionType::Function,
                )?;
                self.current_function = enclosing_function;
                Ok(())
            }
        }
    }

    fn resolve_expr_var(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Variable { name, .. } => {
                if let Some(scope) = self.scopes.last() {
                    if scope.get(&name.lexeme) == Some(&false) {
                        anyhow::bail!("Cannot read local variable in its own initializer");
                    }

                    self.resolve_local(name, expr.get_id())?;
                }
            }
            _ => panic!("Expected Expr::Variable, got {:?}", expr),
        }

        Ok(())
    }

    fn resolve_local(&mut self, name: &Token, resolve_id: usize) -> Result<()> {
        let size = self.scopes.len();

        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&name.lexeme) {
                self.locals.insert(resolve_id, size - 1 - i);
                return Ok(());
            }
        }

        Ok(())
    }

    fn resolve_expr_assign(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Assign { name, value, .. } => {
                self.resolve_expr(value)?;
                self.resolve_local(name, expr.get_id())?;
            }
            _ => panic!("Expected Expr::Assign, got {:?}", expr),
        }

        Ok(())
    }
}
