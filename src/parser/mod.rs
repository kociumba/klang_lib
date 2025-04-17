use std::collections::HashMap;
use crate::lexer::{Lexer, Token};
use crate::parser::ast::*;

pub(crate) mod ast;

/// Will parse the source code and produce a full ast
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn klang_parse() -> *mut Program {
    todo!("not implemented yet");
}

/// Will only quickly check the syntax and return any errors produced
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn klang_check() -> bool {
    todo!("not implemented yet");
}

pub struct Parser<'src> {
    lexer: &'src mut Lexer<'src>,
    current_token: Option<Token>,
    previous_token: Option<Token>,
    had_error: bool,
    errors: Vec<String>,
    current_span: SourceSpan,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: &'src mut Lexer<'src>) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: None,
            previous_token: None,
            had_error: false,
            errors: Vec::new(),
            current_span: SourceSpan {
                start: SourcePosition { line: 1, column: 1 },
                end: SourcePosition { line: 1, column: 1 },
            },
        };
        parser.advance();
        parser
    }

    fn advance(&mut self) -> Option<Token> {
        self.previous_token = self.current_token.clone();

        match self.lexer.next_token() {
            Ok(token) => {
                self.current_span = SourceSpan {
                    start: SourcePosition { line: token.start.0, column: token.start.1 },
                    end: SourcePosition { line: token.end.0, column: token.end.1 },
                };
                self.current_token = Some(token.token);
            }
            Err(e) => {
                self.had_error = true;
                self.errors.push(e);
                self.current_token = None;
            }
        }

        self.previous_token.clone()
    }

    fn check(&self, token_type: &Token) -> bool {
        match &self.current_token {
            Some(t) => t == token_type,
            None => false,
        }
    }

    fn match_token(&mut self, token_type: &Token) -> bool {
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume(&mut self, token_type: &Token, message: &str) -> Result<(), String> {
        if self.check(token_type) {
            self.advance();
            Ok(())
        } else {
            let error = format!("Expected {:#?}, got {:#?}. {}",
                                token_type, self.current_token, message);
            self.errors.push(error.clone());
            Err(error)
        }
    }

    fn current_span(&self) -> SourceSpan {
        self.current_span
    }

    pub fn parse_program(&mut self) -> Result<Program, Vec<String>> {
        let mut declarations = Vec::new();
        let symbol_table = SymbolTable::new();

        // Parse declarations until end of file
        while self.current_token != Some(Token::EOF) {
            match self.parse_declaration() {
                Ok(decl) => declarations.push(decl),
                Err(e) => self.errors.push(e),
            }
        }

        if self.had_error {
            Err(self.errors.clone())
        } else {
            Ok(Program {
                declarations,
                symbol_table,
            })
        }
    }

    fn parse_declaration(&mut self) -> Result<Declaration, String> {
        // Check the token to determine what kind of declaration we're parsing
        match &self.current_token {
            Some(Token::Type) => {
                self.advance(); // Consume 'type'
                self.parse_type_declaration()
            },
            Some(Token::Fun) => {
                self.advance(); // Consume 'fun'
                self.parse_function_declaration()
            },
            Some(Token::Var) | Some(Token::Const) => {
                let is_mutable = self.current_token == Some(Token::Var);
                self.advance(); // Consume 'var' or 'const'
                self.parse_variable_declaration(is_mutable)
                    .map(Declaration::VariableDeclaration)
            },
            _ => Err(format!("Expected declaration, got {:?}", self.current_token)),
        }
    }

    fn parse_type_declaration(&mut self) -> Result<Declaration, String> {
        // Example implementation for parsing type declarations
        // In a complete parser, this would handle both type aliases and structs
        let start_span = self.current_span();

        // Parse type name
        let name = if let Some(Token::Identifier(name)) = &self.current_token {
            let result = name.clone();
            self.advance(); // Consume identifier
            result
        } else {
            return Err(format!("Expected type name, got {:?}", self.current_token));
        };

        // Expect colon
        self.consume(&Token::Colon, "Expected colon after type name")?;

        // Parse type definition (struct or alias)
        let definition = if self.match_token(&Token::Struct) {
            // Parse struct definition
            self.consume(&Token::LeftBrace, "Expected '{' after struct")?;

            let mut fields = Vec::new();

            // Parse fields
            while !self.check(&Token::RightBrace) && self.current_token != Some(Token::EOF) {
                let field_name = if let Some(Token::Identifier(name)) = &self.current_token {
                    let result = name.clone();
                    self.advance(); // Consume identifier
                    result
                } else {
                    return Err(format!("Expected field name, got {:?}", self.current_token));
                };

                self.consume(&Token::Colon, "Expected colon after field name")?;

                let type_ref = self.parse_type_reference()?;

                self.consume(&Token::Comma, "Expected comma after field type")?;

                fields.push(StructField {
                    name: field_name,
                    type_ref,
                    span: self.current_span(),
                });
            }

            self.consume(&Token::RightBrace, "Expected '}' after struct fields")?;

            TypeDefinition::Struct(StructDefinition { fields })
        } else {
            // Parse type alias
            let type_ref = self.parse_type_reference()?;
            TypeDefinition::Alias(type_ref)
        };

        let end_span = self.current_span();

        Ok(Declaration::TypeDeclaration(TypeDeclaration {
            name,
            definition,
            span: SourceSpan {
                start: start_span.start,
                end: end_span.end,
            },
            symbol_id: None, // Will be filled during semantic analysis
        }))
    }

    fn parse_function_declaration(&mut self) -> Result<Declaration, String> {
        let start_span = self.current_span();

        // Parse function name
        let name = if let Some(Token::Identifier(name)) = &self.current_token {
            let result = name.clone();
            self.advance(); // Consume identifier
            result
        } else {
            return Err(format!("Expected function name, got {:?}", self.current_token));
        };

        // Parse parameters
        self.consume(&Token::LeftParen, "Expected '(' after function name")?;

        let parameters = if self.check(&Token::RightParen) {
            Vec::new() // No parameters
        } else {
            self.parse_parameter_list()?
        };

        self.consume(&Token::RightParen, "Expected ')' after parameters")?;

        // Parse optional return type
        let return_type = if self.match_token(&Token::Arrow) {
            self.parse_type_reference()?
        } else {
            TypeReference::Void // Default to void if no return type specified
        };

        // Parse function body
        let body = self.parse_block()?;

        let end_span = self.current_span();

        Ok(Declaration::FunctionDeclaration(FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
            span: SourceSpan {
                start: start_span.start,
                end: end_span.end,
            },
            symbol_id: None, // Will be filled during semantic analysis
        }))
    }

    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>, String> {
        let mut parameters = Vec::new();

        // Parse the first parameter
        parameters.push(self.parse_parameter()?);

        // Parse additional parameters
        while self.match_token(&Token::Comma) {
            parameters.push(self.parse_parameter()?);
        }

        Ok(parameters)
    }

    fn parse_parameter(&mut self) -> Result<Parameter, String> {
        let start_span = self.current_span();

        // Parse parameter name
        let name = if let Some(Token::Identifier(name)) = &self.current_token {
            let result = name.clone();
            self.advance(); // Consume identifier
            result
        } else {
            return Err(format!("Expected parameter name, got {:?}", self.current_token));
        };

        // Parse parameter type
        self.consume(&Token::Colon, "Expected ':' after parameter name")?;
        let type_ref = self.parse_type_reference()?;

        let end_span = self.current_span();

        Ok(Parameter {
            name,
            type_ref,
            span: SourceSpan {
                start: start_span.start,
                end: end_span.end,
            },
        })
    }

    fn parse_type_reference(&mut self) -> Result<TypeReference, String> {
        // Parse basic type or named type
        match &self.current_token {
            Some(Token::Int32) => {
                self.advance();
                Ok(TypeReference::Int32)
            },
            Some(Token::Int64) => {
                self.advance();
                Ok(TypeReference::Int64)
            },
            Some(Token::Float32) => {
                self.advance();
                Ok(TypeReference::Float32)
            },
            Some(Token::Float64) => {
                self.advance();
                Ok(TypeReference::Float64)
            },
            Some(Token::String) => {
                self.advance();
                Ok(TypeReference::String)
            },
            Some(Token::Bool) => {
                self.advance();
                Ok(TypeReference::Bool)
            },
            Some(Token::Void) => {
                self.advance();
                Ok(TypeReference::Void)
            },
            Some(Token::Identifier(name)) => {
                let result = name.clone();
                self.advance();
                Ok(TypeReference::Named(result))
            },
            Some(Token::Function) => {
                self.advance();

                // Parse function type
                self.consume(&Token::LeftParen, "Expected '(' after function declaration")?;

                // Parse parameter types
                let mut parameters = Vec::new();
                if !self.check(&Token::RightParen) {
                    parameters.push(self.parse_type_reference()?);

                    while self.match_token(&Token::Comma) {
                        parameters.push(self.parse_type_reference()?);
                    }
                }

                self.consume(&Token::RightParen, "Expected ')' after parameter types")?;
                self.consume(&Token::Arrow, "Expected '->' after parameter list")?;

                // Parse return type
                let return_type = Box::new(self.parse_type_reference()?);

                Ok(TypeReference::Function {
                    parameters,
                    return_type,
                })
            },
            _ => Err(format!("Expected type, got {:?}", self.current_token)),
        }
    }

    fn parse_variable_declaration(&mut self, is_mutable: bool) -> Result<VariableDeclaration, String> {
        let start_span = self.current_span();

        // Parse variable name
        let name = if let Some(Token::Identifier(name)) = &self.current_token {
            let result = name.clone();
            self.advance(); // Consume identifier
            result
        } else {
            return Err(format!("Expected variable name, got {:?}", self.current_token));
        };

        // Parse type
        self.consume(&Token::Colon, "Expected ':' after variable name")?;
        let type_ref = self.parse_type_reference()?;

        // Parse initializer if present
        let initializer = if self.match_token(&Token::Equal) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let end_span = self.current_span();

        Ok(VariableDeclaration {
            name,
            type_ref,
            initializer,
            is_mutable,
            span: SourceSpan {
                start: start_span.start,
                end: end_span.end,
            },
            symbol_id: None, // Will be filled during semantic analysis
        })
    }

    fn parse_block(&mut self) -> Result<Block, String> {
        let start_span = self.current_span();

        self.consume(&Token::LeftBrace, "Expected '{' at start of block")?;

        let mut statements = Vec::new();

        // Parse statements until '}'
        while !self.check(&Token::RightBrace) && self.current_token != Some(Token::EOF) {
            statements.push(self.parse_statement()?);
        }

        self.consume(&Token::RightBrace, "Expected '}' at end of block")?;

        let end_span = self.current_span();

        Ok(Block {
            statements,
            span: SourceSpan {
                start: start_span.start,
                end: end_span.end,
            },
            local_symbols: HashMap::new(), // Will be filled during semantic analysis
        })
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match &self.current_token {
            Some(Token::Var) | Some(Token::Const) => {
                let is_mutable = self.current_token == Some(Token::Var);
                self.advance(); // Consume 'var' or 'const'
                Ok(Statement::VariableDeclaration(self.parse_variable_declaration(is_mutable)?))
            },
            Some(Token::If) => {
                self.advance(); // Consume 'if'
                self.parse_if_statement()
            },
            Some(Token::While) => {
                self.advance(); // Consume 'while'
                self.parse_while_statement()
            },
            Some(Token::For) => {
                self.advance(); // Consume 'for'
                self.parse_for_statement()
            },
            Some(Token::Return) => {
                self.advance(); // Consume 'return'
                self.parse_return_statement()
            },
            Some(Token::LeftBrace) => {
                Ok(Statement::Block(self.parse_block()?))
            },
            _ => {
                // Try to parse as expression statement or assignment
                let expr = self.parse_expression()?;

                // If next token is an assignment operator, this is an assignment
                if let Some(op) = self.parse_assignment_operator() {
                    // Convert expression to LValue if possible
                    let target = match expr {
                        Expression::Identifier(name) => Ok(LValue::Identifier(name)),
                        Expression::MemberAccess { object, member, .. } => {
                            Ok(LValue::MemberAccess {
                                object,
                                member,
                            })
                        },
                        Expression::IndexAccess { array, index, .. } => {
                            Ok(LValue::IndexAccess {
                                array,
                                index,
                            })
                        },
                        _ => Err("Invalid assignment target".to_string()),
                    }?;

                    let value = self.parse_expression()?;

                    Ok(Statement::Assignment(Assignment {
                        target,
                        operator: op,
                        value,
                        span: self.current_span(),
                    }))
                } else {
                    // This is just an expression statement
                    Ok(Statement::ExpressionStatement(expr))
                }
            }
        }
    }

    fn parse_assignment_operator(&mut self) -> Option<AssignmentOperator> {
        match &self.current_token {
            Some(Token::Equal) => {
                self.advance();
                Some(AssignmentOperator::Simple)
            },
            Some(Token::PlusEqual) => {
                self.advance();
                Some(AssignmentOperator::Add)
            },
            Some(Token::MinusEqual) => {
                self.advance();
                Some(AssignmentOperator::Subtract)
            },
            // Add other assignment operators as needed
            _ => None,
        }
    }

    fn parse_if_statement(&mut self) -> Result<Statement, String> {
        let start_span = self.current_span();

        // Parse condition
        let condition = self.parse_expression()?;

        // Parse then branch
        let then_branch = self.parse_block()?;

        // Parse optional else branch
        let else_branch = if self.match_token(&Token::Else) {
            if self.check(&Token::If) {
                // This is an "else if", so parse another if statement
                Some(Box::new(self.parse_statement()?))
            } else {
                // This is a simple "else", so parse a block
                Some(Box::new(Statement::Block(self.parse_block()?)))
            }
        } else {
            None
        };

        let end_span = self.current_span();

        Ok(Statement::IfStatement(IfStatement {
            condition,
            then_branch,
            else_branch,
            span: SourceSpan {
                start: start_span.start,
                end: end_span.end,
            },
        }))
    }

    fn parse_while_statement(&mut self) -> Result<Statement, String> {
        let start_span = self.current_span();

        // Parse condition
        let condition = self.parse_expression()?;

        // Parse body
        let body = self.parse_block()?;

        let end_span = self.current_span();

        Ok(Statement::WhileStatement(WhileStatement {
            condition,
            body,
            span: SourceSpan {
                start: start_span.start,
                end: end_span.end,
            },
        }))
    }

    fn parse_for_statement(&mut self) -> Result<Statement, String> {
        let start_span = self.current_span();

        // Parse iterator variable
        let iterator = if let Some(Token::Identifier(name)) = &self.current_token {
            let result = name.clone();
            self.advance(); // Consume identifier
            result
        } else {
            return Err(format!("Expected iterator name, got {:?}", self.current_token));
        };

        // Parse 'in' keyword
        if let Some(Token::Identifier(word)) = &self.current_token {
            if word != "in" {
                return Err(format!("Expected 'in', got '{}'", word));
            }
            self.advance(); // Consume 'in'
        } else {
            return Err(format!("Expected 'in', got {:?}", self.current_token));
        }

        // Parse iterable expression
        let iterable = self.parse_expression()?;

        // Parse body
        let body = self.parse_block()?;

        let end_span = self.current_span();

        Ok(Statement::ForStatement(ForStatement {
            iterator,
            iterable,
            body,
            span: SourceSpan {
                start: start_span.start,
                end: end_span.end,
            },
        }))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        let start_span = self.current_span();

        // Parse optional return value
        let value = if self.check(&Token::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };

        let end_span = self.current_span();

        Ok(Statement::ReturnStatement(ReturnStatement {
            value,
            span: SourceSpan {
                start: start_span.start,
                end: end_span.end,
            },
        }))
    }

    fn parse_expression(&mut self) -> Result<Expression, String> {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> Result<Expression, String> {
        // Assignment has the lowest precedence
        self.parse_logical_or_expression()
    }

    fn parse_logical_or_expression(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_logical_and_expression()?;

        while self.match_token(&Token::Or) {
            let operator = BinaryOperator::Or;
            let right = self.parse_logical_and_expression()?;
            let span = self.current_span();

            expr = Expression::BinaryOperation {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
                type_ref: TypeReference::Unresolved, // Will be filled during type checking
            };
        }

        Ok(expr)
    }

    fn parse_logical_and_expression(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_equality_expression()?;

        while self.match_token(&Token::And) {
            let operator = BinaryOperator::And;
            let right = self.parse_equality_expression()?;
            let span = self.current_span();

            expr = Expression::BinaryOperation {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
                type_ref: TypeReference::Unresolved,
            };
        }

        Ok(expr)
    }

    fn parse_equality_expression(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_comparison_expression()?;

        loop {
            let operator = if self.match_token(&Token::EqualEqual) {
                BinaryOperator::Equal
            } else if self.match_token(&Token::NotEqual) {
                BinaryOperator::NotEqual
            } else {
                break;
            };

            let right = self.parse_comparison_expression()?;
            let span = self.current_span();

            expr = Expression::BinaryOperation {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
                type_ref: TypeReference::Unresolved,
            };
        }

        Ok(expr)
    }

    fn parse_comparison_expression(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_bitwise_expression()?;

        loop {
            let operator = if self.match_token(&Token::Less) {
                BinaryOperator::LessThan
            } else if self.match_token(&Token::LessEqual) {
                BinaryOperator::LessThanOrEqual
            } else if self.match_token(&Token::Greater) {
                BinaryOperator::GreaterThan
            } else if self.match_token(&Token::GreaterEqual) {
                BinaryOperator::GreaterThanOrEqual
            } else {
                break;
            };

            let right = self.parse_bitwise_expression()?;
            let span = self.current_span();

            expr = Expression::BinaryOperation {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
                type_ref: TypeReference::Unresolved,
            };
        }

        Ok(expr)
    }

    fn parse_bitwise_expression(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_shift_expression()?;

        loop {
            let operator = if self.match_token(&Token::BitwiseAnd) {
                BinaryOperator::BitwiseAnd
            } else if self.match_token(&Token::BitwiseOr) {
                BinaryOperator::BitwiseOr
            } else if self.match_token(&Token::BitwiseXor) {
                BinaryOperator::BitwiseXor
            } else {
                break;
            };

            let right = self.parse_shift_expression()?;
            let span = self.current_span();

            expr = Expression::BinaryOperation {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
                type_ref: TypeReference::Unresolved,
            };
        }

        Ok(expr)
    }

    fn parse_shift_expression(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_term()?;

        loop {
            let operator = if self.match_token(&Token::LeftShift) {
                BinaryOperator::LeftShift
            } else if self.match_token(&Token::RightShift) {
                BinaryOperator::RightShift
            } else {
                break;
            };

            let right = self.parse_term()?;
            let span = self.current_span();

            expr = Expression::BinaryOperation {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
                type_ref: TypeReference::Unresolved,
            };
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_factor()?;

        loop {
            let operator = if self.match_token(&Token::Plus) {
                BinaryOperator::Add
            } else if self.match_token(&Token::Minus) {
                BinaryOperator::Subtract
            } else {
                break;
            };

            let right = self.parse_factor()?;
            let span = self.current_span();

            expr = Expression::BinaryOperation {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
                type_ref: TypeReference::Unresolved,
            };
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_unary()?;

        loop {
            let operator = if self.match_token(&Token::Star) {
                BinaryOperator::Multiply
            } else if self.match_token(&Token::Slash) {
                BinaryOperator::Divide
            } else if self.match_token(&Token::Percent) {
                BinaryOperator::Modulo
            } else {
                break;
            };

            let right = self.parse_unary()?;
            let span = self.current_span();

            expr = Expression::BinaryOperation {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
                type_ref: TypeReference::Unresolved,
            };
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expression, String> {
        if self.match_token(&Token::Plus) {
            let operator = UnaryOperator::Positive;
            let operand = self.parse_unary()?;
            let span = self.current_span();

            Ok(Expression::UnaryOperation {
                operator,
                operand: Box::new(operand),
                span,
                type_ref: TypeReference::Unresolved,
            })
        } else if self.match_token(&Token::Minus) {
            let operator = UnaryOperator::Negative;
            let operand = self.parse_unary()?;
            let span = self.current_span();

            Ok(Expression::UnaryOperation {
                operator,
                operand: Box::new(operand),
                span,
                type_ref: TypeReference::Unresolved,
            })
        } else if self.match_token(&Token::Not) {
            let operator = UnaryOperator::Not;
            let operand = self.parse_unary()?;
            let span = self.current_span();

            Ok(Expression::UnaryOperation {
                operator,
                operand: Box::new(operand),
                span,
                type_ref: TypeReference::Unresolved,
            })
        } else {
            self.parse_call()
        }
    }

    fn parse_call(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.match_token(&Token::LeftParen) {
                // Function call
                expr = self.finish_call(expr)?;
            } else if self.match_token(&Token::Dot) {
                // Member access
                let member = if let Some(Token::Identifier(name)) = &self.current_token {
                    let result = name.clone();
                    self.advance(); // Consume identifier
                    result
                } else {
                    return Err(format!("Expected property name after '.', got {:?}", self.current_token));
                };

                let span = self.current_span();

                expr = Expression::MemberAccess {
                    object: Box::new(expr),
                    member,
                    span,
                    type_ref: TypeReference::Unresolved,
                };
            } else if self.match_token(&Token::LeftBracket) {
                // Index access
                let index = self.parse_expression()?;
                self.consume(&Token::RightBracket, "Expected ']' after index")?;

                let span = self.current_span();

                expr = Expression::IndexAccess {
                    array: Box::new(expr),
                    index: Box::new(index),
                    span,
                    type_ref: TypeReference::Unresolved,
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expression) -> Result<Expression, String> {
        let mut arguments = Vec::new();

        // Parse arguments
        if !self.check(&Token::RightParen) {
            loop {
                arguments.push(self.parse_expression()?);

                if !self.match_token(&Token::Comma) {
                    break;
                }
            }
        }

        self.consume(&Token::RightParen, "Expected ')' after arguments")?;

        let span = self.current_span();

        Ok(Expression::FunctionCall {
            function: Box::new(callee),
            arguments,
            span,
            type_ref: TypeReference::Unresolved,
        })
    }

    fn parse_primary(&mut self) -> Result<Expression, String> {
        match &self.current_token {
            Some(Token::NumberLiteral(value)) => {
                let result = *value;
                self.advance();
                Ok(Expression::Literal(Literal::Integer(result as i64)))
            },
            Some(Token::StringLiteral(value)) => {
                let result = value.clone();
                self.advance();
                Ok(Expression::Literal(Literal::String(result)))
            },
            Some(Token::Identifier(name)) => {
                let result = name.clone();
                self.advance();
                Ok(Expression::Identifier(result))
            },
            Some(Token::LeftParen) => {
                self.advance();
                let expr = self.parse_expression()?;
                self.consume(&Token::RightParen, "Expected ')' after expression")?;

                let span = self.current_span();

                Ok(Expression::Grouping {
                    expression: Box::new(expr),
                    span,
                    type_ref: TypeReference::Unresolved,
                })
            },
            _ => Err(format!("Expected expression, got {:?}", self.current_token)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_GRAMMAR: &str = r#"
// comments should be ignored
type MyStruct: struct {
    MyField: int,
}

type MyType: string

fun isEven(arg1: int) -> bool {
    return arg1 % 2 == 0
}

fun main() -> int {
    run(5, "6")

    var x: int = 0

    while x < 2 {
        x = x + 1
        printf("%d\n", isEven(x))
    }
    return 0
}

fun run(arg1: int, arg2: string) -> int {
    return loop(arg1, arg2)
}

fun loop(arg1: int, arg2: string) -> int {
    var j: int = 0

    for i in range(0, arg1) {
        j = i + j
        if isEven(i) {
            printf("%d\n", j)
        } else {
            printf("%d\n", i)
        }
    }

    return j
}
"#;

    #[test]
    fn test_parser() {
        let test_str = TEST_GRAMMAR;
        let mut lex = Lexer::new(test_str);
        let mut parser = Parser::new(&mut lex);
        print!("{:#?}", parser.parse_program());
    }
    
    #[test]
    fn test_fail() { // should fail ???
        let test_str = r#"
        type noBody: struct
        
        fun noClosure() {
        "#;
        let mut lex = Lexer::new(test_str);
        let mut parser = Parser::new(&mut lex);
        print!("{:#?}", parser.parse_program());
    }
}
