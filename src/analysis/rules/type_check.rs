use crate::analysis::context::AnalysisContext;
use crate::analysis::diagnostic::DiagnosticSeverity;
use crate::analysis::rule::SemanticRule;
use crate::parser::ast::{
    AstNode, BinaryOperator, Expression, LValue, Literal, SourcePosition, SourceSpan, Statement,
    Symbol, TypeReference, UnaryOperator,
};

// Rule to check types in assignments
pub struct TypeCheckRule;

impl SemanticRule for TypeCheckRule {
    fn id(&self) -> &'static str {
        "type-check"
    }

    fn description(&self) -> &'static str {
        "Checks for type compatibility in assignments and expressions"
    }

    fn severity(&self) -> DiagnosticSeverity {
        DiagnosticSeverity::Error
    }

    fn check(&self, ctx: &mut AnalysisContext, node: &AstNode) -> Result<(), ()> {
        match node {
            AstNode::Statement(Statement::Assignment(assignment)) => {
                let target_type = Self::get_lvalue_type(ctx, &assignment.target);
                let value_type = Self::get_expression_type(ctx, &assignment.value);

                if !Self::are_types_compatible(&target_type, &value_type) {
                    ctx.diagnostics.report_error(
                        self.id(),
                        format!(
                            "Type mismatch: cannot assign '{}' to '{}'",
                            Self::type_to_string(&value_type),
                            Self::type_to_string(&target_type)
                        ),
                        assignment.span,
                    );
                }
            }

            AstNode::Expression(Expression::BinaryOperation {
                left,
                operator,
                right,
                span,
                ..
            }) => {
                let left_type = Self::get_expression_type(ctx, left);
                let right_type = Self::get_expression_type(ctx, right);

                match operator {
                    BinaryOperator::Add
                    | BinaryOperator::Subtract
                    | BinaryOperator::Multiply
                    | BinaryOperator::Divide
                    | BinaryOperator::Modulo => {
                        if !Self::are_numeric_types(&left_type)
                            || !Self::are_numeric_types(&right_type)
                        {
                            ctx.diagnostics.report_error(
                                self.id(),
                                format!(
                                    "Operator '{}' requires numeric operands, got '{}' and '{}'",
                                    Self::operator_to_string(operator),
                                    Self::type_to_string(&left_type),
                                    Self::type_to_string(&right_type)
                                ),
                                *span,
                            );
                        }
                    }
                    // Check other operator types...
                    _ => {}
                }
            }

            // Handle other node types...
            _ => {}
        }

        Ok(())
    }
}

impl TypeCheckRule {
    fn get_lvalue_type(ctx: &mut AnalysisContext, lvalue: &LValue) -> TypeReference {
        match lvalue {
            LValue::Identifier(name) => {
                if let Some(Symbol::Variable(var)) =
                    ctx.symbol_table.lookup_symbol(ctx.current_scope, name)
                {
                    var.type_ref.clone()
                } else {
                    TypeReference::Unresolved
                }
            }
            // Handle other LValue types...
            _ => TypeReference::Unresolved,
        }
    }

    pub(crate) fn get_expression_type(
        ctx: &mut AnalysisContext,
        expr: &Expression,
    ) -> TypeReference {
        match expr {
            Expression::Literal(lit) => match lit {
                Literal::Integer(_) => TypeReference::Int64,
                Literal::Float(_) => TypeReference::Float32,
                Literal::String(_) => TypeReference::String,
                Literal::Boolean(_) => TypeReference::Bool,
                Literal::Null => TypeReference::Void,
            },
            Expression::Identifier(name) => {
                if let Some(Symbol::Variable(var)) =
                    ctx.symbol_table.lookup_symbol(ctx.current_scope, name)
                {
                    var.type_ref.clone()
                } else {
                    let span = ctx.get_span_for_identifier(name).unwrap_or(SourceSpan {
                        start: SourcePosition { line: 0, column: 0 },
                        end: SourcePosition { line: 0, column: 0 },
                    });
                    ctx.diagnostics.report_error(
                        "undefined-variable",
                        format!("Undefined variable '{}'", name),
                        span,
                    );
                    TypeReference::Unresolved
                }
            }
            Expression::BinaryOperation {
                left,
                operator,
                right,
                ..
            } => {
                let left_type = Self::get_expression_type(ctx, left);
                let right_type = Self::get_expression_type(ctx, right);
                match operator {
                    BinaryOperator::Add
                    | BinaryOperator::Subtract
                    | BinaryOperator::Multiply
                    | BinaryOperator::Divide => {
                        if left_type == right_type && Self::are_numeric_types(&left_type) {
                            left_type
                        } else {
                            TypeReference::Unresolved
                        }
                    }
                    BinaryOperator::Equal
                    | BinaryOperator::NotEqual
                    | BinaryOperator::LessThan
                    | BinaryOperator::LessThanOrEqual
                    | BinaryOperator::GreaterThan
                    | BinaryOperator::GreaterThanOrEqual => TypeReference::Bool,
                    BinaryOperator::Modulo => {
                        if left_type == TypeReference::Int64 && right_type == TypeReference::Int64 {
                            TypeReference::Int64
                        } else {
                            TypeReference::Unresolved
                        }
                    }
                    _ => TypeReference::Unresolved,
                }
            }
            Expression::UnaryOperation {
                operator, operand, ..
            } => {
                let operand_type = Self::get_expression_type(ctx, operand);
                match operator {
                    UnaryOperator::Negative => {
                        if Self::are_numeric_types(&operand_type) {
                            operand_type
                        } else {
                            TypeReference::Unresolved
                        }
                    }
                    UnaryOperator::Not => TypeReference::Bool,
                    _ => operand_type,
                }
            }
            Expression::FunctionCall {
                function,
                arguments,
                ..
            } => {
                if let Expression::Identifier(func_name) = &**function {
                    if let Some(Symbol::Function(func)) = ctx
                        .symbol_table
                        .clone()
                        .lookup_symbol(ctx.current_scope, func_name)
                    {
                        // Check argument types match parameter types
                        if func.parameters.len() != arguments.len() {
                            let span =
                                ctx.get_span_for_identifier(func_name)
                                    .unwrap_or(SourceSpan {
                                        start: SourcePosition { line: 0, column: 0 },
                                        end: SourcePosition { line: 0, column: 0 },
                                    });
                            ctx.diagnostics.report_error(
                                "type-check",
                                format!(
                                    "Function '{}' expects {} arguments, got {}",
                                    func_name,
                                    func.parameters.len(),
                                    arguments.len()
                                ),
                                span,
                            );
                            return TypeReference::Unresolved;
                        }
                        for (param, arg) in func.parameters.iter().zip(arguments.iter()) {
                            let arg_type = Self::get_expression_type(ctx, arg);
                            if !Self::are_types_compatible(&param.type_ref, &arg_type) {
                                ctx.diagnostics.report_error(
                                    "type-check",
                                    format!(
                                        "Argument type mismatch: expected '{}', got '{}'",
                                        Self::type_to_string(&param.type_ref),
                                        Self::type_to_string(&arg_type)
                                    ),
                                    SourceSpan {
                                        // can not really get the real spans right now
                                        start: SourcePosition { line: 0, column: 0 },
                                        end: SourcePosition { line: 0, column: 0 },
                                    },
                                );
                            }
                        }
                        return func.return_type.clone();
                    }
                }
                TypeReference::Unresolved
            }
            Expression::MemberAccess { object, member, .. } => {
                // Placeholder: requires struct type resolution
                TypeReference::Unresolved
            }
            Expression::IndexAccess { array, .. } => {
                // Placeholder: requires array type resolution
                TypeReference::Unresolved
            }
            Expression::Grouping { expression, .. } => Self::get_expression_type(ctx, expression),
        }
    }

    pub(crate) fn are_types_compatible(t1: &TypeReference, t2: &TypeReference) -> bool {
        match (t1, t2) {
            (TypeReference::Unresolved, _) | (_, TypeReference::Unresolved) => false,
            (t1, t2) if t1 == t2 => true,
            // TODO: when codegen is implemented with type coercion, handle coerced type compatibility
            _ => false,
        }
    }

    fn are_numeric_types(typ: &TypeReference) -> bool {
        matches!(
            typ,
            TypeReference::Int32
                | TypeReference::Int64
                | TypeReference::Float32
                | TypeReference::Float64
        )
    }

    pub(crate) fn type_to_string(typ: &TypeReference) -> String {
        match typ {
            TypeReference::Int32 => "int32".to_string(),
            TypeReference::Int64 => "int64".to_string(),
            TypeReference::Float32 => "float32".to_string(),
            TypeReference::Float64 => "float64".to_string(),
            TypeReference::Bool => "bool".to_string(),
            TypeReference::String => "string".to_string(),
            TypeReference::Void => "void".to_string(),
            TypeReference::Named(name) => name.clone(),
            TypeReference::Function {
                parameters,
                return_type,
            } => {
                let params = parameters
                    .iter()
                    .map(|p| Self::type_to_string(p))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("fn({}) -> {}", params, Self::type_to_string(return_type))
            }
            TypeReference::Unresolved => "<unresolved>".to_string(),
        }
    }

    fn operator_to_string(op: &BinaryOperator) -> String {
        match op {
            BinaryOperator::Add => "+".to_string(),
            BinaryOperator::Subtract => "-".to_string(),
            BinaryOperator::Multiply => "*".to_string(),
            BinaryOperator::Divide => "/".to_string(),
            BinaryOperator::Modulo => "%".to_string(),
            // Other operators...
            _ => format!("{:?}", op),
        }
    }
}
