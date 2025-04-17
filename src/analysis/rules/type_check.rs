use crate::analysis::context::AnalysisContext;
use crate::analysis::diagnostic::DiagnosticSeverity;
use crate::analysis::rule::SemanticRule;
use crate::parser::ast::{AstNode, BinaryOperator, Expression, LValue, Literal, SourcePosition, SourceSpan, Statement, Symbol, TypeReference};

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
                        assignment.span
                    );
                }
            }

            AstNode::Expression(Expression::BinaryOperation { left, operator, right, span, .. }) => {
                let left_type = Self::get_expression_type(ctx, left);
                let right_type = Self::get_expression_type(ctx, right);

                match operator {
                    BinaryOperator::Add | BinaryOperator::Subtract | BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Modulo => {
                        if !Self::are_numeric_types(&left_type) || !Self::are_numeric_types(&right_type) {
                            ctx.diagnostics.report_error(
                                self.id(),
                                format!(
                                    "Operator '{}' requires numeric operands, got '{}' and '{}'",
                                    Self::operator_to_string(operator),
                                    Self::type_to_string(&left_type),
                                    Self::type_to_string(&right_type)
                                ),
                                *span
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
                if let Some(Symbol::Variable(var)) = ctx.symbol_table.lookup_symbol(ctx.current_scope, name) {
                    var.type_ref.clone()
                } else {
                    TypeReference::Unresolved
                }
            }
            // Handle other LValue types...
            _ => TypeReference::Unresolved,
        }
    }

    pub(crate) fn get_expression_type(ctx: &mut AnalysisContext, expr: &Expression) -> TypeReference {
        match expr {
            Expression::Literal(lit) => match lit {
                Literal::Integer(_) => TypeReference::Int64,
                Literal::Float(_) => TypeReference::Float32,
                Literal::String(_) => TypeReference::String,
                Literal::Boolean(_) => TypeReference::Bool,
                Literal::Null => TypeReference::Void,
            },
            Expression::Identifier(name) => {
                if let Some(Symbol::Variable(var)) = ctx.symbol_table.lookup_symbol(ctx.current_scope, name) {
                    var.type_ref.clone()
                } else {
                    ctx.diagnostics.report_error(
                        "undefined-variable",
                        format!("Undefined variable '{}'", name),
                        SourceSpan {
                            start: SourcePosition { line: 0, column: 0 },
                            end: SourcePosition { line: 0, column: 0 }
                        },
                    );
                    TypeReference::Unresolved
                }
            },
            Expression::BinaryOperation { left, operator, right, .. } => {
                let left_type = Self::get_expression_type(ctx, left);
                let right_type = Self::get_expression_type(ctx, right);
                match operator {
                    BinaryOperator::Add | BinaryOperator::Subtract | BinaryOperator::Multiply | BinaryOperator::Divide => {
                        if left_type == right_type && Self::are_numeric_types(&left_type) {
                            left_type
                        } else {
                            TypeReference::Unresolved
                        }
                    }
                    BinaryOperator::Equal | BinaryOperator::NotEqual => TypeReference::Bool,
                    _ => TypeReference::Unresolved,
                }
            },
            Expression::FunctionCall { function, arguments, .. } => {
                // TODO: resolve function types
                TypeReference::Unresolved
            },
            _ => TypeReference::Unresolved,
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
            TypeReference::Int32 | TypeReference::Int64 | TypeReference::Float32 | TypeReference::Float64
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
            TypeReference::Function { parameters, return_type } => {
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