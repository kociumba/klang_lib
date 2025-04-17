use crate::analysis::context::AnalysisContext;
use crate::analysis::diagnostic::DiagnosticSeverity;
use crate::analysis::rule::SemanticRule;
use crate::analysis::rules::type_check::TypeCheckRule;
use crate::parser::ast::{AstNode, Statement, Symbol, TypeReference};

pub struct ReturnTypeCheckRule;

impl SemanticRule for ReturnTypeCheckRule {
    fn id(&self) -> &'static str { "return-type-check" }
    fn description(&self) -> &'static str { "Checks return statement types match function signature" }
    fn severity(&self) -> DiagnosticSeverity { DiagnosticSeverity::Error }

    fn check(&self, ctx: &mut AnalysisContext, node: &AstNode) -> Result<(), ()> {
        let AstNode::Statement(Statement::ReturnStatement(ret_stmt)) = node else {
            return Ok(());
        };

        let Some(current_func) = &ctx.current_function else {
            return Ok(());
        };

        let Some(Symbol::Function(func)) = ctx.symbol_table.lookup_symbol(ctx.current_scope, current_func) else {
            return Ok(());
        };

        let expected_type = func.return_type.clone();

        if let Some(value) = &ret_stmt.value {
            let actual = TypeCheckRule::get_expression_type(ctx, value);
            if !TypeCheckRule::are_types_compatible(&expected_type, &actual) {
                ctx.diagnostics.report_error(
                    self.id(),
                    format!("Return type mismatch: expected '{}', got '{}'",
                            TypeCheckRule::type_to_string(&expected_type),
                            TypeCheckRule::type_to_string(&actual)),
                    ret_stmt.span,
                );
            }
        } else if expected_type != TypeReference::Void {
            ctx.diagnostics.report_error(
                self.id(),
                "Missing return value in non-void function".to_string(),
                ret_stmt.span,
            );
        }

        Ok(())
    }
}