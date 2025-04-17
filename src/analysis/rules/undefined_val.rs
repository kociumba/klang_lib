use crate::analysis::context::AnalysisContext;
use crate::analysis::diagnostic::DiagnosticSeverity;
use crate::analysis::rule::SemanticRule;
use crate::parser::ast::{AstNode, Expression, SourcePosition, SourceSpan};

// Rule to check for undefined variables
pub struct UndefinedVariableRule;

impl SemanticRule for UndefinedVariableRule {
    fn id(&self) -> &'static str {
        "undefined-variable"
    }

    fn description(&self) -> &'static str {
        "Checks for usage of undefined variables"
    }

    fn severity(&self) -> DiagnosticSeverity {
        DiagnosticSeverity::Error
    }

    fn check(&self, ctx: &mut AnalysisContext, node: &AstNode) -> Result<(), ()> {
        if let AstNode::Expression(Expression::Identifier(name)) = node {
            if ctx.symbol_table.lookup_symbol(ctx.current_scope, name).is_none() {
                // Lookup failed, variable is undefined
                // Find the span for this identifier
                let span = ctx.get_span_for_identifier(name).unwrap_or_else(|| SourceSpan {
                    start: SourcePosition { line: 0, column: 0 },
                    end: SourcePosition { line: 0, column: 0 },
                });

                ctx.diagnostics.report_error(
                    self.id(),
                    format!("Undefined variable '{}'", name),
                    span
                );
            }
        }

        Ok(())
    }
}