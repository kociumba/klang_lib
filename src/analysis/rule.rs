use crate::analysis::context::AnalysisContext;
use crate::analysis::diagnostic::DiagnosticSeverity;
use crate::parser::ast::AstNode;

pub trait SemanticRule {
    // Unique identifier for this rule
    fn id(&self) -> &'static str;

    // Short description of what this rule checks
    fn description(&self) -> &'static str;

    // Severity level of violations (error, warning, info)
    fn severity(&self) -> DiagnosticSeverity;

    // Apply the rule to a specific node type in the AST
    fn check(&self, ctx: &mut AnalysisContext, node: &AstNode) -> Result<(), ()>;

    // Optional: whether this rule is enabled by default
    fn enabled_by_default(&self) -> bool {
        true
    }
}