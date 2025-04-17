use crate::analysis::context::AnalysisContext;
use crate::analysis::diagnostic::DiagnosticSeverity;
use crate::analysis::rule::SemanticRule;
use crate::parser::ast::AstNode;

// Rule to check for duplicate declarations
pub struct DuplicateDeclarationRule;

impl SemanticRule for DuplicateDeclarationRule {
    fn id(&self) -> &'static str {
        "duplicate-declaration"
    }

    fn description(&self) -> &'static str {
        "Checks for duplicate declarations in the same scope"
    }

    fn severity(&self) -> DiagnosticSeverity {
        DiagnosticSeverity::Error
    }

    #[allow(unused_variables)]
    fn check(&self, ctx: &mut AnalysisContext, node: &AstNode) -> Result<(), ()> {
        // This rule is now handled during symbol table construction,
        // but we could add more sophisticated checks here
        Ok(())
    }
}