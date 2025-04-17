use crate::analysis::context::AnalysisContext;
use crate::analysis::diagnostic::DiagnosticSeverity;
use crate::analysis::rule::SemanticRule;
use crate::parser::ast::{AstNode, TypeReference};

pub struct TypeResolutionRule;

impl SemanticRule for TypeResolutionRule {
    fn id(&self) -> &'static str { "type-resolution" }
    fn description(&self) -> &'static str { "Ensures all type references are defined" }
    fn severity(&self) -> DiagnosticSeverity { DiagnosticSeverity::Error }

    fn check(&self, ctx: &mut AnalysisContext, node: &AstNode) -> Result<(), ()> {
        match node {
            AstNode::VariableDeclaration(var_decl) => {
                if let TypeReference::Named(name) = &var_decl.type_ref {
                    if ctx.symbol_table.lookup_symbol(ctx.current_scope, name).is_none() {
                        ctx.diagnostics.report_error(
                            self.id(),
                            format!("Unresolved type '{}'", name),
                            var_decl.span,
                        );
                    }
                }
            },
            AstNode::FunctionDeclaration(fun_decl) => {
                if let TypeReference::Named(name) = &fun_decl.return_type {
                    if ctx.symbol_table.lookup_symbol(ctx.current_scope, name).is_none() {
                        ctx.diagnostics.report_error(
                            self.id(),
                            format!("Unresolved return type '{}'", name),
                            fun_decl.span,
                        );
                    }
                }
                for param in &fun_decl.parameters {
                    if let TypeReference::Named(name) = &param.type_ref {
                        if ctx.symbol_table.lookup_symbol(ctx.current_scope, name).is_none() {
                            ctx.diagnostics.report_error(
                                self.id(),
                                format!("Unresolved parameter type '{}'", name),
                                param.span,
                            );
                        }
                    }
                }
            },
            _ => {}
        }
        Ok(())
    }
}