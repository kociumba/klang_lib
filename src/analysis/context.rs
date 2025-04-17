use crate::analysis::diagnostic::DiagnosticCollector;
use crate::parser::ast::{Program, ScopeId, SourceSpan, Symbol, SymbolTable};
use std::collections::HashSet;

#[derive(Debug)]
pub struct AnalysisContext<'a> {
    pub diagnostics: DiagnosticCollector,
    pub symbol_table: SymbolTable,
    pub program: &'a mut Program,
    pub current_scope: ScopeId,
    pub current_function: Option<String>,
    pub disabled_rules: HashSet<String>,
}

impl<'a> AnalysisContext<'a> {
    pub fn new(program: &'a mut Program) -> Self {
        let mut symbol_table = SymbolTable::new();

        Self {
            diagnostics: DiagnosticCollector::new(),
            symbol_table: symbol_table.clone(),
            program,
            current_scope: symbol_table.global_scope(),
            current_function: None,
            disabled_rules: HashSet::new(),
        }
    }

    pub fn is_rule_enabled(&self, rule_id: &str) -> bool {
        !self.disabled_rules.contains(rule_id)
    }

    // Helper methods for common operations
    pub fn enter_scope(&mut self) -> ScopeId {
        let new_scope = self.symbol_table.create_scope(Some(self.current_scope));
        self.current_scope = new_scope;
        new_scope
    }

    pub fn exit_scope(&mut self) {
        if let Some(parent) = self.symbol_table.get_parent_scope(self.current_scope) {
            self.current_scope = parent;
        }
    }

    pub fn get_span_for_identifier(&mut self, name: &String) -> Option<SourceSpan> {
        let mut current_scope = Some(self.current_scope);
        while let Some(scope_id) = current_scope {
            if let Some(symbol) = self.symbol_table.lookup_symbol(scope_id, name) {
                return Some(match symbol {
                    Symbol::Variable(var) => var.span,
                    Symbol::Function(func) => func.span,
                    Symbol::Type(typ) => typ.span,
                });
            }
            current_scope = self.symbol_table.get_parent_scope(scope_id);
        }
        None
    }

    // TODO: this impl is unfinished
}
