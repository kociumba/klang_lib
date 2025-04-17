use crate::analysis::context::AnalysisContext;
use crate::analysis::diagnostic::{Diagnostic, DiagnosticSeverity};
use crate::analysis::SemanticAnalyzer;
use crate::parser::ast::{Program, SourcePosition, SourceSpan};

pub struct AnalyzerConfig {
    pub disabled_rules: Vec<String>,
    pub warning_as_error: bool,
    pub error_limit: Option<usize>,
}

impl Default for AnalyzerConfig {
    fn default() -> Self {
        Self {
            disabled_rules: Vec::new(),
            warning_as_error: false,
            error_limit: None,
        }
    }
}

impl SemanticAnalyzer {
    pub fn analyze_with_config(
        &self,
        program: &mut Program,
        config: AnalyzerConfig
    ) -> Result<(), Vec<Diagnostic>> {
        let mut ctx = AnalysisContext::new(program);

        // Apply configuration
        for rule_id in &config.disabled_rules {
            ctx.disabled_rules.insert(rule_id.clone());
        }

        // Run analysis
        self.collect_declarations(&mut ctx);
        self.apply_rules(&mut ctx);

        // Process diagnostics based on config
        let mut diagnostics: Vec<Diagnostic> = ctx.diagnostics.diagnostics().to_vec();

        if config.warning_as_error {
            for diag in &mut diagnostics {
                if diag.severity == DiagnosticSeverity::Warning {
                    diag.severity = DiagnosticSeverity::Error;
                }
            }
        }

        if let Some(limit) = config.error_limit {
            let error_count = diagnostics.iter()
                .filter(|d| d.severity == DiagnosticSeverity::Error)
                .count();

            if error_count > limit {
                diagnostics.push(Diagnostic {
                    message: format!("Too many errors ({}), stopping analysis", error_count),
                    span: SourceSpan {
                        start: SourcePosition { line: 0, column: 0 },
                        end: SourcePosition { line: 0, column: 0 },
                    },
                    severity: DiagnosticSeverity::Info,
                    rule_id: "error-limit".to_string(),
                    related_info: Vec::new(),
                });

                return Err(diagnostics);
            }
        }

        // Return results
        if diagnostics.iter().any(|d| d.severity == DiagnosticSeverity::Error) {
            Err(diagnostics)
        } else {
            Ok(())
        }
    }

    // Method to get all available rules
    pub fn list_rules(&self) -> Vec<(&'static str, &'static str, DiagnosticSeverity)> {
        self.rule_registry.get_all_rules()
            .iter()
            .map(|rule| (rule.id(), rule.description(), rule.severity()))
            .collect()
    }
}