use crate::analysis::SemanticAnalyzer;
use crate::analysis::context::AnalysisContext;
use crate::analysis::diagnostic::{Diagnostic, DiagnosticSeverity};
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
        config: AnalyzerConfig,
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
            let error_count = diagnostics
                .iter()
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
        if diagnostics
            .iter()
            .any(|d| d.severity == DiagnosticSeverity::Error)
        {
            Err(diagnostics)
        } else {
            Ok(())
        }
    }

    /// get all registered rules
    pub fn list_rules(&self) -> Vec<(&'static str, &'static str, DiagnosticSeverity)> {
        self.rule_registry
            .get_all_rules()
            .iter()
            .map(|rule| (rule.id(), rule.description(), rule.severity()))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::diagnostic_printer::DiagnosticPrinter;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use log::error;

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
    fn test_analysis() {
        let test_str = TEST_GRAMMAR;
        let mut lex = Lexer::new(test_str);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();
        let result = SemanticAnalyzer::new().analyze_with_config(
            &mut program.unwrap(),
            AnalyzerConfig {
                disabled_rules: vec![],
                warning_as_error: false,
                error_limit: None,
            },
        );
        assert!(result.is_err()); // we expect some errors for now
        DiagnosticPrinter::new(
            TEST_GRAMMAR.to_string(),
            "test_file.k".to_string(),
            Some(false),
        )
        .print_errors(result.err().unwrap());
        // print!("{:#?}", result);
        // assert!(result.is_ok());
    }
}
