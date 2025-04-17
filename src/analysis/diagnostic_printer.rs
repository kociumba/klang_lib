use crate::analysis::diagnostic::{Diagnostic, DiagnosticSeverity};

pub struct DiagnosticPrinter {
    pub use_colors: bool,
    pub source_code: String,
    pub file_name: String,
}

impl DiagnosticPrinter {
    pub fn new(source_code: String, file_name: String, use_colors: Option<bool>) -> Self {
        Self {
            use_colors: use_colors.is_some(),
            source_code,
            file_name,
        }
    }

    pub fn sprint_errors(&mut self, diagnostics: Vec<Diagnostic>) -> String {
        let mut output = String::new();
        for diag in diagnostics {
            let severity = match diag.severity {
                DiagnosticSeverity::Error => "error",
                DiagnosticSeverity::Warning => "warning",
                DiagnosticSeverity::Info => "info",
            };
            let location = format!(
                "{}:{}:{}",
                self.file_name,
                diag.span.start.line,
                diag.span.start.column
            );
            let message = format!("{} [{}] {}\n", severity, diag.rule_id, diag.message);
            output.push_str(&format!("{}: {}", location, message));

            // Extract and display the source line
            let lines: Vec<&str> = self.source_code.lines().collect();
            if let Some(line) = lines.get(diag.span.start.line.checked_sub(1).unwrap_or(diag.span.start.line)) {
                output.push_str(&format!("    {}\n", line));
                // Add a pointer to the error position
                let mut pointer = String::from("    ");
                for _ in 1..diag.span.start.column {
                    pointer.push(' ');
                }
                pointer.push('^');
                output.push_str(&format!("{}\n", pointer));
            }

            // Handle related info
            for related in &diag.related_info {
                let related_loc = format!(
                    "{}:{}:{}",
                    self.file_name,
                    related.span.start.line,
                    related.span.start.column
                );
                output.push_str(&format!("  related: {}: {}\n", related_loc, related.message));
            }
        }
        output
    }

    pub fn print_errors(&mut self, diagnostics: Vec<Diagnostic>) {
        print!("{}", self.sprint_errors(diagnostics))
    }
}