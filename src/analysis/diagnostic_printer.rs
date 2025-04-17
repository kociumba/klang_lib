use crate::analysis::diagnostic::Diagnostic;

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
        todo!("implement the error message creation")
    }

    pub fn print_errors(&mut self, diagnostics: Vec<Diagnostic>) {
        print!("{}", self.sprint_errors(diagnostics))
    }
}