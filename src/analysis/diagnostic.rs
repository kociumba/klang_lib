use std::ffi::{c_char, CString};
use crate::parser::ast::SourceSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Info,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
    pub span: SourceSpan,
    pub severity: DiagnosticSeverity,
    pub rule_id: String,
    pub related_info: Vec<DiagnosticRelatedInfo>, // optional
}

#[derive(Debug, Clone)]
pub struct DiagnosticRelatedInfo {
    pub message: String,
    pub span: SourceSpan,
}

#[derive(Debug, Clone)]
pub struct DiagnosticCollector {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticCollector {
    pub fn new() -> Self {
        Self { diagnostics: Vec::new() }
    }

    pub fn add(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn report_error(&mut self, rule_id: &str, message: String, span: SourceSpan) {
        self.add(Diagnostic {
            message,
            span,
            severity: DiagnosticSeverity::Error,
            rule_id: rule_id.to_string(),
            related_info: Vec::new(),
        });
    }

    pub fn report_warning(&mut self, rule_id: &str, message: String, span: SourceSpan) {
        self.add(Diagnostic {
            message,
            span,
            severity: DiagnosticSeverity::Warning,
            rule_id: rule_id.to_string(),
            related_info: Vec::new(),
        })
    }

    pub fn report_info(&mut self, rule_id: &str, message: String, span: SourceSpan) {
        self.add(Diagnostic {
            message,
            span,
            severity: DiagnosticSeverity::Info,
            rule_id: rule_id.to_string(),
            related_info: Vec::new(),
        })
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| d.severity == DiagnosticSeverity::Error)
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    // TODO: should be later wrapped in an extern C function for use with c ffi
    pub fn to_c_strings(&self) -> Vec<*mut c_char> {
        self.diagnostics.iter()
            .map(|d| {
                let msg = format!(
                    "{}:{}:{} [{}] {}",
                    d.span.start.line, d.span.start.column,
                    match d.severity {
                        DiagnosticSeverity::Error => "error",
                        DiagnosticSeverity::Warning => "warning",
                        DiagnosticSeverity::Info => "info",
                    },
                    d.rule_id, d.message
                );
                CString::new(msg).unwrap().into_raw()
            })
            .collect()
    }
}