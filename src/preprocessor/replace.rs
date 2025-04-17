use crate::KlangError;
use crate::preprocessor::{DirectiveHandler, PreprocessorState};

/// Handler for the @replace directive.
pub struct ReplaceDirective;

impl DirectiveHandler for ReplaceDirective {
    fn process(&self, line: &str, state: &mut PreprocessorState) -> Result<(), KlangError> {
        let mut parts = line.split("->");
        let original = parts
            .next()
            .ok_or_else(|| KlangError::ParseDirectiveError("Missing original token".to_string()))?
            .trim();
        let replacement = parts
            .next()
            .ok_or_else(|| {
                KlangError::ParseDirectiveError("Missing replacement token".to_string())
            })?
            .trim();
        state
            .replacements
            .insert(original.to_string(), replacement.to_string());
        Ok(())
    }
}
