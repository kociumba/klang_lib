mod replace;

use crate::preprocessor::replace::ReplaceDirective;
use crate::*;
use std::collections::HashMap;
use std::ffi::{CString, c_char, c_int};
use std::ptr;
use std::str;

/// Use to preprocess a klang file (expected as a c string `*char`) if the preprocessing fails the returned pointer will be null,
/// and the out_len will be 0
///
/// # Warning
/// Since rust takes ownership of the string you pass in, you don't have to free it, but you do have to free the returned string
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn klang_preprocess(
    input: *mut c_char,
    // len: c_int, // not needed since we are using a proper c_char
    out_len: *mut c_int,
) -> *mut c_char {
    if input.is_null() {
        klang_error("the passed string pointer is null");
        if !out_len.is_null() {
            *out_len = 0;
        }
        return ptr::null_mut();
    }

    // if len < 0 {
    //     klang_error("the string length is < 0");
    //     if !out_len.is_null() {
    //         *out_len = 0;
    //     }
    //     return ptr::null_mut();
    // }

    // let length = len as usize;
    // let byte_slice = slice::from_raw_parts(input, length);
    //
    // match str::from_utf8(byte_slice) {
    match CString::from_raw(input).to_str() {
        Ok(s) => match klang_internal_preprocess(s) {
            Ok(result_string) => match CString::new(result_string) {
                Ok(cstring) => {
                    if !out_len.is_null() {
                        *out_len = cstring.as_bytes().len() as c_int;
                    }
                    cstring.into_raw()
                }
                Err(_) => {
                    klang_error("failed to create CString from processed string");
                    if !out_len.is_null() {
                        *out_len = 0;
                    }
                    ptr::null_mut()
                }
            },
            Err(err) => {
                klang_error(&format!("klang_internal_preprocess failed: {:?}", err));
                if !out_len.is_null() {
                    *out_len = 0;
                }
                ptr::null_mut()
            }
        },
        Err(err) => {
            klang_error(&format!("failed to decode UTF-8: {:?}", err));
            if !out_len.is_null() {
                *out_len = 0;
            }
            ptr::null_mut()
        }
    }
}

pub struct Preprocessor {
    directives: HashMap<String, Box<dyn DirectiveHandler>>,
    state: PreprocessorState,
}

pub struct PreprocessorState {
    replacements: HashMap<String, String>,
}

impl Preprocessor {
    pub fn new() -> Self {
        let mut p = Preprocessor {
            directives: HashMap::new(),
            state: PreprocessorState {
                replacements: HashMap::new(),
            },
        };
        p.register_directive("replace", Box::new(ReplaceDirective));
        p
    }

    pub fn register_directive(&mut self, name: &str, handler: Box<dyn DirectiveHandler>) {
        self.directives.insert(name.to_string(), handler);
    }

    pub fn process(&mut self, input: &str) -> Result<String, KlangError> {
        let mut content_lines = Vec::new();
        for line in input.lines() {
            let trimmed_line = line.trim();
            if trimmed_line.starts_with("@") {
                let directive_line = trimmed_line.strip_prefix("@").unwrap();
                let (directive_name, rest) = directive_line
                    .split_once(' ')
                    .unwrap_or((directive_line, ""));
                if let Some(handler) = self.directives.get(directive_name) {
                    handler.process(rest, &mut self.state)?;
                } else {
                    return Err(KlangError::UnknownDirectiveError(
                        directive_name.to_string(),
                    ));
                }
            } else {
                content_lines.push(line);
            }
        }
        let mut output = content_lines.join("\n");
        for (original, replacement) in &self.state.replacements {
            output = output.replace(replacement, original);
        }
        Ok(output)
    }
}

pub trait DirectiveHandler {
    fn process(&self, line: &str, state: &mut PreprocessorState) -> Result<(), KlangError>;
}

/// Uses the new modular `Preprocessor`
pub fn klang_internal_preprocess(input: &str) -> Result<String, KlangError> {
    let mut preprocessor = Preprocessor::new();
    preprocessor.process(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_GRAMMAR: &str = r#"
// @replaces a keyword
@replace for -> gabagool

type MyStruct: struct {
    MyField: int,
}

type MyType: string

fun isEven(arg1: int) -> bool {
    return arg1 % 2 == 0
}

//if return type is blank, use void in c
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
    // or j := 0 should not require var and should infer type
    var j: int = 0

    // replace at the top replaces for with gabagool
    // while loops can also be supported, but not required
    gabagool i in range(0, arg1) {
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
    fn preprocessor() {
        let replacements = klang_internal_preprocess(TEST_GRAMMAR);
        println!("{:?}", replacements)
    }

    #[test]
    fn test_preprocessor() {
        let result = klang_internal_preprocess(TEST_GRAMMAR);
        assert!(result.is_ok());
        let processed = result.unwrap();
        assert!(processed.contains("for i in range")); // properly replaced
        assert!(!processed.contains("@replace for -> gabagool")); // the directive is removed
    }

    #[test]
    fn test_error_handling() {
        let invalid_input = "@replace missing_arrow gabagool";
        let result = klang_internal_preprocess(invalid_input);
        assert!(result.is_err());
        match result {
            Err(KlangError::ParseDirectiveError(_)) => (), // Expected
            _ => panic!("Expected ParseDirectiveError"),
        }
    }
}
