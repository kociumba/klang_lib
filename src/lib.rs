use log::error;
use std::ffi::{CString, c_char};
use std::ptr;
use thiserror::Error;

mod lexer;
mod parser;
mod preprocessor;
mod analysis;

#[derive(Debug, Error)]
pub enum KlangError {
    #[error("Failed to parse replacement directive: {0}")]
    ParseDirectiveError(String),
    #[error("UTF-8 conversion error: {0}")]
    Utf8Error(String),
    #[error("Unknown preprocessor directive encountered: {0}")]
    UnknownDirectiveError(String),
    #[error("Lexer encountered and error while tokenizing input: {0}")]
    LexerError(String),
}

thread_local! {
    static ERRORS: std::cell::RefCell<Vec<String>> = std::cell::RefCell::new(Vec::new());
}

#[allow(static_mut_refs)]
pub fn klang_error(err: &str) {
    ERRORS.with(|errors| errors.borrow_mut().push(err.to_string()));
    error!("{:?}", err);
}

/// classic get errors function, returns the last error emitted by klang lib, if there are no errors returns a null pointer
///
/// you have to free the returned string using `klang_free_string`
#[unsafe(no_mangle)]
#[allow(static_mut_refs)]
pub unsafe extern "C" fn klang_get_errors() -> *mut c_char {
    ERRORS.with(|errors| {
        let errors = errors.borrow();
        if errors.is_empty() {
            ptr::null_mut()
        } else {
            match errors.last() {
                Some(last_error) => match CString::new(last_error.clone()) {
                    Ok(cstring) => cstring.into_raw(),
                    Err(_) => ptr::null_mut(),
                },
                None => ptr::null_mut(),
            }
        }
    })
}

/// Use to free any strings allocated by klang lib
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn klang_free_string(ptr: *mut c_char) {
    if !ptr.is_null() {
        let _ = CString::from_raw(ptr);
    }
}
