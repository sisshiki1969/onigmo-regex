use crate::*;

#[derive(Debug)]
pub struct OnigmoError {
    message: String,
    span: Option<(usize, usize)>,
    code: Option<isize>,
}

impl std::fmt::Display for OnigmoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl OnigmoError {
    pub(crate) fn new_with_span(message: impl Into<String>, span: Option<(usize, usize)>) -> Self {
        let message = message.into();
        Self {
            message,
            span,
            code: None,
        }
    }

    pub(crate) fn new(message: impl Into<String>) -> Self {
        Self::new_with_span(message, None)
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn span(&self) -> Option<(usize, usize)> {
        self.span
    }

    /// The Onigmo error code this came from, for the errors that carry
    /// one (everything built by [`OnigmoError::from_code`]).
    pub fn code(&self) -> Option<isize> {
        self.code
    }

    /// Whether the match was cut short by the deadline set with
    /// [`crate::set_deadline`], rather than failing for a reason in the
    /// pattern or the subject. Callers that expose a timeout of their
    /// own — Ruby's `Regexp.timeout` and its `Regexp::TimeoutError` —
    /// need to tell this apart from a real error.
    pub fn is_timeout(&self) -> bool {
        self.code == Some(crate::ONIGERR_TIMEOUT as isize)
    }

    pub(crate) fn from_code(code: isize) -> Self {
        let mut s = [0; ONIG_MAX_ERROR_MESSAGE_LEN as usize];
        let err_len = unsafe { onig_error_code_to_str(s.as_mut_ptr(), code as _) } as usize;
        let message = match std::str::from_utf8(&s[..err_len]) {
            Ok(err) => err.to_string(),
            Err(err) => {
                return OnigmoError {
                    message: format!("Error message is invalid UTF-8: {err}"),
                    span: None,
                    code: Some(code),
                };
            }
        };
        Self {
            message,
            span: None,
            code: Some(code),
        }
    }
}
