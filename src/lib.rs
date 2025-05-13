#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[derive(Debug)]
pub struct OnigmoError {
    kind: OnigmoErrKind,
    message: String,
    span: Option<(usize, usize)>,
}

#[derive(Debug)]
pub enum OnigmoErrKind {
    InvalidPattern,
    SearchError,
    InvalidErrorMessage,
}

pub struct OnigmoRegex {
    raw: *mut re_pattern_buffer,
    pattern: String,
    error_info: OnigErrorInfo,
}

impl OnigmoRegex {
    pub fn new(pattern: String) -> Result<Self, OnigmoError> {
        let mut raw = std::ptr::null_mut();
        let pattern_start: *const u8 = pattern.as_ptr();
        let pattern_end = unsafe { pattern_start.add(pattern.len()) };
        let mut einfo = std::mem::MaybeUninit::uninit();
        let r = unsafe {
            onig_new(
                &mut raw as _,
                pattern_start,
                pattern_end,
                ONIG_OPTION_NONE,
                &OnigEncodingUTF_8 as _,
                &OnigSyntaxRuby as _,
                einfo.as_mut_ptr(),
            )
        };
        if r != ONIG_NORMAL as _ {
            let mut s = [0; ONIG_MAX_ERROR_MESSAGE_LEN as usize];
            let err_len =
                unsafe { onig_error_code_to_str(s.as_mut_ptr(), r as _, einfo.as_mut_ptr()) }
                    as usize;
            let err = std::str::from_utf8(&s[..err_len]).unwrap();
            let e_info = unsafe { einfo.assume_init() };

            let span = if e_info.par.is_null() {
                None
            } else {
                Some((
                    unsafe { e_info.par.offset_from(pattern.as_ptr()) } as usize,
                    unsafe { e_info.par_end.offset_from(pattern.as_ptr()) } as usize,
                ))
            };
            return Err(OnigmoError {
                kind: OnigmoErrKind::InvalidPattern,
                message: err.to_string(),
                span,
            });
        }
        Ok(Self {
            raw,
            pattern,
            error_info: unsafe { einfo.assume_init() },
        })
    }

    pub fn as_str(&self) -> &str {
        &self.pattern
    }

    fn captures(&mut self, heystack: &str) -> Result<Option<Vec<(usize, usize)>>, OnigmoError> {
        let hey_start = heystack.as_ptr();
        let hey_end = unsafe { hey_start.add(heystack.len()) };
        let range_start = hey_start;
        let range_end = hey_end;
        let region = unsafe { onig_region_new() };

        let r = unsafe {
            onig_search(
                self.raw,
                hey_start,
                hey_end,
                range_start,
                range_end,
                region,
                ONIG_OPTION_NONE,
            )
        };

        if r >= 0 {
            let region = unsafe { &*region };
            let num = region.num_regs as usize;
            let beg: *const usize = region.beg as _;
            let end: *const usize = region.end as _;
            let v = (0..num)
                .map(|i| unsafe { (*beg.add(i), *end.add(i)) })
                .collect();
            Ok(Some(v))
        } else if r == ONIG_MISMATCH as _ {
            Ok(None)
        } else {
            let mut s = [0; ONIG_MAX_ERROR_MESSAGE_LEN as usize];
            let err_len = unsafe { onig_error_code_to_str(s.as_mut_ptr(), r as _) } as usize;
            let message = match std::str::from_utf8(&s[..err_len]) {
                Ok(err) => err.to_string(),
                Err(err) => {
                    return Err(OnigmoError {
                        kind: OnigmoErrKind::InvalidErrorMessage,
                        message: format!("Error message is invalid UTF-8: {err}"),
                        span: None,
                    });
                }
            };
            Err(OnigmoError {
                kind: OnigmoErrKind::SearchError,
                message,
                span: None,
            })
        }
    }
}

#[test]
fn test() {
    let _ = unsafe { onig_init() };
    let mut onig = OnigmoRegex::new("(?<first>.)a(?<second>.)".to_string()).unwrap();
    assert_eq!(
        vec![(4, 7), (4, 5), (6, 7)],
        onig.captures("some accounts always taken into account")
            .unwrap()
            .unwrap()
    );
    assert_eq!(
        vec![(0, 3), (0, 1), (2, 3)],
        onig.captures("aaaabhddkwaaanuiuaabiubiuca")
            .unwrap()
            .unwrap()
    );
}
