#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use num_enum::TryFromPrimitive;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[derive(Debug, Eq, PartialEq, TryFromPrimitive)]
#[repr(u32)]
pub enum OnigmoOption {
    None = ONIG_OPTION_NONE,
    IgnoreCase = ONIG_OPTION_IGNORECASE,
    FreeFormat = ONIG_OPTION_EXTEND,
    DotAll = ONIG_OPTION_DOTALL,
}

impl std::ops::BitOr for OnigmoOption {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self::try_from((self as u32) | (rhs as u32)).unwrap()
    }
}

#[derive(Debug)]
pub struct OnigmoError {
    kind: OnigmoErrKind,
    message: String,
    span: Option<(usize, usize)>,
}

#[derive(Debug)]
pub enum OnigmoErrKind {
    InvalidPattern = -1,
    SearchError = -2,
    InvalidErrorMessage = -3,
    CallbackError = -4,
}

pub struct OnigmoRegex {
    raw: *mut re_pattern_buffer,
    pattern: String,
    error_info: OnigErrorInfo,
}

impl OnigmoRegex {
    pub fn new(pattern: String) -> Result<Self, OnigmoError> {
        Self::new_with_option(pattern, OnigmoOption::None)
    }

    pub fn new_with_option(pattern: String, option: OnigmoOption) -> Result<Self, OnigmoError> {
        let mut raw = std::ptr::null_mut();
        let pattern_start: *const u8 = pattern.as_ptr();
        let pattern_end = unsafe { pattern_start.add(pattern.len()) };
        let mut einfo = std::mem::MaybeUninit::uninit();
        let r = unsafe {
            onig_new(
                &mut raw as _,
                pattern_start,
                pattern_end,
                option as u32,
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

    pub fn captures(&mut self, heystack: &str) -> Result<Option<Vec<(usize, usize)>>, OnigmoError> {
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

    pub fn captures_str<'h>(
        &mut self,
        heystack: &'h str,
    ) -> Result<Option<Vec<&'h str>>, OnigmoError> {
        let captures = self.captures(heystack)?;
        if let Some(captures) = captures {
            let result = captures
                .into_iter()
                .map(|(start, end)| &heystack[start..end])
                .collect();
            Ok(Some(result))
        } else {
            Ok(None)
        }
    }

    pub fn names(&self) -> Result<Vec<(String, usize)>, OnigmoError> {
        let mut names: Vec<(String, usize)> = vec![];
        let res = unsafe {
            onig_foreach_name(
                self.raw,
                Some(names_callback),
                &mut names as *mut Vec<_> as _,
            )
        };
        if res != 0 {
            return Err(OnigmoError {
                kind: OnigmoErrKind::CallbackError,
                message: "Failed to get names".to_string(),
                span: None,
            });
        }
        Ok(names)
    }
}

extern "C" fn names_callback(
    name_start: *const u8,
    name_end: *const u8,
    group: i32,
    id: *mut i32,
    _buf: *mut re_pattern_buffer,
    f: *mut std::ffi::c_void,
) -> i32 {
    let name = unsafe {
        std::slice::from_raw_parts(name_start, name_end.offset_from_unsigned(name_start))
    };
    let id = unsafe { *id } as usize;
    let name = match std::str::from_utf8(name) {
        Ok(name) => name,
        Err(_) => return OnigmoErrKind::CallbackError as i32,
    };
    let v: &mut Vec<(String, usize)> = unsafe { &mut *(f as *mut Vec<(String, usize)>) };
    v.push((name.to_string(), id));
    0
}

mod test {
    use super::*;

    fn assert_regex(expected: Vec<&str>, heystack: &str, pattern: &str) {
        let mut onig = OnigmoRegex::new(pattern.to_string()).unwrap();
        assert_eq!(expected, onig.captures_str(heystack).unwrap().unwrap());
    }

    fn assert_regex_nomatch(heystack: &str, pattern: &str) {
        let mut onig = OnigmoRegex::new(pattern.to_string()).unwrap();
        assert_eq!(None, onig.captures_str(heystack).unwrap());
    }

    fn assert_regex_with_mode(
        expected: Vec<&str>,
        heystack: &str,
        pattern: &str,
        option: OnigmoOption,
    ) {
        let mut onig = OnigmoRegex::new_with_option(pattern.to_string(), option).unwrap();
        assert_eq!(expected, onig.captures_str(heystack).unwrap().unwrap());
    }

    #[test]
    fn test() {
        let _ = unsafe { onig_init() };
        // /(.)(.)\k<-2>\k<-1>/.match("xyzyz") # => #<MatchData "yzyz" 1:"y" 2:"z">
        assert_regex(vec!["yzyz", "y", "z"], "xyzyz", r#"(.)(.)\k<-2>\k<-1>"#);
        // /([aeiou]\w){2}/.match("Caenorhabditis elegans") #=> #<MatchData "enor" 1:"or">
        assert_regex(
            vec!["enor", "or"],
            "Caenorhabditis elegans",
            r#"([aeiou]\w){2}"#,
        );
        // /[aeiou]\w{2}/.match("Caenorhabditis elegans") #=> #<MatchData "aen">
        assert_regex(vec!["aen"], "Caenorhabditis elegans", r#"[aeiou]\w{2}"#);
        // /I(n)ves(ti)ga\2ons/.match("Investigations") # => #<MatchData "Investigations" 1:"n" 2:"ti">
        assert_regex(
            vec!["Investigations", "n", "ti"],
            "Investigations",
            r#"I(n)ves(ti)ga\2ons"#,
        );
        // /I(?:n)ves(ti)ga\1ons/.match("Investigations") # => #<MatchData "Investigations" 1:"ti">
        assert_regex(
            vec!["Investigations", "ti"],
            "Investigations",
            r#"I(?:n)ves(ti)ga\1ons"#,
        );
        // /\A(?<a>|.|(?:(?<b>.)\g<a>\k<b+0>))\z/.match("rekxker") # => #<MatchData "rekxker" a:"rekxker" b:"k">
        assert_regex(
            vec!["rekxker", "rekxker", "k"],
            "rekxker",
            r#"\A(?<a>|.|(?:(?<b>.)\g<a>\k<b+0>))\z"#,
        );
        // /(?<=<b>)\w+(?=<\/b>)/.match("Fortune favours the <b>bold</b>") # => #<MatchData "bold">
        assert_regex(
            vec!["bold"],
            "Fortune favours the <b>bold</b>",
            r#"(?<=<b>)\w+(?=<\/b>)"#,
        );
        // /<b>\K\w+(?=<\/b>)/.match("Fortune favours the <b>bold</b>") # => #<MatchData "bold">
        assert_regex(
            vec!["bold"],
            "Fortune favours the <b>bold</b>",
            r#"<b>\K\w+(?=<\/b>)"#,
        );
        // /a(?i:b)c/.match("aBc") # => #<MatchData "aBc">
        assert_regex(vec!["aBc"], "aBc", r#"a(?i:b)c"#);
        // /a(?i:b)c/.match("abc") # => #<MatchData "abc">
        assert_regex(vec!["abc"], "abc", r#"a(?i:b)c"#);
        // /a(?i)bc/.match("aBc") # => #<MatchData "aBc">
        assert_regex(vec!["aBc"], "aBc", r#"a(?i)bc"#);
        // /a(?i)bc/.match("aBC") # => #<MatchData "aBC">
        assert_regex(vec!["aBc"], "aBc", r#"a(?i)bc"#);
        // /a(?:(?i)bc)d/.match("aBCd") # => #<MatchData "aBCd">
        assert_regex(vec!["aBCd"], "aBCd", r#"a(?:(?i)bc)d"#);
        // /a(?:(?i)bc)d/.match("aBCD") # => nil
        assert_regex_nomatch("aBCD", r#"a(?:(?i)bc)d"#);
        //float_pat = /\A
        //  \d+ # 整数部
        //  (\. # 小数点
        //    \d+ # 小数部
        //  )?  # 小数点 + 小数部 はなくともよい
        //\z/x
        //float_pat.match("3.14") # => #<MatchData "3.14" 1:".14">
        assert_regex_with_mode(
            vec!["3.14", ".14"],
            "3.14",
            r##"\A
          \d+   # 整数部
          (\.   # 小数点
            \d+ # 小数部
          )?    # 小数点 + 小数部 はなくともよい
        \z"##,
            OnigmoOption::FreeFormat,
        );
        //assert_regex_with_mode(
        //    vec!["A3.14", ".14"],
        //    "a3.14",
        //    r##"\A
        //  A
        //  \d+   # 整数部
        //  (\.   # 小数点
        //    \d+ # 小数部
        //  )?    # 小数点 + 小数部 はなくともよい
        //\z"##,
        //    OnigmoOption::FreeFormat | OnigmoOption::IgnoreCase,
        //);
    }
}
