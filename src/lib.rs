#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

pub mod OnigmoOption {
    pub const None: u32 = crate::ONIG_OPTION_NONE;
    pub const IgnoreCase: u32 = crate::ONIG_OPTION_IGNORECASE;
    pub const FreeFormat: u32 = crate::ONIG_OPTION_EXTEND;
    pub const DotAll: u32 = crate::ONIG_OPTION_DOTALL;
}

#[derive(Debug)]
pub struct OnigmoError {
    kind: OnigmoErrKind,
    message: String,
    span: Option<(usize, usize)>,
}

impl std::fmt::Display for OnigmoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Debug)]
pub enum OnigmoErrKind {
    InvalidPattern = -1,
    SearchError = -2,
    InvalidErrorMessage = -3,
    CallbackError = -4,
}

/// Represents a single match of a regex in a haystack.
///
/// A `Match` contains both the start and end byte offsets of the match and the
/// actual substring corresponding to the range of those byte offsets. It is
/// guaranteed that `start <= end`. When `start == end`, the match is empty.
///
/// Since this `Match` can only be produced by the top-level `Regex` APIs
/// that only support searching UTF-8 encoded strings, the byte offsets for a
/// `Match` are guaranteed to fall on valid UTF-8 codepoint boundaries. That
/// is, slicing a `&str` with [`Match::range`] is guaranteed to never panic.
///
/// The lifetime parameter `'h` refers to the lifetime of the matched of the
/// haystack that this match was produced from.
#[derive(Debug)]
pub struct Match<'h> {
    heystack: &'h str,
    start: usize,
    end: usize,
}

impl<'h> Match<'h> {
    /// Returns the substring of the haystack that matched.
    pub fn as_str(&'h self) -> &'h str {
        &self.heystack[self.start..self.end]
    }

    /// Returns the substring of the haystack that matched as `String`.
    pub fn to_string(&'h self) -> String {
        self.as_str().to_string()
    }

    /// Returns the byte offset of the start of the match in the haystack. The
    /// start of the match corresponds to the position where the match begins
    /// and includes the first byte in the match.
    ///
    /// It is guaranteed that `Match::start() <= Match::end()`.
    ///
    /// This is guaranteed to fall on a valid UTF-8 codepoint boundary. That
    /// is, it will never be an offset that appears between the UTF-8 code
    /// units of a UTF-8 encoded Unicode scalar value. Consequently, it is
    /// always safe to slice the corresponding haystack using this offset.
    pub fn start(&self) -> usize {
        self.start
    }

    /// Returns the byte offset of the end of the match in the haystack. The
    /// end of the match corresponds to the byte immediately following the last
    /// byte in the match. This means that `&slice[start..end]` works as one
    /// would expect.
    ///
    /// It is guaranteed that `Match::start() <= Match::end()`.
    ///
    /// This is guaranteed to fall on a valid UTF-8 codepoint boundary. That
    /// is, it will never be an offset that appears between the UTF-8 code
    /// units of a UTF-8 encoded Unicode scalar value. Consequently, it is
    /// always safe to slice the corresponding haystack using this offset.
    pub fn end(&self) -> usize {
        self.end
    }

    pub fn range(&self) -> std::ops::Range<usize> {
        self.start..self.end
    }
}

/// Represents the capture groups for a single match.
///
/// Capture groups refer to parts of a regex enclosed in parentheses. They
/// can be optionally named. The purpose of capture groups is to be able to
/// reference different parts of a match based on the original pattern. In
/// essence, a `Captures` is a container of [`Match`] values for each group
/// that participated in a regex match. Each `Match` can be looked up by either
/// its capture group index or name (if it has one).
///
/// `'h` is the lifetime of the matched text.
#[derive(Debug, Clone)]
pub struct Captures<'h> {
    heystack: &'h str,
    len: usize,
    region: *mut OnigRegion,
}

impl<'h> Captures<'h> {
    /// Returns the `Match` associated with the capture group at index `i`. If
    /// `i` does not correspond to a capture group, or if the capture group did
    /// not participate in the match, then `None` is returned.
    ///
    /// When `i == 0`, this is guaranteed to return a non-`None` value.
    pub fn get(&'h self, i: usize) -> Option<Match<'h>> {
        let region = unsafe { &*self.region };
        let beg: *const usize = region.beg as _;
        let end: *const usize = region.end as _;
        let (start, end) = unsafe { (*beg.add(i), *end.add(i)) };
        if i < self.len {
            Some(Match {
                heystack: self.heystack,
                start,
                end,
            })
        } else {
            None
        }
    }

    /// Returns the total number of capture groups. This includes both
    /// matching and non-matching groups.
    ///
    /// The length returned is always equivalent to the number of elements
    /// yielded by [`Captures::iter`]. Consequently, the length is always
    /// greater than zero since every `Captures` value always includes the
    /// match for the entire regex.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns an iterator over all capture groups. This includes both
    /// matching and non-matching groups.
    ///
    /// The iterator always yields at least one matching group: the first group
    /// (at index `0`) with no name. Subsequent groups are returned in the order
    /// of their opening parenthesis in the regex.
    ///
    /// The elements yielded have type `Option<Match<'h>>`, where a non-`None`
    /// value is present if the capture group matches.
    pub fn iter(&'h self) -> CapturesIter<'h> {
        CapturesIter { cap: self, i: 0 }
    }
}

pub struct CapturesIter<'h> {
    cap: &'h Captures<'h>,
    i: usize,
}

impl<'h> Iterator for CapturesIter<'h> {
    type Item = Match<'h>;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.cap.get(self.i)?;
        self.i += 1;
        Some(result)
    }
}

/// A compilled regular expression.
#[derive(Debug)]
pub struct Regex {
    raw: *mut re_pattern_buffer,
    pattern: String,
    error_info: OnigErrorInfo,
    option: u32,
}

unsafe impl Send for Regex {}
unsafe impl Sync for Regex {}

impl Regex {
    /// Parse and compile a regex with default options.
    ///
    /// Returns an `OnigmoError` if the pattern could not be parsed.
    pub fn new(pattern: &str) -> Result<Self, OnigmoError> {
        Self::new_with_option(pattern, OnigmoOption::None)
    }

    /// Parse and compile a regex with given options.
    ///
    /// Returns an `OnigmoError` if the pattern could not be parsed.
    pub fn new_with_option(pattern: &str, option: u32) -> Result<Self, OnigmoError> {
        let mut raw = std::ptr::null_mut();
        let pattern = pattern.to_string();
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
            option,
        })
    }

    /// Returns the pattern string.
    pub fn as_str(&self) -> &str {
        &self.pattern
    }

    /// Returns the capture groups for the first match in `heystack`.
    ///
    /// If no match is found, then `Ok(None)` is returned.
    ///
    /// # Examples
    ///
    /// Finding matches and capturing parts of the match:
    ///
    /// ```rust
    /// # use onigmo_regex::*;
    ///
    /// let mut re = OnigmoRegex::new(r"(\d{4})-(\d{2})-(\d{2})".to_string()).unwrap();
    /// let text = "The date was 2018-04-07";
    /// let captures = re.captures(text).unwrap().unwrap();
    ///
    /// assert_eq!(captures.get(1).unwrap().as_str(), "2018");
    /// assert_eq!(captures.get(2).unwrap().as_str(), "04");
    /// assert_eq!(captures.get(3).unwrap().as_str(), "07");
    /// assert_eq!(captures.get(0).unwrap().as_str(), "2018-04-07");
    /// ```
    pub fn captures<'h>(&self, heystack: &'h str) -> Result<Option<Captures<'h>>, OnigmoError> {
        self.captures_from_pos(heystack, 0)
    }

    /// Returns the capture groups for the first match in `heystack`, starting from
    /// the specified byte position `pos`.
    ///
    /// If no match is found, then `Ok(None)` is returned.
    ///
    /// # Examples
    ///
    /// Finding captures starting at a position:
    ///
    /// ```rust
    /// # use onigmo_regex::*;
    ///
    /// let mut re = OnigmoRegex::new(r"(?m:^)(\d+)".to_string()).unwrap();
    /// let text = "1 test 123\n2 foo";
    /// let captures = re.captures_from_pos(text, 7).unwrap().unwrap();
    ///
    /// let group = captures.get(1).unwrap();
    /// assert_eq!(group.as_str(), "2");
    /// assert_eq!(group.start(), 11);
    /// assert_eq!(group.end(), 12);
    /// ```
    ///
    pub fn captures_from_pos<'h>(
        &self,
        heystack: &'h str,
        pos: usize,
    ) -> Result<Option<Captures<'h>>, OnigmoError> {
        let hey_start = heystack.as_ptr();
        let hey_end = unsafe { hey_start.add(heystack.len()) };
        let range_start = unsafe { hey_start.add(pos) };
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
            let len = unsafe { *region }.num_regs as usize;
            Ok(Some(Captures {
                heystack,
                len,
                region,
            }))
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

#[cfg(test)]
mod test {
    use super::*;

    fn assert_regex(expected: Option<&[&'static str]>, heystack: &str, pattern: &str) {
        assert_regex_with_mode(expected, heystack, pattern, OnigmoOption::None);
    }

    fn assert_regex_with_mode<'h>(
        expected: Option<&[&'static str]>,
        heystack: &'h str,
        pattern: &str,
        option: u32,
    ) {
        let actual: Option<Vec<String>> = Regex::new_with_option(pattern, option)
            .unwrap()
            .captures(heystack)
            .unwrap()
            .map(|c| c.iter().map(|s| s.to_string()).collect());
        assert_eq!(
            expected.map(|s| s.into_iter().map(|c| c.to_string()).collect::<Vec<_>>()),
            actual
        );
    }

    #[test]
    fn test() {
        let _ = unsafe { onig_init() };
        // /[a-z[0-9]]/.match("y") # => #<MatchData "y">
        assert_regex(Some(&["y"]), "y", r#"[a-z[0-9]]"#);
        // /[a-z[0-9]]/.match("[") # => nil
        assert_regex(None, "[", r#"[a-z[0-9]]"#);
        // r = /[a-w&&[^c-g]e]/ # ([a-w] かつ ([^c-g] もしくは e)) つまり [abeh-w] と同じ
        let pat = r#"[a-w&&[^c-g]e]"#;
        // r.match("b") # => #<MatchData "b">
        assert_regex(Some(&["b"]), "b", pat);
        // r.match("c") # => nil
        assert_regex(None, "c", pat);
        // r.match("e") # => #<MatchData "e">
        assert_regex(Some(&["e"]), "e", pat);
        // r.match("g") # => nil
        assert_regex(None, "g", pat);
        // r.match("h") # => #<MatchData "h">
        assert_regex(Some(&["h"]), "h", pat);
        // r.match("w") # => #<MatchData "w">
        assert_regex(Some(&["w"]), "w", pat);
        // r.match("z") # => nil
        assert_regex(None, "z", pat);
        // /(.)(.)\k<-2>\k<-1>/.match("xyzyz") # => #<MatchData "yzyz" 1:"y" 2:"z">
        assert_regex(Some(&["yzyz", "y", "z"]), "xyzyz", r#"(.)(.)\k<-2>\k<-1>"#);
        // /([aeiou]\w){2}/.match("Caenorhabditis elegans") #=> #<MatchData "enor" 1:"or">
        assert_regex(
            Some(&["enor", "or"]),
            "Caenorhabditis elegans",
            r#"([aeiou]\w){2}"#,
        );
        // /[aeiou]\w{2}/.match("Caenorhabditis elegans") #=> #<MatchData "aen">
        assert_regex(Some(&["aen"]), "Caenorhabditis elegans", r#"[aeiou]\w{2}"#);
        // /I(n)ves(ti)ga\2ons/.match("Investigations") # => #<MatchData "Investigations" 1:"n" 2:"ti">
        assert_regex(
            Some(&["Investigations", "n", "ti"]),
            "Investigations",
            r#"I(n)ves(ti)ga\2ons"#,
        );
        // /I(?:n)ves(ti)ga\1ons/.match("Investigations") # => #<MatchData "Investigations" 1:"ti">
        assert_regex(
            Some(&["Investigations", "ti"]),
            "Investigations",
            r#"I(?:n)ves(ti)ga\1ons"#,
        );
        // /\A(?<a>|.|(?:(?<b>.)\g<a>\k<b+0>))\z/.match("rekxker") # => #<MatchData "rekxker" a:"rekxker" b:"k">
        assert_regex(
            Some(&["rekxker", "rekxker", "k"]),
            "rekxker",
            r#"\A(?<a>|.|(?:(?<b>.)\g<a>\k<b+0>))\z"#,
        );
        // /(?<=<b>)\w+(?=<\/b>)/.match("Fortune favours the <b>bold</b>") # => #<MatchData "bold">
        assert_regex(
            Some(&["bold"]),
            "Fortune favours the <b>bold</b>",
            r#"(?<=<b>)\w+(?=<\/b>)"#,
        );
        // /<b>\K\w+(?=<\/b>)/.match("Fortune favours the <b>bold</b>") # => #<MatchData "bold">
        assert_regex(
            Some(&["bold"]),
            "Fortune favours the <b>bold</b>",
            r#"<b>\K\w+(?=<\/b>)"#,
        );
        // /a(?i:b)c/.match("aBc") # => #<MatchData "aBc">
        assert_regex(Some(&["aBc"]), "aBc", r#"a(?i:b)c"#);
        // /a(?i:b)c/.match("abc") # => #<MatchData "abc">
        assert_regex(Some(&["abc"]), "abc", r#"a(?i:b)c"#);
        // /a(?i)bc/.match("aBc") # => #<MatchData "aBc">
        assert_regex(Some(&["aBc"]), "aBc", r#"a(?i)bc"#);
        // /a(?i)bc/.match("aBC") # => #<MatchData "aBC">
        assert_regex(Some(&["aBc"]), "aBc", r#"a(?i)bc"#);
        // /a(?:(?i)bc)d/.match("aBCd") # => #<MatchData "aBCd">
        assert_regex(Some(&["aBCd"]), "aBCd", r#"a(?:(?i)bc)d"#);
        // /a(?:(?i)bc)d/.match("aBCD") # => nil
        assert_regex(None, "aBCD", r#"a(?:(?i)bc)d"#);
        //float_pat = /\A
        //  \d+ # 整数部
        //  (\. # 小数点
        //    \d+ # 小数部
        //  )?  # 小数点 + 小数部 はなくともよい
        //\z/x
        //float_pat.match("3.14") # => #<MatchData "3.14" 1:".14">
        assert_regex_with_mode(
            Some(&["3.14", ".14"]),
            "3.14",
            r##"\A
          \d+   # 整数部
          (\.   # 小数点
            \d+ # 小数部
          )?    # 小数点 + 小数部 はなくともよい
        \z"##,
            OnigmoOption::FreeFormat,
        );
        assert_regex_with_mode(
            Some(&["a3.14", ".14"]),
            "a3.14",
            r##"\A
          A
          \d+   # 整数部
          (\.   # 小数点
            \d+ # 小数部
          )?    # 小数点 + 小数部 はなくともよい
        \z"##,
            OnigmoOption::FreeFormat | OnigmoOption::IgnoreCase,
        );
    }
}
