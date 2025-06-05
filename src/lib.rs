#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

mod captures;
mod error;
mod region;

pub use captures::{Captures, FindCaptures, FindMatches, Match, SubCaptures};
pub use error::OnigmoError;
pub use region::Region;

pub mod OnigmoOption {
    pub const None: u32 = crate::ONIG_OPTION_NONE;
    pub const IgnoreCase: u32 = crate::ONIG_OPTION_IGNORECASE;
    pub const FreeFormat: u32 = crate::ONIG_OPTION_EXTEND;
    pub const DotAll: u32 = crate::ONIG_OPTION_DOTALL;
}

/// A compilled regular expression.
#[derive(Debug)]
pub struct Regex {
    raw: *mut re_pattern_buffer,
    pattern: String,
    option: u32,
}

unsafe impl Send for Regex {}
unsafe impl Sync for Regex {}

impl Drop for Regex {
    fn drop(&mut self) {
        if !self.raw.is_null() {
            unsafe { onig_free(self.raw) };
        }
    }
}

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
            return Err(OnigmoError::new_with_span(err.to_string(), span));
        }
        Ok(Self {
            raw,
            pattern,
            option,
        })
    }

    /// Returns the pattern string.
    pub fn as_str(&self) -> &str {
        &self.pattern
    }

    /// Returns the pattern string.
    pub fn option(&self) -> u32 {
        self.option
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
    /// let mut re = Regex::new(r"(\d{4})-(\d{2})-(\d{2})").unwrap();
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
    /// let mut re = Regex::new(r"(?m:^)(\d+)").unwrap();
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
        let region = Region::new();

        let r = unsafe {
            onig_search(
                self.raw,
                hey_start,
                hey_end,
                range_start,
                range_end,
                region.raw(),
                self.option,
            )
        };

        if r >= 0 {
            Ok(Some(Captures::new(heystack, region, r as usize)))
        } else if r == ONIG_MISMATCH as _ {
            Ok(None)
        } else {
            let mut s = [0; ONIG_MAX_ERROR_MESSAGE_LEN as usize];
            let err_len = unsafe { onig_error_code_to_str(s.as_mut_ptr(), r as _) } as usize;
            let message = match std::str::from_utf8(&s[..err_len]) {
                Ok(err) => err.to_string(),
                Err(err) => {
                    return Err(OnigmoError::new(format!(
                        "Error message is invalid UTF-8: {err}"
                    )));
                }
            };
            Err(OnigmoError::new(message))
        }
    }

    /// Returns an iterator over all the non-overlapping capture groups matched
    /// in `text`. This is operationally the same as `find_iter` (except it
    /// yields information about submatches).
    ///
    /// # Example
    ///
    /// We can use this to find all movie titles and their release years in
    /// some text, where the movie is formatted like "'Title' (xxxx)":
    ///
    /// ```rust
    /// # use onigmo_regex::*;
    /// # fn main() {
    /// let re = Regex::new(r"'([^']+)'\s+\((\d{4})\)").unwrap();
    /// let heystack = "'Citizen Kane' (1941), 'The Wizard of Oz' (1939), 'M' (1931).";
    /// let mut it = re.captures_iter(heystack).map(|caps| caps.unwrap());
    /// let cap0 = it.next().unwrap();
    /// assert_eq!(cap0.at(1).unwrap(), "Citizen Kane");
    /// assert_eq!(cap0.at(2).unwrap(), "1941");
    /// let cap1 = it.next().unwrap();
    /// assert_eq!(cap1.at(1).unwrap(), "The Wizard of Oz");
    /// assert_eq!(cap1.at(2).unwrap(), "1939");
    /// let cap2 = it.next().unwrap();
    /// assert_eq!(cap2.at(1).unwrap(), "M");
    /// assert_eq!(cap2.at(2).unwrap(), "1931");
    /// assert!(it.next().is_none());
    /// # }
    /// ```
    pub fn captures_iter<'r, 'h>(&'r self, heystack: &'h str) -> FindCaptures<'r, 'h> {
        FindCaptures::new(self, heystack)
    }

    /// Search pattern in string.
    ///
    /// Search for matches the regex in a string. This method will return the
    /// index of the first match of the regex within the string, if
    /// there is one. If `from` is less than `to`, then search is performed
    /// in forward order, otherwise – in backward order.
    ///
    /// # Arguments
    ///
    ///  * `heystack` - The string to search in.
    ///  * `from` - The byte index in the passed slice to start search
    ///  * `to` - The byte index in the passed slice to finish search
    ///  * `options` - The options for the search.
    ///  * `region` - The region for return group match range info
    ///
    /// # Returns
    ///
    /// `Some(pos)` if the regex matches, where `pos` is the
    /// byte-position of the start of the match. `None` if the regex
    /// doesn't match anywhere in `heystack`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use onigmo_regex::*;
    ///
    /// let r = Regex::new("l{1,2}").unwrap();
    /// let res = r.search("hello", 0, 5, None).unwrap();
    /// assert_eq!(Some(2), res); // match starts at character 3
    /// ```
    pub fn search(
        &self,
        heystack: &str,
        from: usize,
        to: usize,
        region: Option<&mut Region>,
    ) -> Result<Option<usize>, OnigmoError> {
        let beg = heystack.as_ptr();
        let end = unsafe { beg.add(heystack.len()) };
        let r = unsafe {
            let start = beg.add(from);
            let range = beg.add(to);
            if start > end {
                return Err(OnigmoError::new("Start of match should be before end"));
            }
            if range > end {
                return Err(OnigmoError::new("Limit of match should be before end"));
            }
            onig_search(
                self.raw,
                beg,
                end,
                start,
                range,
                match region {
                    Some(region) => (*region).raw(),
                    None => std::ptr::null_mut(),
                },
                self.option,
            )
        };

        if r >= 0 {
            Ok(Some(r as usize))
        } else if r == ONIG_MISMATCH as isize {
            Ok(None)
        } else {
            Err(OnigmoError::from_code(r))
        }
    }

    /// Find pattern in string.
    ///
    /// Finds the first match of the regular expression within the
    /// string.
    ///
    /// # Arguments
    ///  * `heystack` - The text to search in.
    ///
    /// # Returns
    ///
    ///  The offset of the start and end of the first match. If no
    ///  match exists `None` is returned.
    /// # Examples
    ///
    /// ```
    /// # use onigmo_regex::*;
    ///
    /// let r = Regex::new("l{1,2}").unwrap();
    /// let res = r.find("hello").unwrap();
    /// assert_eq!(Some((2, 4)), res);
    /// ```
    pub fn find(&self, heystack: &str) -> Result<Option<(usize, usize)>, OnigmoError> {
        let mut region = Region::new();
        let len = heystack.len();
        Ok(self
            .search(heystack, 0, len, Some(&mut region))?
            .and_then(|_| region.pos(0)))
    }

    /// Returns an iterator for each successive non-overlapping match in `heystack`,
    /// returning the start and end byte indices with respect to `heystack`.
    ///
    /// # Example
    ///
    /// Find the start and end location of every word with exactly 13
    /// characters:
    ///
    /// ```rust
    /// # use onigmo_regex::*;
    /// # fn main() {
    /// let text = "Retroactively relinquishing remunerations is reprehensible.";
    /// for pos in Regex::new(r"\b\w{13}\b").unwrap().find_iter(text) {
    ///     println!("{:?}", pos);
    /// }
    /// // Output:
    /// // (0, 13)
    /// // (14, 27)
    /// // (28, 41)
    /// // (45, 58)
    /// # }
    /// ```
    pub fn find_iter<'r, 'h>(&'r self, heystack: &'h str) -> FindMatches<'r, 'h> {
        FindMatches::new(self, heystack)
    }

    /// Enumerate capture names, returning a vector of names for each capture group.
    ///
    /// If the regex has no named captures, an empty vector is returned.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use onigmo_regex::*;
    /// let re = Regex::new(r"(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})").unwrap();
    /// let names = re.capture_names().unwrap();
    /// assert_eq!(names, vec!["year".to_string(), "month".to_string(), "day".to_string()]);
    /// ```
    ///
    /// ```rust
    /// # use onigmo_regex::*;
    /// let re = Regex::new(r"(?<A::B>.)(?<a>.)(.)(?<foo-bar>.)(.)(?<foo-bar>x)?(?<Ruby>.)").unwrap();
    /// let names = re.capture_names().unwrap();
    /// assert_eq!(names, vec![
    ///     "A::B".to_string(),
    ///     "a".to_string(),
    ///     "foo-bar".to_string(),
    ///     "foo-bar".to_string(),
    ///     "Ruby".to_string(),
    /// ]);
    /// ```
    ///
    /// ```rust
    /// # use onigmo_regex::*;
    /// let re = Regex::new(r"(\d{4})-(\d{2})-(\d{2})").unwrap();
    /// let names = re.capture_names().unwrap();
    /// assert!(names.is_empty());
    /// ```
    pub fn capture_names(&self) -> Result<Vec<String>, OnigmoError> {
        if unsafe { onig_number_of_names(self.raw) } == 0 {
            return Ok(vec![]);
        }
        let len = unsafe { onig_number_of_captures(self.raw) } as usize;
        let mut names = vec![String::new(); len];
        let res = unsafe {
            onig_foreach_name(
                self.raw,
                Some(names_callback),
                &mut names as *mut Vec<_> as _,
            )
        };
        if res != 0 {
            return Err(OnigmoError::new("Failed to get names"));
        }
        Ok(names)
    }

    /// Returns the list of group numbers for a given named capture.
    /// If the name does not exist or has no captures, `None` is returned.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use onigmo_regex::*;
    /// let re = Regex::new(r"(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})").unwrap();
    /// let nums = re.get_group_nembers("year");
    /// assert_eq!(nums, vec![1]);
    /// ```
    ///
    /// ```rust
    /// # use onigmo_regex::*;
    /// let re = Regex::new(r"(?<A::B>.)(?<a>.)(.)(?<foo-bar>.)(.)(?<foo-bar>x)?(?<Ruby>.)").unwrap();
    /// let nums = re.get_group_nembers("foo-bar");
    /// assert_eq!(nums, vec![3, 4]);
    /// ```
    pub fn get_group_nembers(&self, name: &str) -> Vec<i32> {
        let mut nums: *mut std::os::raw::c_int = std::ptr::null_mut();
        let name_start = name.as_ptr();
        let name_end = unsafe { name_start.add(name.len()) };
        let r =
            unsafe { onig_name_to_group_numbers(self.raw, name_start, name_end, &mut nums as _) };
        if r <= 0 {
            return vec![];
        }
        let len = r as usize;
        unsafe { std::slice::from_raw_parts(nums, len) }.to_vec()
    }
}

extern "C" fn names_callback(
    name_start: *const u8,
    name_end: *const u8,
    group_len: i32,
    group_list: *mut i32,
    _buf: *mut re_pattern_buffer,
    f: *mut std::ffi::c_void,
) -> i32 {
    let name = unsafe {
        std::slice::from_raw_parts(name_start, name_end.offset_from(name_start) as usize)
    };
    let name = match std::str::from_utf8(name) {
        Ok(name) => name,
        Err(_) => return -1i32,
    };
    let v = unsafe { &mut *(f as *mut Vec<String>) };
    let list =
        unsafe { std::slice::from_raw_parts(group_list as *const _, group_len as usize).iter() };
    for i in list {
        v[*i as usize - 1] = name.to_string();
    }
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
            .map(|c| c.iter().flat_map(|s| s.map(|s| s.to_string())).collect());
        assert_eq!(
            expected.map(|s| s.into_iter().map(|c| c.to_string()).collect::<Vec<_>>()),
            actual
        );
    }

    #[test]
    fn test_match() {
        let _ = unsafe { onig_init() };
        // /[a-z[0-9]]/.match("y") # => #<MatchData "y">
        assert_regex(Some(&["y"]), "y", r#"[a-z[0-9]]"#);
        // /[a-z[0-9]]/.match("[") # => nil
        assert_regex(None, "[", r#"[a-z[0-9]]"#);
        // r = /[a-w&&[^c-g]e]/ # ([a-w] かつ ([^c-g] もしくは e)) つまり [abeh-w] と同じ
    }

    #[test]
    fn char_class() {
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
    }

    #[test]
    fn back_reference() {
        // /(.)(.)\k<-2>\k<-1>/.match("xyzyz") # => #<MatchData "yzyz" 1:"y" 2:"z">
        assert_regex(Some(&["yzyz", "y", "z"]), "xyzyz", r#"(.)(.)\k<-2>\k<-1>"#);
    }

    #[test]
    fn grouping() {
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
    }

    #[test]
    fn subexpression() {
        // /\A(?<a>|.|(?:(?<b>.)\g<a>\k<b+0>))\z/.match("rekxker") # => #<MatchData "rekxker" a:"rekxker" b:"k">
        assert_regex(
            Some(&["rekxker", "rekxker", "k"]),
            "rekxker",
            r#"\A(?<a>|.|(?:(?<b>.)\g<a>\k<b+0>))\z"#,
        );
    }

    #[test]
    fn lookahead() {
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
    }

    #[test]
    fn options() {
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

    #[test]
    fn find_iter() {
        let text = "Retroactively relinquishing remunerations is reprehensible.";
        for pos in Regex::new(r"\b\w{13}\b").unwrap().find_iter(text) {
            println!("{:?}", pos);
        }
        // Output:
        // (0, 13)
        // (14, 27)
        // (28, 41)
        // (45, 58)
    }
}
