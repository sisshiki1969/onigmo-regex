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

impl OnigmoError {
    pub fn custom(message: &str) -> Self {
        Self {
            kind: OnigmoErrKind::Custom,
            message: message.to_string(),
            span: None,
        }
    }

    pub fn from_code(code: isize) -> Self {
        let mut s = [0; ONIG_MAX_ERROR_MESSAGE_LEN as usize];
        let err_len = unsafe { onig_error_code_to_str(s.as_mut_ptr(), code as _) } as usize;
        let message = match std::str::from_utf8(&s[..err_len]) {
            Ok(err) => err.to_string(),
            Err(err) => {
                return OnigmoError {
                    kind: OnigmoErrKind::InvalidErrorMessage,
                    message: format!("Error message is invalid UTF-8: {err}"),
                    span: None,
                };
            }
        };
        Self {
            kind: OnigmoErrKind::Custom,
            message,
            span: None,
        }
    }
}

#[derive(Debug)]
pub enum OnigmoErrKind {
    InvalidPattern = -1,
    SearchError = -2,
    InvalidErrorMessage = -3,
    CallbackError = -4,
    Custom = -5,
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
#[derive(Debug)]
pub struct Captures<'h> {
    heystack: &'h str,
    len: usize,
    region: Region,
    offset: usize,
}

impl<'h> Captures<'h> {
    /// Returns the start and end positions of the Nth capture group. Returns
    /// `None` if i is not a valid capture group or if the capture group did
    /// not match anything. The positions returned are always byte indices with
    /// respect to the original string matched.
    pub fn pos(&self, pos: usize) -> Option<(usize, usize)> {
        self.region.pos(pos)
    }

    /// Returns the matched string for the capture group `i`. If `i` isn't
    /// a valid capture group or didn't match anything, then `None` is returned.
    pub fn at(&self, pos: usize) -> Option<&'h str> {
        self.pos(pos).map(|(beg, end)| &self.heystack[beg..end])
    }

    /// Returns the `Match` associated with the capture group at index `i`. If
    /// `i` does not correspond to a capture group, or if the capture group did
    /// not participate in the match, then `None` is returned.
    ///
    /// When `i == 0`, this is guaranteed to return a non-`None` value.
    pub fn get(&'h self, i: usize) -> Option<Match<'h>> {
        let start = self.region.beg(i);
        let end = self.region.end(i);
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
    pub fn iter(&'h self) -> SubCaptures<'h> {
        SubCaptures { caps: self, i: 0 }
    }
}

/// An iterator that yields all non-overlapping capture groups matching a
/// particular regular expression.
///
/// The iterator stops when no more matches can be found.
///
/// `'r` is the lifetime of the `Regex` struct and `'h` is the lifetime
/// of the matched string.
pub struct FindCaptures<'r, 'h> {
    regex: &'r Regex,
    heystack: &'h str,
    last_end: usize,
    last_match_end: Option<usize>,
}

impl<'r, 'h> Iterator for FindCaptures<'r, 'h> {
    type Item = Result<Captures<'h>, OnigmoError>;

    fn next(&mut self) -> Option<Result<Captures<'h>, OnigmoError>> {
        if self.last_end > self.heystack.len() {
            return None;
        }

        let mut region = Region::new();
        let r = match self.regex.search(
            self.heystack,
            self.last_end,
            self.heystack.len(),
            Some(&mut region),
        ) {
            Ok(pos) => pos?,
            Err(err) => return Some(Err(err)),
        };
        let (s, e) = region.pos(0).unwrap();

        // Don't accept empty matches immediately following the last match.
        // i.e., no infinite loops please.
        if e == s && self.last_match_end.map_or(false, |l| l == e) {
            self.last_end += self.heystack[self.last_end..]
                .chars()
                .next()
                .map(|c| c.len_utf8())
                .unwrap_or(1);
            return self.next();
        } else {
            self.last_end = e;
            self.last_match_end = Some(e);
        }
        Some(Ok(Captures {
            heystack: self.heystack,
            len: region.len(),
            region,
            offset: r,
        }))
    }
}

impl<'r, 'h> std::iter::FusedIterator for FindCaptures<'r, 'h> {}

/// An iterator over all non-overlapping matches for a particular string.
///
/// The iterator yields a tuple of integers corresponding to the start and end
/// of the match. The indices are byte offsets. The iterator stops when no more
/// matches can be found.
///
/// `'r` is the lifetime of the `Regex` struct and `'h` is the lifetime
/// of the matched string.
pub struct FindMatches<'r, 'h> {
    regex: &'r Regex,
    region: Region,
    heystack: &'h str,
    last_end: usize,
    last_match_end: Option<usize>,
}

impl<'r, 'h> Iterator for FindMatches<'r, 'h> {
    type Item = Result<(usize, usize), OnigmoError>;

    fn next(&mut self) -> Option<Result<(usize, usize), OnigmoError>> {
        if self.last_end > self.heystack.len() {
            return None;
        }
        self.region.clear();
        match self.regex.search(
            self.heystack,
            self.last_end,
            self.heystack.len(),
            Some(&mut self.region),
        ) {
            Ok(pos) => pos?,
            Err(err) => return Some(Err(err)),
        };
        let (s, e) = self.region.pos(0).unwrap();

        // Don't accept empty matches immediately following the last match.
        // i.e., no infinite loops please.
        if e == s && self.last_match_end.map_or(false, |l| l == e) {
            self.last_end += self.heystack[self.last_end..]
                .chars()
                .next()
                .map(|c| c.len_utf8())
                .unwrap_or(1);
            return self.next();
        } else {
            self.last_end = e;
            self.last_match_end = Some(e);
        }

        Some(Ok((s, e)))
    }
}

impl<'r, 'h> std::iter::FusedIterator for FindMatches<'r, 'h> {}

pub struct SubCaptures<'h> {
    caps: &'h Captures<'h>,
    i: usize,
}

impl<'h> Iterator for SubCaptures<'h> {
    type Item = Match<'h>;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.caps.get(self.i)?;
        self.i += 1;
        Some(result)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = self.caps.len();
        (size, Some(size))
    }

    fn count(self) -> usize {
        self.caps.len()
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub struct Region(*mut OnigRegion);

impl Drop for Region {
    fn drop(&mut self) {
        unsafe { onig_region_free(self.0, 1) };
    }
}

impl Region {
    pub fn new() -> Self {
        let region = unsafe { onig_region_new() };
        Self(region)
    }

    pub fn len(&self) -> usize {
        unsafe { (*self.0).num_regs as usize }
    }

    pub fn beg(&self, i: usize) -> usize {
        unsafe { *(*self.0).beg.add(i) as _ }
    }

    pub fn end(&self, i: usize) -> usize {
        unsafe { *(*self.0).end.add(i) as _ }
    }

    pub fn pos(&self, i: usize) -> Option<(usize, usize)> {
        if i < self.len() {
            Some((self.beg(i), self.end(i)))
        } else {
            None
        }
    }

    /// This can be used to clear out a region so it can be used
    /// again.
    pub fn clear(&mut self) {
        unsafe { onig_region_clear(self.0) };
    }
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
                region.0,
                self.option,
            )
        };

        if r >= 0 {
            let len = region.len();
            Ok(Some(Captures {
                heystack,
                len,
                region,
                offset: r as usize,
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
        FindCaptures {
            regex: self,
            heystack,
            last_end: 0,
            last_match_end: None,
        }
    }

    /// Search pattern in string
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
        let (beg, end) = (heystack.as_ptr(), unsafe {
            heystack.as_ptr().add(heystack.len())
        });
        let r = unsafe {
            let start = beg.add(from);
            let range = beg.add(to);
            if start > end {
                return Err(OnigmoError::custom("Start of match should be before end"));
            }
            if range > end {
                return Err(OnigmoError::custom("Limit of match should be before end"));
            }
            onig_search(
                self.raw,
                beg,
                end,
                start,
                range,
                match region {
                    Some(region) => (*region).0,
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
        FindMatches {
            regex: self,
            region: Region::new(),
            heystack,
            last_end: 0,
            last_match_end: None,
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
        std::slice::from_raw_parts(name_start, name_end.offset_from(name_start) as usize)
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
