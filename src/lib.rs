#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

mod captures;
mod error;
mod region;

pub use captures::{Captures, CapturesBytes, FindCaptures, FindMatches, Match, SubCaptures};
pub use error::OnigmoError;
pub use region::Region;

pub mod OnigmoOption {
    pub const None: u32 = crate::ONIG_OPTION_NONE;
    pub const IgnoreCase: u32 = crate::ONIG_OPTION_IGNORECASE;
    pub const FreeFormat: u32 = crate::ONIG_OPTION_EXTEND;
    pub const DotAll: u32 = crate::ONIG_OPTION_DOTALL;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OnigmoEncoding {
    UTF8,
    ASCII,
    UTF16BE,
    UTF16LE,
    UTF32BE,
    UTF32LE,
    EUC_JP,
    EUC_CN,
    EUC_KR,
    EUC_TW,
    Shift_JIS,
    Windows_31J,
    Big5,
    GB18030,
    KOI8_R,
    KOI8_U,
    ISO_8859_1,
    ISO_8859_2,
    ISO_8859_3,
    ISO_8859_4,
    ISO_8859_5,
    ISO_8859_6,
    ISO_8859_7,
    ISO_8859_8,
    ISO_8859_9,
    ISO_8859_10,
    ISO_8859_11,
    ISO_8859_13,
    ISO_8859_14,
    ISO_8859_15,
    ISO_8859_16,
    Windows_1250,
    Windows_1251,
    Windows_1252,
    Windows_1253,
    Windows_1254,
    Windows_1257,
}

impl OnigmoEncoding {
    pub fn as_type(&self) -> OnigEncoding {
        unsafe {
            match self {
                OnigmoEncoding::UTF8         => &OnigEncodingUTF_8 as _,
                OnigmoEncoding::ASCII        => &OnigEncodingASCII as _,
                OnigmoEncoding::UTF16BE      => &OnigEncodingUTF_16BE as _,
                OnigmoEncoding::UTF16LE      => &OnigEncodingUTF_16LE as _,
                OnigmoEncoding::UTF32BE      => &OnigEncodingUTF_32BE as _,
                OnigmoEncoding::UTF32LE      => &OnigEncodingUTF_32LE as _,
                OnigmoEncoding::EUC_JP       => &OnigEncodingEUC_JP as _,
                OnigmoEncoding::EUC_CN       => &OnigEncodingEUC_CN as _,
                OnigmoEncoding::EUC_KR       => &OnigEncodingEUC_KR as _,
                OnigmoEncoding::EUC_TW       => &OnigEncodingEUC_TW as _,
                OnigmoEncoding::Shift_JIS    => &OnigEncodingShift_JIS as _,
                OnigmoEncoding::Windows_31J  => &OnigEncodingWindows_31J as _,
                OnigmoEncoding::Big5         => &OnigEncodingBIG5 as _,
                OnigmoEncoding::GB18030      => &OnigEncodingGB18030 as _,
                OnigmoEncoding::KOI8_R       => &OnigEncodingKOI8_R as _,
                OnigmoEncoding::KOI8_U       => &OnigEncodingKOI8_U as _,
                OnigmoEncoding::ISO_8859_1   => &OnigEncodingISO_8859_1 as _,
                OnigmoEncoding::ISO_8859_2   => &OnigEncodingISO_8859_2 as _,
                OnigmoEncoding::ISO_8859_3   => &OnigEncodingISO_8859_3 as _,
                OnigmoEncoding::ISO_8859_4   => &OnigEncodingISO_8859_4 as _,
                OnigmoEncoding::ISO_8859_5   => &OnigEncodingISO_8859_5 as _,
                OnigmoEncoding::ISO_8859_6   => &OnigEncodingISO_8859_6 as _,
                OnigmoEncoding::ISO_8859_7   => &OnigEncodingISO_8859_7 as _,
                OnigmoEncoding::ISO_8859_8   => &OnigEncodingISO_8859_8 as _,
                OnigmoEncoding::ISO_8859_9   => &OnigEncodingISO_8859_9 as _,
                OnigmoEncoding::ISO_8859_10  => &OnigEncodingISO_8859_10 as _,
                OnigmoEncoding::ISO_8859_11  => &OnigEncodingISO_8859_11 as _,
                OnigmoEncoding::ISO_8859_13  => &OnigEncodingISO_8859_13 as _,
                OnigmoEncoding::ISO_8859_14  => &OnigEncodingISO_8859_14 as _,
                OnigmoEncoding::ISO_8859_15  => &OnigEncodingISO_8859_15 as _,
                OnigmoEncoding::ISO_8859_16  => &OnigEncodingISO_8859_16 as _,
                OnigmoEncoding::Windows_1250 => &OnigEncodingWindows_1250 as _,
                OnigmoEncoding::Windows_1251 => &OnigEncodingWindows_1251 as _,
                OnigmoEncoding::Windows_1252 => &OnigEncodingWindows_1252 as _,
                OnigmoEncoding::Windows_1253 => &OnigEncodingWindows_1253 as _,
                OnigmoEncoding::Windows_1254 => &OnigEncodingWindows_1254 as _,
                OnigmoEncoding::Windows_1257 => &OnigEncodingWindows_1257 as _,
            }
        }
    }
}

// ---------------------------------------------------------------------
// Compile-time warning collection.
//
// Onigmo reports pattern-level diagnostics ("nested repeat operator
// '?' and '+' was replaced with '*' in regular expression", ...)
// through a process-global warning callback which defaults to a
// no-op. CRuby routes it to `rb_warn`; we capture the messages into a
// thread-local buffer during `onig_new` and attach them to the
// resulting `Regex`, so callers (e.g. a Ruby implementation) can
// forward them to their own warning mechanism.
//
// The callback fires on the thread running `onig_new`, so a
// thread-local buffer needs no locking.
// ---------------------------------------------------------------------

thread_local! {
    static COMPILE_WARNINGS: std::cell::RefCell<Vec<String>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

unsafe extern "C" fn collect_warning(s: *const std::os::raw::c_char) {
    if s.is_null() {
        return;
    }
    let msg = unsafe { std::ffi::CStr::from_ptr(s) }
        .to_string_lossy()
        .into_owned();
    COMPILE_WARNINGS.with(|w| w.borrow_mut().push(msg));
}

fn install_warn_hooks_once() {
    static ONCE: std::sync::Once = std::sync::Once::new();
    ONCE.call_once(|| unsafe {
        onig_set_warn_func(Some(collect_warning));
        onig_set_verb_warn_func(Some(collect_warning));
    });
}

fn drain_warnings() -> Vec<String> {
    COMPILE_WARNINGS.with(|w| std::mem::take(&mut *w.borrow_mut()))
}

/// A compilled regular expression.
#[derive(Debug)]
pub struct Regex {
    raw: *mut re_pattern_buffer,
    /// The pattern bytes as compiled. UTF-8 for regexes built via
    /// [`Regex::new`] and friends; arbitrary bytes (interpreted under
    /// the compile-time encoding) for [`Regex::new_bytes_with_encoding`].
    pattern: Vec<u8>,
    option: u32,
    encoding: OnigmoEncoding,
    /// Diagnostics Onigmo emitted while parsing this pattern (e.g.
    /// "nested repeat operator ... was replaced with ..."). Empty for
    /// clean patterns.
    warnings: Vec<String>,
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
        Self::new_with_option_and_encoding(pattern, option, OnigmoEncoding::UTF8)
    }

    /// Parse and compile a regex with given options.
    ///
    /// Returns an `OnigmoError` if the pattern could not be parsed.
    pub fn new_with_option_and_encoding(
        pattern: &str,
        option: u32,
        encoding: OnigmoEncoding,
    ) -> Result<Self, OnigmoError> {
        Self::new_bytes_with_encoding(pattern.as_bytes(), option, encoding)
    }

    /// Parse and compile a regex from arbitrary bytes under the given
    /// [`OnigmoEncoding`].
    ///
    /// Use this for non-UTF-8 patterns (Shift_JIS, EUC-JP, ISO-8859,
    /// ...); the bytes are handed to Onigmo verbatim and interpreted
    /// under `encoding`. For UTF-8 patterns prefer [`Regex::new`] /
    /// [`Regex::new_with_option_and_encoding`], which take a `&str`.
    pub fn new_bytes_with_encoding(
        pattern: &[u8],
        option: u32,
        encoding: OnigmoEncoding,
    ) -> Result<Self, OnigmoError> {
        install_warn_hooks_once();
        drain_warnings(); // discard any stale entries from a failed prior compile
        let mut raw = std::ptr::null_mut();
        let pattern: Vec<u8> = pattern.to_vec();
        let pattern_start: *const u8 = pattern.as_ptr();
        let pattern_end = unsafe { pattern_start.add(pattern.len()) };
        let mut einfo = std::mem::MaybeUninit::uninit();
        let r = unsafe {
            onig_new(
                &mut raw as _,
                pattern_start,
                pattern_end,
                option as u32,
                encoding.as_type(),
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
            encoding,
            warnings: drain_warnings(),
        })
    }

    /// Diagnostics Onigmo emitted while parsing this pattern, e.g.
    /// `nested repeat operator '?' and '+' was replaced with '*' in
    /// regular expression`. Empty for clean patterns. CRuby surfaces
    /// these via `rb_warn`; callers embedding this crate can forward
    /// them to their own warning mechanism.
    pub fn warnings(&self) -> &[String] {
        &self.warnings
    }

    /// Returns the pattern as a `&str`.
    ///
    /// Panics if the pattern is not valid UTF-8 (only possible for
    /// regexes built with [`Regex::new_bytes_with_encoding`]). Use
    /// [`Regex::as_bytes`] to get the raw pattern bytes safely.
    pub fn as_str(&self) -> &str {
        std::str::from_utf8(&self.pattern)
            .expect("regex pattern is not UTF-8; use as_bytes()")
    }

    /// Returns the pattern bytes as compiled. This is the same slice
    /// that was handed to Onigmo.
    pub fn as_bytes(&self) -> &[u8] {
        &self.pattern
    }

    /// Returns the compile-time option bits (`ONIG_OPTION_*`).
    pub fn option(&self) -> u32 {
        self.option
    }

    /// Returns the encoding this regex was compiled under.
    pub fn encoding(&self) -> OnigmoEncoding {
        self.encoding
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

    /// Anchored match: try the pattern exactly at `heystack[at..]` (Onigmo's
    /// `onig_match`, no forward search) and record the registers into the
    /// caller-owned `region`, which is reused across calls — no allocation
    /// per match once it has grown to the pattern's group count. Returns the
    /// byte offset (into `heystack`) of the match end.
    ///
    /// `\A` and `^` anchor at the start of `heystack`, so a caller that
    /// wants CRuby `StringScanner` semantics (anchors at the scan position)
    /// passes the suffix as `heystack` with `at == 0`. `heystack` must be
    /// valid under the regex's compile-time encoding, and `at` a character
    /// boundary.
    pub fn match_at_with_region(
        &self,
        heystack: &[u8],
        at: usize,
        region: &mut Region,
    ) -> Result<Option<usize>, OnigmoError> {
        let hey_start = heystack.as_ptr();
        let hey_end = unsafe { hey_start.add(heystack.len()) };
        let at_ptr = unsafe { hey_start.add(at) };
        let r = unsafe {
            onig_match(
                self.raw,
                hey_start,
                hey_end,
                at_ptr,
                region.raw(),
                self.option,
            )
        };
        Self::onig_result(r).map(|r| r.map(|len| at + len))
    }

    /// Forward search from `heystack[from..]` recording the registers into
    /// the caller-owned, reusable `region` (see [`Regex::match_at_with_region`]).
    /// Returns the byte offset of the match start.
    pub fn search_with_region(
        &self,
        heystack: &[u8],
        from: usize,
        region: &mut Region,
    ) -> Result<Option<usize>, OnigmoError> {
        let hey_start = heystack.as_ptr();
        let hey_end = unsafe { hey_start.add(heystack.len()) };
        let range_start = unsafe { hey_start.add(from) };
        let r = unsafe {
            onig_search(
                self.raw,
                hey_start,
                hey_end,
                range_start,
                hey_end,
                region.raw(),
                self.option,
            )
        };
        Self::onig_result(r)
    }

    /// Map an `onig_match` / `onig_search` return code to
    /// `Ok(Some(position))`, `Ok(None)` on `ONIG_MISMATCH`, or the
    /// engine's error message.
    fn onig_result(r: OnigPosition) -> Result<Option<usize>, OnigmoError> {
        if r >= 0 {
            Ok(Some(r as usize))
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

    /// Byte-slice analogue of [`Regex::captures`] for non-UTF-8
    /// haystacks (Shift_JIS / EUC-JP / ISO-8859 / ...). The regex
    /// should have been compiled with a matching `OnigmoEncoding`,
    /// otherwise Onigmo may report an encoding-mismatch error.
    pub fn captures_bytes<'h>(
        &self,
        heystack: &'h [u8],
    ) -> Result<Option<CapturesBytes<'h>>, OnigmoError> {
        self.captures_bytes_from_pos(heystack, 0)
    }

    /// Byte-slice analogue of [`Regex::captures_from_pos`].
    pub fn captures_bytes_from_pos<'h>(
        &self,
        heystack: &'h [u8],
        pos: usize,
    ) -> Result<Option<CapturesBytes<'h>>, OnigmoError> {
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
            Ok(Some(CapturesBytes::new(heystack, region, r as usize)))
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

    /// Byte-slice analogue of [`Regex::search`]. The regex should have
    /// been compiled with a matching `OnigmoEncoding` for `heystack`.
    pub fn search_bytes(
        &self,
        heystack: &[u8],
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

    #[test]
    fn windows_31j_dot_matches_one_char() {
        // /./ compiled under Windows-31J, matched against
        // "\xc3\xe9" (Windows-31J bytes): 0xC3 is a single-byte char
        // (it's outside the 0x81-0x9F / 0xE0-0xFC lead-byte range),
        // so `/./` should match just the first byte `\xc3`.
        let re = Regex::new_bytes_with_encoding(
            b".",
            OnigmoOption::None,
            OnigmoEncoding::Windows_31J,
        )
        .unwrap();
        let caps = re.captures_bytes(b"\xc3\xe9").unwrap().unwrap();
        assert_eq!(caps.at(0), Some(&b"\xc3"[..]));
    }

    #[test]
    fn euc_jp_pattern_matches_multibyte() {
        // A single EUC-JP two-byte char (`あ` = 0xa4 0xa2) should be
        // matched by `/./` compiled under EUC-JP as one character
        // (two bytes).
        let re = Regex::new_bytes_with_encoding(
            b".",
            OnigmoOption::None,
            OnigmoEncoding::EUC_JP,
        )
        .unwrap();
        let caps = re.captures_bytes(b"\xa4\xa2").unwrap().unwrap();
        assert_eq!(caps.at(0), Some(&b"\xa4\xa2"[..]));
    }

    #[test]
    fn iso_8859_1_matches_bytewise() {
        let re = Regex::new_bytes_with_encoding(
            b".",
            OnigmoOption::None,
            OnigmoEncoding::ISO_8859_1,
        )
        .unwrap();
        // Latin-1 "é" is a single byte 0xE9.
        let caps = re.captures_bytes(b"\xe9x").unwrap().unwrap();
        assert_eq!(caps.at(0), Some(&b"\xe9"[..]));
    }

    #[test]
    fn nested_quantifier_not_reduced() {
        // Bug #17341 (ported from CRuby): a+?* must behave as (a+?)*,
        // not be reduced away. Before the ReduceTypeTable fix the
        // match against "aa" stopped at "a".
        let re = Regex::new("a+?*").unwrap();
        let caps = re.captures("aa").unwrap().unwrap();
        assert_eq!(caps.at(0), Some("aa"));
        let caps = re.captures("").unwrap().unwrap();
        assert_eq!(caps.at(0), Some(""));
        // a+?+ likewise stays unreduced.
        let re = Regex::new("a+?+").unwrap();
        let caps = re.captures("aa").unwrap().unwrap();
        assert_eq!(caps.at(0), Some("aa"));
    }

    #[test]
    fn word_class_matches_join_control() {
        // CRuby >= 3.4 includes Join_Control (U+200C/U+200D) in the
        // word ctype per UTS #18; CR_Word carries the same range now.
        let re = Regex::new("[[:word:]]").unwrap();
        assert!(re.captures("\u{200C}").unwrap().is_some());
        assert!(re.captures("\u{200D}").unwrap().is_some());
        let re = Regex::new(r"\p{Word}").unwrap();
        assert!(re.captures("\u{200C}").unwrap().is_some());
        // Ruby's \w is ASCII-only (OP_ASCII_WORD) — it must NOT be
        // affected by the CR_Word change.
        let re = Regex::new(r"\w").unwrap();
        assert!(re.captures("\u{200C}").unwrap().is_none());
        assert!(re.captures("\u{3042}").unwrap().is_none());
    }

    #[test]
    fn compile_warnings_are_collected() {
        // A{0,1}+ triggers "nested repeat operator '?' and '+' was
        // replaced with '*' in regular expression".
        let re = Regex::new("foo(A{0,1}+)Abar").unwrap();
        assert_eq!(re.warnings().len(), 1, "warnings: {:?}", re.warnings());
        assert!(
            re.warnings()[0].contains("nested repeat operator"),
            "unexpected warning: {:?}",
            re.warnings()
        );
        // The match semantics agree with CRuby.
        let caps = re.captures("fooAAAbar").unwrap().unwrap();
        assert_eq!(caps.at(0), Some("fooAAAbar"));
        assert_eq!(caps.at(1), Some("AA"));
        // A clean pattern collects nothing (and drains stale state).
        let re = Regex::new("abc").unwrap();
        assert!(re.warnings().is_empty());
    }

    #[test]
    fn encoding_is_preserved() {
        let re = Regex::new_bytes_with_encoding(
            b"a",
            OnigmoOption::None,
            OnigmoEncoding::Shift_JIS,
        )
        .unwrap();
        assert_eq!(re.encoding(), OnigmoEncoding::Shift_JIS);
        assert_eq!(re.as_bytes(), b"a");
    }
}
