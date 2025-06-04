use crate::*;

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
    region: Region,
    offset: usize,
}

impl<'h> Captures<'h> {
    pub(crate) fn new(heystack: &'h str, region: Region, offset: usize) -> Self {
        Self {
            heystack,
            region,
            offset,
        }
    }

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
        let (start, end) = self.region.pos(i)?;
        Some(Match {
            heystack: self.heystack,
            start,
            end,
        })
    }

    /// Returns the total number of capture groups. This includes both
    /// matching and non-matching groups.
    ///
    /// The length returned is always equivalent to the number of elements
    /// yielded by [`Captures::iter`]. Consequently, the length is always
    /// greater than zero since every `Captures` value always includes the
    /// match for the entire regex.
    pub fn len(&self) -> usize {
        self.region.len()
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

    /// Returns the substring of the haystack behind the matched substring.
    pub fn post(&'h self) -> &'h str {
        &self.heystack[self.end..]
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

    /// Returns the range over the starting and ending byte offsets of the
    /// match in the haystack.
    ///
    /// It is always correct to slice the original haystack searched with this
    /// range. That is, because the offsets are guaranteed to fall on valid
    /// UTF-8 boundaries, the range returned is always valid.
    pub fn range(&self) -> std::ops::Range<usize> {
        self.start..self.end
    }
}

pub struct SubCaptures<'h> {
    caps: &'h Captures<'h>,
    i: usize,
}

impl<'h> Iterator for SubCaptures<'h> {
    type Item = Option<Match<'h>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.i >= self.caps.len() {
            return None;
        }
        let result = self.caps.get(self.i);
        self.i += 1;
        Some(result)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = self.caps.len() - self.i;
        (size, Some(size))
    }

    fn count(self) -> usize {
        self.caps.len() - self.i
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
            region,
            offset: r,
        }))
    }
}

impl<'r, 'h> std::iter::FusedIterator for FindCaptures<'r, 'h> {}

impl<'r, 'h> FindCaptures<'r, 'h> {
    pub(crate) fn new(regex: &'r Regex, heystack: &'h str) -> Self {
        Self {
            regex,
            heystack,
            last_end: 0,
            last_match_end: None,
        }
    }
}
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

impl<'r, 'h> FindMatches<'r, 'h> {
    pub(crate) fn new(regex: &'r Regex, heystack: &'h str) -> Self {
        Self {
            regex,
            region: Region::new(),
            heystack,
            last_end: 0,
            last_match_end: None,
        }
    }
}
