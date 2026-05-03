use crate::bytes::MatchBytes;
use crate::Match;
use alloc::string::String;

/// Returns the smallest possible index of the next valid UTF-8 sequence
/// starting after `i`.
///
/// Adapted from a function with the same name in the `regex` crate.
pub(crate) fn next_input_pos(text: &[u8], i: usize) -> usize {
    let b = match text.get(i) {
        None => return i + 1,
        Some(&b) => b,
    };
    i + crate::codepoint_len(b)
}

/// A trait abstracting over input types for regex matching.
///
/// This trait is implemented for `str`, `String`, `[u8]`, and `[u8; N]`, allowing
/// regex methods to work with both UTF-8 text and raw byte slices.
///
/// When the regex is compiled with [`BytesMode::Ascii`](crate::BytesMode),
/// patterns like `.` will match any byte in `[u8]` input. Without bytes mode,
/// `.` only matches valid UTF-8 codepoints even in byte slices.
///
/// The associated type [`Match<'t>`](Self::Match) is the concrete match type
/// returned for a given input: [`Match<'t>`](crate::Match) for string inputs,
/// [`MatchBytes<'t>`](crate::MatchBytes) for byte slice inputs.
pub trait RegexInput {
    /// The match type produced for this input.
    type Match<'t>
    where
        Self: 't;

    /// Returns the length of the input in bytes.
    fn len(&self) -> usize;
    /// Returns the input as a raw byte slice.
    fn as_bytes(&self) -> &[u8];
    /// Returns `true` if `ix` is a valid position between UTF-8 codepoints.
    fn is_char_boundary(&self, ix: usize) -> bool;
    /// Returns `true` if the entire input is ASCII.
    fn is_ascii(&self) -> bool;
    /// Steps back one codepoint from position `i`, returning the start position.
    fn prev_codepoint_ix(&self, i: usize) -> usize;
    /// Returns `true` if the input is empty.
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
    /// Construct a match from byte offsets.
    fn make_match<'t>(&'t self, start: usize, end: usize) -> Self::Match<'t>;
    /// Advance past the codepoint at position `i`.
    fn advance_position(&self, i: usize) -> usize;
}

impl<S: RegexInput + ?Sized> RegexInput for &S {
    type Match<'t>
        = S::Match<'t>
    where
        Self: 't;

    fn len(&self) -> usize {
        (**self).len()
    }
    fn as_bytes(&self) -> &[u8] {
        (**self).as_bytes()
    }
    fn is_char_boundary(&self, ix: usize) -> bool {
        (**self).is_char_boundary(ix)
    }
    fn is_ascii(&self) -> bool {
        (**self).is_ascii()
    }
    fn prev_codepoint_ix(&self, i: usize) -> usize {
        (**self).prev_codepoint_ix(i)
    }
    fn make_match<'t>(&'t self, start: usize, end: usize) -> Self::Match<'t> {
        (**self).make_match(start, end)
    }
    fn advance_position(&self, i: usize) -> usize {
        (**self).advance_position(i)
    }
}

impl RegexInput for str {
    type Match<'t> = Match<'t>;

    fn len(&self) -> usize {
        str::len(self)
    }
    fn as_bytes(&self) -> &[u8] {
        str::as_bytes(self)
    }
    fn is_char_boundary(&self, ix: usize) -> bool {
        str::is_char_boundary(self, ix)
    }
    fn is_ascii(&self) -> bool {
        str::is_ascii(self)
    }
    fn prev_codepoint_ix(&self, i: usize) -> usize {
        crate::prev_codepoint_ix(self, i)
    }
    fn make_match<'t>(&'t self, start: usize, end: usize) -> Match<'t> {
        Match::new(self, start, end)
    }
    fn advance_position(&self, i: usize) -> usize {
        next_input_pos(self.as_bytes(), i)
    }
}

impl RegexInput for String {
    type Match<'t> = Match<'t>;

    fn len(&self) -> usize {
        String::len(self)
    }
    fn as_bytes(&self) -> &[u8] {
        String::as_bytes(self)
    }
    fn is_char_boundary(&self, ix: usize) -> bool {
        str::is_char_boundary(self, ix)
    }
    fn is_ascii(&self) -> bool {
        str::is_ascii(self)
    }
    fn prev_codepoint_ix(&self, i: usize) -> usize {
        crate::prev_codepoint_ix(self, i)
    }
    fn make_match<'t>(&'t self, start: usize, end: usize) -> Match<'t> {
        Match::new(self, start, end)
    }
    fn advance_position(&self, i: usize) -> usize {
        next_input_pos(self.as_bytes(), i)
    }
}

impl RegexInput for [u8] {
    type Match<'t> = MatchBytes<'t>;

    fn len(&self) -> usize {
        <[u8]>::len(self)
    }
    fn as_bytes(&self) -> &[u8] {
        self
    }
    fn is_char_boundary(&self, ix: usize) -> bool {
        ix == 0 || ix >= <[u8]>::len(self) || self[ix] & 0xC0 != 0x80
    }
    fn is_ascii(&self) -> bool {
        <[u8]>::is_ascii(self)
    }
    fn prev_codepoint_ix(&self, mut i: usize) -> usize {
        i -= 1;
        while i > 0 && self[i] & 0xC0 == 0x80 {
            i -= 1;
        }
        i
    }
    fn make_match<'t>(&'t self, start: usize, end: usize) -> MatchBytes<'t> {
        MatchBytes::new(self, start, end)
    }
    fn advance_position(&self, i: usize) -> usize {
        next_input_pos(self, i)
    }
}

impl<const N: usize> RegexInput for [u8; N] {
    type Match<'t> = MatchBytes<'t>;

    fn len(&self) -> usize {
        N
    }
    fn as_bytes(&self) -> &[u8] {
        self
    }
    fn is_char_boundary(&self, ix: usize) -> bool {
        ix == 0 || ix >= N || self[ix] & 0xC0 != 0x80
    }
    fn is_ascii(&self) -> bool {
        <[u8]>::is_ascii(self)
    }
    fn prev_codepoint_ix(&self, mut i: usize) -> usize {
        i -= 1;
        while i > 0 && self[i] & 0xC0 == 0x80 {
            i -= 1;
        }
        i
    }
    fn make_match<'t>(&'t self, start: usize, end: usize) -> MatchBytes<'t> {
        MatchBytes::new(self, start, end)
    }
    fn advance_position(&self, i: usize) -> usize {
        next_input_pos(self, i)
    }
}
