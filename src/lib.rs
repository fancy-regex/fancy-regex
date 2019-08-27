// Copyright 2016 The Fancy Regex Authors.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

/*!
An implementation of regexes, supporting a relatively rich set of features, including backreferences
and look-around.

It builds on top of the excellent [regex](https://crates.io/crates/regex) crate. If you are not
familiar with it, make sure you read its documentation and maybe you don't even need fancy-regex.

If your regex or parts of it does not use any special features, the matching is delegated to the
regex crate. That means it has linear runtime. But if you use "fancy" features such as
backreferences or look-around, an engine with backtracking needs to be used. In that case, depending
on the regex and the input you can run into what is called "catastrophic backtracking".

# Usage

The API should feel very similar to the regex crate, and involves compiling a regex and then using
it to find matches in text.

An example with backreferences to check if a text consists of two identical words:

```rust
use fancy_regex::Regex;

let re = Regex::new(r"^(\w+) (\1)$").unwrap();
let result = re.is_match("foo foo");

assert!(result.is_ok());
let matched = result.unwrap();
assert!(matched);
```

Note that like in the regex crate, the regex needs anchors like `^` and `$` to match against the
entire input text.

# Example: Find matches

```rust
use fancy_regex::Regex;

let re = Regex::new(r"\d+").unwrap();
let result = re.find("foo 123");

assert!(result.is_ok(), "execution was successful");
let match_option = result.unwrap();

assert!(match_option.is_some(), "found a match");
let m = match_option.unwrap();

assert_eq!(m.start(), 4);
assert_eq!(m.end(), 7);
assert_eq!(m.as_str(), "123");
```
*/

#![deny(warnings)]
//#![deny(missing_docs)]
#![deny(missing_debug_implementations)]

use bit_set::BitSet;
use std::fmt;
use std::usize;

mod analyze;
mod compile;
mod parse;
mod vm;

use crate::analyze::analyze;
use crate::compile::compile;
use crate::parse::Parser;
use crate::vm::Prog;

const MAX_RECURSION: usize = 64;

// the public API

pub type Result<T> = ::std::result::Result<T, Error>;

// We use one Error type for both compile time and run time errors,
// to minimize the boilerplate for callers.
#[derive(Debug)]
pub enum Error {
    // Compile time errors
    ParseError,
    UnclosedOpenParen,
    InvalidRepeat,
    RecursionExceeded,
    LookBehindNotConst,
    TrailingBackslash,
    InvalidEscape,
    UnclosedUnicodeName,
    InvalidHex,
    InvalidCodepointValue,
    InvalidClass,
    UnknownFlag,
    NonUnicodeUnsupported,
    InvalidBackref,
    InnerError(regex::Error),

    // Run time errors
    StackOverflow,
}

pub struct Regex(RegexImpl);

// Separate enum because we don't want to expose any of this
enum RegexImpl {
    // Do we want to box this? It's pretty big...
    Wrap {
        inner: regex::Regex,
        inner1: Option<Box<regex::Regex>>,
        original: String,
    },
    Fancy {
        prog: Prog,
        n_groups: usize,
        original: String,
    },
}

/// A single match of a regex in an input text
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Match<'t> {
    text: &'t str,
    start: usize,
    end: usize,
}

#[derive(Debug)]
pub struct Captures<'t>(CapturesImpl<'t>);

#[derive(Debug)]
enum CapturesImpl<'t> {
    Wrap {
        text: &'t str,
        inner: regex::Captures<'t>,

        // starting position, in _from_pos variants
        offset: usize,

        enclosing_groups: usize,
    },
    Fancy {
        text: &'t str,
        saves: Vec<usize>,
    },
}

/// Iterator for captured groups in order in which they appear in the regex.
#[derive(Debug)]
pub struct SubCaptureMatches<'c, 't> {
    caps: &'c Captures<'t>,
    i: usize,
}

impl fmt::Debug for Regex {
    /// Shows the original regular expression.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl Regex {
    /// Parse and compile a regex.
    pub fn new(re: &str) -> Result<Regex> {
        let (raw_e, backrefs) = Expr::parse(re)?;

        // wrapper to search for re at arbitrary start position,
        // and to capture the match bounds
        let e = Expr::Concat(vec![
            Expr::Repeat {
                child: Box::new(Expr::Any { newline: true }),
                lo: 0,
                hi: usize::MAX,
                greedy: false,
            },
            Expr::Group(Box::new(raw_e)),
        ]);

        let info = analyze(&e, &backrefs)?;

        let inner_info = &info.children[1].children[0]; // references inner expr
        if !inner_info.hard {
            // easy case, wrap regex

            // we do our own to_str because escapes are different
            let mut re_cooked = String::new();
            // same as raw_e above, but it was moved, so traverse to find it
            let raw_e = match e {
                Expr::Concat(ref v) => match v[1] {
                    Expr::Group(ref child) => child,
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
            raw_e.to_str(&mut re_cooked, 0);
            let inner = compile::compile_inner(&re_cooked)?;
            let inner1 = if inner_info.looks_left {
                // create regex to handle 1-char look-behind
                let re1 = ["^(?s:.)+?(", re_cooked.as_str(), ")"].concat();
                let compiled = compile::compile_inner(&re1)?;
                Some(Box::new(compiled))
            } else {
                None
            };
            return Ok(Regex(RegexImpl::Wrap {
                inner,
                inner1,
                original: re.to_string(),
            }));
        }

        let prog = compile(&info)?;
        Ok(Regex(RegexImpl::Fancy {
            prog,
            n_groups: info.end_group,
            original: re.to_string(),
        }))
    }

    /// Returns the original string of this regex.
    pub fn as_str(&self) -> &str {
        match &self.0 {
            RegexImpl::Wrap { ref original, .. } => original,
            RegexImpl::Fancy { ref original, .. } => original,
        }
    }

    /// Check if the regex matches the input text.
    ///
    /// # Example
    ///
    /// Test if some text contains the same word twice:
    ///
    /// ```rust
    /// # use fancy_regex::Regex;
    ///
    /// let re = Regex::new(r"(\w+) \1").unwrap();
    /// assert!(re.is_match("mirror mirror on the wall").unwrap());
    /// ```
    pub fn is_match(&self, text: &str) -> Result<bool> {
        match &self.0 {
            RegexImpl::Wrap { ref inner, .. } => Ok(inner.is_match(text)),
            RegexImpl::Fancy { ref prog, .. } => {
                let result = vm::run(prog, text, 0, 0)?;
                Ok(result.is_some())
            }
        }
    }

    /// Find the first match in the input text.
    ///
    /// If you have capturing groups in your regex that you want to extract, use the [captures()]
    /// method.
    ///
    /// # Example
    ///
    /// Find a word that is followed by an exclamation point:
    ///
    /// ```rust
    /// # use fancy_regex::Regex;
    ///
    /// let re = Regex::new(r"\w+(?=!)").unwrap();
    /// assert_eq!(re.find("so fancy!").unwrap().unwrap().as_str(), "fancy");
    /// ```
    pub fn find<'t>(&self, text: &'t str) -> Result<Option<Match<'t>>> {
        match &self.0 {
            RegexImpl::Wrap { inner, .. } => Ok(inner
                .find(text)
                .map(|m| Match::new(text, m.start(), m.end()))),
            RegexImpl::Fancy { prog, .. } => {
                let result = vm::run(prog, text, 0, 0)?;
                Ok(result.map(|saves| Match::new(text, saves[0], saves[1])))
            }
        }
    }

    /// Returns the capture groups for the first match in `text`.
    ///
    /// If no match is found, then `Ok(None)` is returned.
    ///
    /// # Examples
    ///
    /// Finding matches and capturing parts of the match:
    ///
    /// ```rust
    /// # use fancy_regex::Regex;
    ///
    /// let re = Regex::new(r"(\d{4})-(\d{2})-(\d{2})").unwrap();
    /// let text = "The date was 2018-04-07";
    /// let captures = re.captures(text).unwrap().unwrap();
    ///
    /// assert_eq!(captures.get(1).unwrap().as_str(), "2018");
    /// assert_eq!(captures.get(2).unwrap().as_str(), "04");
    /// assert_eq!(captures.get(3).unwrap().as_str(), "07");
    /// assert_eq!(captures.get(0).unwrap().as_str(), "2018-04-07");
    /// ```
    pub fn captures<'t>(&self, text: &'t str) -> Result<Option<Captures<'t>>> {
        match &self.0 {
            RegexImpl::Wrap { inner, .. } => Ok(inner.captures(text).map(|caps| {
                Captures(CapturesImpl::Wrap {
                    text,
                    inner: caps,
                    offset: 0,
                    enclosing_groups: 0,
                })
            })),
            RegexImpl::Fancy { prog, n_groups, .. } => {
                let result = vm::run(prog, text, 0, 0)?;
                Ok(result.map(|mut saves| {
                    saves.truncate(n_groups * 2);
                    Captures(CapturesImpl::Fancy { text, saves: saves })
                }))
            }
        }
    }

    /// Returns the capture groups for the first match in `text`, starting from
    /// the specified byte position `pos`.
    ///
    /// # Examples
    ///
    /// Finding captures starting at a position:
    ///
    /// ```
    /// # use fancy_regex::Regex;
    /// let re = Regex::new(r"(?m:^)(\d+)").unwrap();
    /// let text = "1 test 123\n2 foo";
    /// let captures = re.captures_from_pos(text, 7).unwrap().unwrap();
    ///
    /// let group = captures.get(1).unwrap();
    /// assert_eq!(group.as_str(), "2");
    /// assert_eq!(group.start(), 11);
    /// assert_eq!(group.end(), 12);
    /// ```
    ///
    /// Note that in some cases this is not the same as using the `captures`
    /// methods and passing a slice of the string, see the capture that we get
    /// when we do this:
    ///
    /// ```
    /// # use fancy_regex::Regex;
    /// let re = Regex::new(r"(?m:^)(\d+)").unwrap();
    /// let text = "1 test 123\n2 foo";
    /// let captures = re.captures(&text[7..]).unwrap().unwrap();
    /// assert_eq!(captures.get(1).unwrap().as_str(), "123");
    /// ```
    ///
    /// This matched the number "123" because it's at the beginning of the text
    /// of the string slice.
    ///
    pub fn captures_from_pos<'t>(&self, text: &'t str, pos: usize) -> Result<Option<Captures<'t>>> {
        match &self.0 {
            RegexImpl::Wrap { inner, inner1, .. } => {
                if inner1.is_none() || pos == 0 {
                    let result = inner.captures(&text[pos..]);
                    Ok(result.map(|caps| {
                        Captures(CapturesImpl::Wrap {
                            text,
                            inner: caps,
                            offset: pos,
                            enclosing_groups: 0,
                        })
                    }))
                } else {
                    let ix = prev_codepoint_ix(text, pos);
                    let inner1 = inner1.as_ref().unwrap();
                    let result = inner1.captures(&text[ix..]);
                    Ok(result.map(|caps| {
                        Captures(CapturesImpl::Wrap {
                            text,
                            inner: caps,
                            offset: ix,
                            enclosing_groups: 1,
                        })
                    }))
                }
            }
            RegexImpl::Fancy { prog, n_groups, .. } => {
                let result = vm::run(prog, text, pos, 0)?;
                Ok(result.map(|mut saves| {
                    saves.truncate(n_groups * 2);
                    Captures(CapturesImpl::Fancy { text, saves })
                }))
            }
        }
    }

    // for debugging only
    #[doc(hidden)]
    pub fn debug_print(&self) {
        match &self.0 {
            RegexImpl::Wrap { inner, .. } => println!("wrapped {:?}", inner),
            RegexImpl::Fancy { prog, .. } => prog.debug_print(),
        }
    }
}

impl<'t> Match<'t> {
    /// Returns the starting byte offset of the match in the text.
    #[inline]
    pub fn start(&self) -> usize {
        self.start
    }

    /// Returns the ending byte offset of the match in the text.
    #[inline]
    pub fn end(&self) -> usize {
        self.end
    }

    /// Returns the matched text.
    #[inline]
    pub fn as_str(&self) -> &'t str {
        &self.text[self.start..self.end]
    }

    fn new(text: &'t str, start: usize, end: usize) -> Match<'t> {
        Match { text, start, end }
    }
}

impl<'t> Captures<'t> {
    /// Get the capture group by its index in the regex.
    ///
    /// If there is no match for that group or the index does not correspond to a group, `None` is
    /// returned. The index 0 returns the whole match.
    pub fn get(&self, i: usize) -> Option<Match<'t>> {
        match &self.0 {
            CapturesImpl::Wrap {
                text,
                inner,
                offset,
                enclosing_groups,
            } => inner.get(i + *enclosing_groups).map(|m| Match {
                text,
                start: m.start() + *offset,
                end: m.end() + *offset,
            }),
            CapturesImpl::Fancy { text, ref saves } => {
                if i >= saves.len() {
                    return None;
                }
                let lo = saves[i * 2];
                if lo == std::usize::MAX {
                    return None;
                }
                let hi = saves[i * 2 + 1];
                Some(Match {
                    text,
                    start: lo,
                    end: hi,
                })
            }
        }
    }

    /// Iterate over the captured groups in order in which they appeared in the regex. The first
    /// capture corresponds to the whole match.
    pub fn iter<'c>(&'c self) -> SubCaptureMatches<'c, 't> {
        SubCaptureMatches { caps: self, i: 0 }
    }

    /// How many groups were captured.
    pub fn len(&self) -> usize {
        match self.0 {
            CapturesImpl::Wrap {
                ref inner,
                enclosing_groups,
                ..
            } => inner.len() - enclosing_groups,
            CapturesImpl::Fancy { ref saves, .. } => saves.len() / 2,
        }
    }
}

impl<'c, 't> Iterator for SubCaptureMatches<'c, 't> {
    type Item = Option<Match<'t>>;

    fn next(&mut self) -> Option<Option<Match<'t>>> {
        if self.i < self.caps.len() {
            let result = self.caps.get(self.i);
            self.i += 1;
            Some(result)
        } else {
            None
        }
    }
}

// TODO: might be nice to implement ExactSizeIterator etc for SubCaptures

// impl error traits (::std::error::Error, fmt::Display)

/// Regular expression AST. This is public for now but may change.
#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    /// An empty expression, e.g. the last branch in `(a|b|)`
    Empty,
    /// Any character, regex `.`
    Any {
        /// Whether it also matches newlines or not
        newline: bool,
    },
    /// Start of input text
    StartText,
    /// End of input text
    EndText,
    /// Start of a line
    StartLine,
    /// End of a line
    EndLine,
    /// The string as a literal, e.g. `a`
    Literal {
        /// The string to match
        val: String,
        /// Whether match is case-insensitive or not
        casei: bool,
    },
    /// Concatenation of multiple expressions, must match in order, e.g. `a.` is a concatenation of
    /// the literal `a` and `.` for any character
    Concat(Vec<Expr>),
    /// Alternative of multiple expressions, one of them must match, e.g. `a|b` is an alternative
    /// where either the literal `a` or `b` must match
    Alt(Vec<Expr>),
    /// Capturing group of expression, e.g. `(a.)` matches `a` and any character and "captures"
    /// (remembers) the match
    Group(Box<Expr>),
    /// Look-around (e.g. positive/negative look-ahead or look-behind) with an expression, e.g.
    /// `(?=a)` means the next character must be `a` (but the match is not consumed)
    LookAround(Box<Expr>, LookAround),
    /// Repeat of an expression, e.g. `a*` or `a+` or `a{1,3}`
    Repeat {
        /// The expression that is being repeated
        child: Box<Expr>,
        /// The minimum number of repetitions
        lo: usize,
        /// The maximum number of repetitions (or `usize::MAX`)
        hi: usize,
        /// Greedy means as much as possible is matched, e.g. `.*b` would match all of `abab`.
        /// Non-greedy means as little as possible, e.g. `.*?b` would match only `ab` in `abab`.
        greedy: bool,
    },
    /// Delegate a regex to the regex crate. This is used as a simplification so that we don't have
    /// to represent all the expressions in the AST, e.g. character classes.
    Delegate {
        /// The regex
        inner: String,
        /// How many characters the regex matches
        size: usize, // TODO: move into analysis result
        /// Whether the matching is case-insensitive or not
        casei: bool,
    },
    /// Back reference to a capture group, e.g. `\1` in `(abc|def)\1` references the captured group
    /// and the whole regex matches either `abcabc` or `defdef`.
    Backref(usize),
    /// Atomic non-capturing group, e.g. `(?>ab|a)` in text that contains `ab` will match `ab` and
    /// never backtrack and try `a`, even if matching fails after the atomic group.
    AtomicGroup(Box<Expr>),
}

/// Type of look-around assertion as used for a look-around expression.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LookAround {
    /// Look-ahead assertion, e.g. `(?=a)`
    LookAhead,
    /// Negative look-ahead assertion, e.g. `(?!a)`
    LookAheadNeg,
    /// Look-behind assertion, e.g. `(?<=a)`
    LookBehind,
    /// Negative look-behind assertion, e.g. `(?<!a)`
    LookBehindNeg,
}

// silly to write my own, but this is super-fast for the common 1-digit
// case.
fn push_usize(s: &mut String, x: usize) {
    if x >= 10 {
        push_usize(s, x / 10);
        s.push((b'0' + (x % 10) as u8) as char);
    } else {
        s.push((b'0' + (x as u8)) as char);
    }
}

fn push_quoted(buf: &mut String, s: &str) {
    for c in s.chars() {
        match c {
            '\\' | '.' | '+' | '*' | '?' | '(' | ')' | '|' | '[' | ']' | '{' | '}' | '^' | '$'
            | '#' => buf.push('\\'),
            _ => (),
        }
        buf.push(c);
    }
}

impl Expr {
    /// Parse the regex and return an expression (AST) and a bit set with the indexes of groups
    /// that are referenced by backrefs.
    pub fn parse(re: &str) -> Result<(Expr, BitSet)> {
        Parser::parse(re)
    }

    /// Convert expression to a regex string in the regex crate's syntax.
    ///
    /// # Panics
    ///
    /// Panics for expressions that are hard, i.e. can not be handled by the regex crate.
    pub fn to_str(&self, buf: &mut String, precedence: u8) {
        match *self {
            Expr::Empty => (),
            Expr::Any { newline } => buf.push_str(if newline { "(?s:.)" } else { "." }),
            Expr::Literal { ref val, casei } => {
                if casei {
                    buf.push_str("(?i:");
                }
                push_quoted(buf, val);
                if casei {
                    buf.push_str(")");
                }
            }
            Expr::StartText => buf.push('^'),
            Expr::EndText => buf.push('$'),
            Expr::StartLine => buf.push_str("(?m:^)"),
            Expr::EndLine => buf.push_str("(?m:$)"),
            Expr::Concat(ref children) => {
                if precedence > 1 {
                    buf.push_str("(?:");
                }
                for child in children {
                    child.to_str(buf, 2);
                }
                if precedence > 1 {
                    buf.push(')')
                }
            }
            Expr::Alt(ref children) => {
                if precedence > 0 {
                    buf.push_str("(?:");
                }

                let is_empty = |e| match e {
                    &Expr::Empty => true,
                    _ => false,
                };
                let contains_empty = children.iter().any(&is_empty);
                if contains_empty {
                    buf.push_str("(?:");
                }
                for (i, child) in children.iter().filter(|&c| !is_empty(c)).enumerate() {
                    if i != 0 {
                        buf.push('|');
                    }
                    child.to_str(buf, 1);
                }
                if contains_empty {
                    // regex fails with `(a|b|)`, so transform to `((?:a|b)?)`
                    buf.push_str(")?");
                }

                if precedence > 0 {
                    buf.push(')');
                }
            }
            Expr::Group(ref child) => {
                buf.push('(');
                child.to_str(buf, 0);
                buf.push(')');
            }
            Expr::Repeat {
                ref child,
                lo,
                hi,
                greedy,
            } => {
                if precedence > 2 {
                    buf.push_str("(?:");
                }
                child.to_str(buf, 3);
                match (lo, hi) {
                    (0, 1) => buf.push('?'),
                    (0, usize::MAX) => buf.push('*'),
                    (1, usize::MAX) => buf.push('+'),
                    (lo, hi) => {
                        buf.push('{');
                        push_usize(buf, lo);
                        if lo != hi {
                            buf.push(',');
                            if hi != usize::MAX {
                                push_usize(buf, hi);
                            }
                        }
                        buf.push('}');
                    }
                }
                if !greedy {
                    buf.push('?');
                }
                if precedence > 2 {
                    buf.push(')');
                }
            }
            Expr::Delegate {
                ref inner, casei, ..
            } => {
                // at the moment, delegate nodes are just atoms
                if casei {
                    buf.push_str("(?i:");
                }
                buf.push_str(inner);
                if casei {
                    buf.push_str(")");
                }
            }
            _ => panic!("attempting to format hard expr"),
        }
    }
}

// precondition: ix > 0
fn prev_codepoint_ix(s: &str, mut ix: usize) -> usize {
    let bytes = s.as_bytes();
    loop {
        ix -= 1;
        // fancy bit magic for ranges 0..0x80 + 0xc0..
        if (bytes[ix] as i8) >= -0x40 {
            break;
        }
    }
    ix
}

fn codepoint_len(b: u8) -> usize {
    match b {
        b if b < 0x80 => 1,
        b if b < 0xe0 => 2,
        b if b < 0xf0 => 3,
        _ => 4,
    }
}

// If this returns false, then there is no possible backref in the re

// Both potential implementations are turned off, because we currently
// always need to do a deeper analysis because of 1-character
// look-behind. If we could call a find_from_pos method of regex::Regex,
// it would make sense to bring this back.
/*
pub fn detect_possible_backref(re: &str) -> bool {
    let mut last = b'\x00';
    for b in re.as_bytes() {
        if b'0' <= *b && *b <= b'9' && last == b'\\' { return true; }
        last = *b;
    }
    false
}

pub fn detect_possible_backref(re: &str) -> bool {
    let mut bytes = re.as_bytes();
    loop {
        match memchr::memchr(b'\\', &bytes[..bytes.len() - 1]) {
            Some(i) => {
                bytes = &bytes[i + 1..];
                let c = bytes[0];
                if b'0' <= c && c <= b'9' { return true; }
            }
            None => return false
        }
    }
}
*/

/// The internal module only exists so that the toy example can access internals for debugging and
/// experimenting.
#[doc(hidden)]
pub mod internal {
    pub use crate::analyze::analyze;
    pub use crate::compile::compile;
    pub use crate::vm::{trace, Insn, Prog};
}

#[cfg(test)]
mod tests {
    use crate::parse::make_literal;
    use crate::Expr;
    use crate::Regex;
    use std::usize;
    //use detect_possible_backref;

    // tests for to_str

    fn to_str(e: Expr) -> String {
        let mut s = String::new();
        e.to_str(&mut s, 0);
        s
    }

    #[test]
    fn to_str_concat_alt() {
        let e = Expr::Concat(vec![
            Expr::Alt(vec![make_literal("a"), make_literal("b")]),
            make_literal("c"),
        ]);
        assert_eq!(to_str(e), "(?:a|b)c");
    }

    #[test]
    fn to_str_rep_concat() {
        let e = Expr::Repeat {
            child: Box::new(Expr::Concat(vec![make_literal("a"), make_literal("b")])),
            lo: 2,
            hi: 3,
            greedy: true,
        };
        assert_eq!(to_str(e), "(?:ab){2,3}");
    }

    #[test]
    fn to_str_group_alt() {
        let e = Expr::Group(Box::new(Expr::Alt(vec![
            make_literal("a"),
            make_literal("b"),
        ])));
        assert_eq!(to_str(e), "(a|b)");
    }

    #[test]
    fn as_str_debug() {
        let s = r"(a+)b\1";
        let regex = Regex::new(s).unwrap();
        assert_eq!(s, regex.as_str());
        assert_eq!(s, format!("{:?}", regex));
    }

    #[test]
    fn to_str_repeat() {
        fn repeat(lo: usize, hi: usize, greedy: bool) -> Expr {
            Expr::Repeat {
                child: Box::new(make_literal("a")),
                lo,
                hi,
                greedy,
            }
        }

        assert_eq!(to_str(repeat(2, 2, true)), "a{2}");
        assert_eq!(to_str(repeat(2, 2, false)), "a{2}?");
        assert_eq!(to_str(repeat(2, 3, true)), "a{2,3}");
        assert_eq!(to_str(repeat(2, 3, false)), "a{2,3}?");
        assert_eq!(to_str(repeat(2, usize::MAX, true)), "a{2,}");
        assert_eq!(to_str(repeat(2, usize::MAX, false)), "a{2,}?");
        assert_eq!(to_str(repeat(0, 1, true)), "a?");
        assert_eq!(to_str(repeat(0, 1, false)), "a??");
        assert_eq!(to_str(repeat(0, usize::MAX, true)), "a*");
        assert_eq!(to_str(repeat(0, usize::MAX, false)), "a*?");
        assert_eq!(to_str(repeat(1, usize::MAX, true)), "a+");
        assert_eq!(to_str(repeat(1, usize::MAX, false)), "a+?");
    }

    /*
    #[test]
    fn detect_backref() {
        assert_eq!(detect_possible_backref("a0a1a2"), false);
        assert_eq!(detect_possible_backref("a0a1\\a2"), false);
        assert_eq!(detect_possible_backref("a0a\\1a2"), true);
        assert_eq!(detect_possible_backref("a0a1a2\\"), false);
    }
    */
}
