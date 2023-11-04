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
and lookaround.

It builds on top of the excellent [regex] crate. If you are not
familiar with it, make sure you read its documentation and maybe you don't even need fancy-regex.

If your regex or parts of it does not use any special features, the matching is delegated to the
regex crate. That means it has linear runtime. But if you use "fancy" features such as
backreferences or look-around, an engine with backtracking needs to be used. In that case, the regex
can be slow and take exponential time to run because of what is called "catastrophic backtracking".
This depends on the regex and the input.

# Usage

The API should feel very similar to the regex crate, and involves compiling a regex and then using
it to find matches in text.

## Example: Matching text

An example with backreferences to check if a text consists of two identical words:

```rust
use fancy_regex::Regex;

let re = Regex::new(r"^(\w+) (\1)$").unwrap();
let result = re.is_match("foo foo");

assert!(result.is_ok());
let did_match = result.unwrap();
assert!(did_match);
```

Note that like in the regex crate, the regex needs anchors like `^` and `$` to match against the
entire input text.

## Example: Finding the position of matches

```rust
use fancy_regex::Regex;

let re = Regex::new(r"(\d)\1").unwrap();
let result = re.find("foo 22");

assert!(result.is_ok(), "execution was successful");
let match_option = result.unwrap();

assert!(match_option.is_some(), "found a match");
let m = match_option.unwrap();

assert_eq!(m.start(), 4);
assert_eq!(m.end(), 6);
assert_eq!(m.as_str(), "22");
```

## Example: Capturing groups

```rust
use fancy_regex::Regex;

let re = Regex::new(r"(?<!AU)\$(\d+)").unwrap();
let result = re.captures("AU$10, $20");

let captures = result.expect("Error running regex").expect("No match found");
let group = captures.get(1).expect("No group");
assert_eq!(group.as_str(), "20");
```

# Syntax

The regex syntax is based on the [regex] crate's, with some additional supported syntax.

Escapes:

`\h`
: hex digit (`[0-9A-Fa-f]`) \
`\H`
: not hex digit (`[^0-9A-Fa-f]`) \
`\e`
: escape control character (`\x1B`) \
`\K`
: keep text matched so far out of the overall match ([docs](https://www.regular-expressions.info/keep.html))\
`\G`
: anchor to where the previous match ended ([docs](https://www.regular-expressions.info/continue.html))

Backreferences:

`\1`
: match the exact string that the first capture group matched \
`\2`
: backref to the second capture group, etc

Named capture groups:

`(?<name>exp)`
: match *exp*, creating capture group named *name* \
`\k<name>`
: match the exact string that the capture group named *name* matched \
`(?P<name>exp)`
: same as `(?<name>exp)` for compatibility with Python, etc. \
`(?P=name)`
: same as `\k<name>` for compatibility with Python, etc.

Look-around assertions for matching without changing the current position:

`(?=exp)`
: look-ahead, succeeds if *exp* matches to the right of the current position \
`(?!exp)`
: negative look-ahead, succeeds if *exp* doesn't match to the right \
`(?<=exp)`
: look-behind, succeeds if *exp* matches to the left of the current position \
`(?<!exp)`
: negative look-behind, succeeds if *exp* doesn't match to the left

Atomic groups using `(?>exp)` to prevent backtracking within `exp`, e.g.:

```
# use fancy_regex::Regex;
let re = Regex::new(r"^a(?>bc|b)c$").unwrap();
assert!(re.is_match("abcc").unwrap());
// Doesn't match because `|b` is never tried because of the atomic group
assert!(!re.is_match("abc").unwrap());
```

Conditionals - if/then/else:

`(?(1))`
: continue only if first capture group matched \
`(?(<name>))`
: continue only if capture group named *name* matched \
`(?(1)true_branch|false_branch)`
: if the first capture group matched then execute the true_branch regex expression, else execute false_branch ([docs](https://www.regular-expressions.info/conditional.html)) \
`(?(condition)true_branch|false_branch)`
: if the condition matches then execute the true_branch regex expression, else execute false_branch from the point just before the condition was evaluated

[regex]: https://crates.io/crates/regex
*/

#![doc(html_root_url = "https://docs.rs/fancy-regex/0.11.0")]
#![deny(missing_docs)]
#![deny(missing_debug_implementations)]

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use spin::mutex::{SpinMutex, SpinMutexGuard};
use std::borrow::Cow;
use std::convert::TryInto;
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::ops::{Deref, DerefMut, Index, Range};
use std::str::FromStr;

mod analyze;
mod compile;
mod error;
mod expand;
mod parse;
mod replacer;
mod vm;

use crate::analyze::analyze;
use crate::compile::compile_with_options;
pub use crate::parse::ExprTree;
use crate::parse::NamedGroups;
use crate::vm::{Prog, OPTION_SKIPPED_EMPTY_MATCH};

pub use crate::error::{CompileError, Error, ParseError, Result, RuntimeError};
pub use crate::expand::Expander;
pub use crate::replacer::{NoExpand, Replacer, ReplacerRef};

const MAX_RECURSION: usize = 64;

// the public API

/// A builder for a `Regex` to allow configuring options.
#[derive(Copy, Clone, Debug)]
pub struct RegexBuilder(RegexOptions);

/// A compiled regular expression.
#[derive(Clone)]
pub struct Regex {
    pattern: Option<String>,
    tree: ExprTree,
    prog: Prog,
    n_groups: usize,
    options: RegexOptions,
    saves: SpinCache<Vec<usize>>,
}

#[derive(Debug)]
struct SpinCache<T>(SpinMutex<T>);

impl<T: Default> Clone for SpinCache<T> {
    fn clone(&self) -> Self {
        SpinCache(SpinMutex::new(T::default()))
    }
}

impl<T> AsRef<SpinMutex<T>> for SpinCache<T> {
    fn as_ref(&self) -> &SpinMutex<T> {
        self
    }
}

impl<T> AsMut<SpinMutex<T>> for SpinCache<T> {
    fn as_mut(&mut self) -> &mut SpinMutex<T> {
        self
    }
}

impl<T> Deref for SpinCache<T> {
    type Target = SpinMutex<T>;
    fn deref(&self) -> &SpinMutex<T> {
        &self.0
    }
}

impl<T> DerefMut for SpinCache<T> {
    fn deref_mut(&mut self) -> &mut SpinMutex<T> {
        &mut self.0
    }
}

impl<T: Default> Default for SpinCache<T> {
    fn default() -> Self {
        SpinCache(SpinMutex::new(T::default()))
    }
}

/// A single match of a regex or group in an input text
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Match<'t> {
    text: &'t str,
    start: usize,
    end: usize,
}

/// An iterator over all non-overlapping matches for a particular string.
///
/// The iterator yields a `Result<Match>`. The iterator stops when no more
/// matches can be found.
///
/// `'r` is the lifetime of the compiled regular expression and `'t` is the
/// lifetime of the matched string.
#[derive(Debug)]
pub struct Matches<'r, 't> {
    re: &'r Regex,
    text: &'t str,
    last_end: usize,
    last_match: Option<usize>,
}

impl<'r, 't> Matches<'r, 't> {
    /// Return the text being searched.
    pub fn text(&self) -> &'t str {
        self.text
    }

    /// Return the underlying regex.
    pub fn regex(&self) -> &'r Regex {
        &self.re
    }
}

impl<'r, 't> Iterator for Matches<'r, 't> {
    type Item = Result<Match<'t>>;

    /// Adapted from the `regex` crate. Calls `find_from_pos` repeatedly.
    /// Ignores empty matches immediately after a match.
    fn next(&mut self) -> Option<Self::Item> {
        if self.last_end > self.text.len() {
            return None;
        }

        let option_flags = if let Some(last_match) = self.last_match {
            if self.last_end > last_match {
                OPTION_SKIPPED_EMPTY_MATCH
            } else {
                0
            }
        } else {
            0
        };
        let mat =
            match self
                .re
                .find_from_pos_with_option_flags(self.text, self.last_end, option_flags)
            {
                Err(error) => return Some(Err(error)),
                Ok(None) => return None,
                Ok(Some(mat)) => mat,
            };

        if mat.start == mat.end {
            // This is an empty match. To ensure we make progress, start
            // the next search at the smallest possible starting position
            // of the next match following this one.
            self.last_end = next_utf8(self.text, mat.end);
            // Don't accept empty matches immediately following a match.
            // Just move on to the next match.
            if Some(mat.end) == self.last_match {
                return self.next();
            }
        } else {
            self.last_end = mat.end;
        }

        self.last_match = Some(mat.end);

        Some(Ok(mat))
    }
}

/// An iterator that yields all non-overlapping capture groups matching a
/// particular regular expression.
///
/// The iterator stops when no more matches can be found.
///
/// `'r` is the lifetime of the compiled regular expression and `'t` is the
/// lifetime of the matched string.
#[derive(Debug)]
pub struct CaptureMatches<'r, 't>(Matches<'r, 't>);

impl<'r, 't> CaptureMatches<'r, 't> {
    /// Return the text being searched.
    pub fn text(&self) -> &'t str {
        self.0.text
    }

    /// Return the underlying regex.
    pub fn regex(&self) -> &'r Regex {
        &self.0.re
    }
}

impl<'r, 't> Iterator for CaptureMatches<'r, 't> {
    type Item = Result<Captures<'r, 't>>;

    /// Adapted from the `regex` crate. Calls `captures_from_pos` repeatedly.
    /// Ignores empty matches immediately after a match.
    fn next(&mut self) -> Option<Self::Item> {
        if self.0.last_end > self.0.text.len() {
            return None;
        }

        let captures = match self.0.re.captures_from_pos(self.0.text, self.0.last_end) {
            Err(error) => return Some(Err(error)),
            Ok(None) => return None,
            Ok(Some(captures)) => captures,
        };

        let mat = captures
            .get(0)
            .expect("`Captures` is expected to have entire match at 0th position");
        if mat.start == mat.end {
            self.0.last_end = next_utf8(self.0.text, mat.end);
            if Some(mat.end) == self.0.last_match {
                return self.next();
            }
        } else {
            self.0.last_end = mat.end;
        }

        self.0.last_match = Some(mat.end);

        Some(Ok(captures))
    }
}

/// A set of capture groups found for a regex.
#[derive(Debug)]
pub struct Captures<'r, 't> {
    text: &'t str,
    saves: CowSpin<'r, Vec<usize>>,
    named_groups: &'r NamedGroups,
}

#[derive(Debug)]
enum CowSpin<'r, T> {
    Borrowed(SpinMutexGuard<'r, T>),
    Owned(T),
}

impl<T> AsRef<T> for CowSpin<'_, T> {
    fn as_ref(&self) -> &T {
        self
    }
}

impl<T> AsMut<T> for CowSpin<'_, T> {
    fn as_mut(&mut self) -> &mut T {
        self
    }
}

impl<T> Deref for CowSpin<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        match self {
            CowSpin::Borrowed(borrowed) => borrowed.deref(),
            CowSpin::Owned(owned) => owned,
        }
    }
}

impl<T> DerefMut for CowSpin<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        match self {
            CowSpin::Borrowed(borrowed) => borrowed.deref_mut(),
            CowSpin::Owned(owned) => owned,
        }
    }
}

/// Iterator for captured groups in order in which they appear in the regex.
#[derive(Debug)]
pub struct SubCaptureMatches<'r, 't> {
    caps: &'r Captures<'r, 't>,
    i: usize,
}

#[derive(Clone, Debug)]
enum RegexSource {
    Pattern(String),
    ExprTree(ExprTree),
}

#[derive(Copy, Clone, Debug)]
struct RegexOptions {
    backtrack_limit: usize,
    delegate_size_limit: Option<usize>,
    delegate_dfa_size_limit: Option<usize>,
}

impl Default for RegexOptions {
    fn default() -> Self {
        RegexOptions {
            backtrack_limit: DEFAULT_BACKTRACK_LIMIT,
            delegate_size_limit: None,
            delegate_dfa_size_limit: None,
        }
    }
}

const DEFAULT_BACKTRACK_LIMIT: usize = 1_000_000;

impl RegexBuilder {
    /// Create a new regex builder with a regex pattern.
    ///
    /// If the pattern is invalid, the call to `build` will fail later.
    pub fn new() -> Self {
        let builder = RegexBuilder(RegexOptions::default());
        builder
    }

    /// Build the `Regex`.
    ///
    /// Returns an [`Error`](enum.Error.html) if the pattern could not be parsed.
    pub fn build(&self, pattern: impl Into<String>) -> Result<Regex> {
        Regex::new_with_source_and_options(RegexSource::Pattern(pattern.into()), self.0)
    }

    /// Build the `Regex` from `Expr`
    pub fn build_from_expr_tree(&self, expr_tree: impl Into<ExprTree>) -> Result<Regex> {
        Regex::new_with_source_and_options(RegexSource::ExprTree(expr_tree.into()), self.0)
    }

    /// Limit for how many times backtracking should be attempted for fancy regexes (where
    /// backtracking is used). If this limit is exceeded, execution returns an error with
    /// [`Error::BacktrackLimitExceeded`](enum.Error.html#variant.BacktrackLimitExceeded).
    /// This is for preventing a regex with catastrophic backtracking to run for too long.
    ///
    /// Default is `1_000_000` (1 million).
    pub fn backtrack_limit(&mut self, limit: usize) -> &mut Self {
        self.0.backtrack_limit = limit;
        self
    }

    /// Set the approximate size limit of the compiled regular expression.
    ///
    /// This option is forwarded from the wrapped `regex` crate. Note that depending on the used
    /// regex features there may be multiple delegated sub-regexes fed to the `regex` crate. As
    /// such the actual limit is closer to `<number of delegated regexes> * delegate_size_limit`.
    pub fn delegate_size_limit(&mut self, limit: usize) -> &mut Self {
        self.0.delegate_size_limit = Some(limit);
        self
    }

    /// Set the approximate size of the cache used by the DFA.
    ///
    /// This option is forwarded from the wrapped `regex` crate. Note that depending on the used
    /// regex features there may be multiple delegated sub-regexes fed to the `regex` crate. As
    /// such the actual limit is closer to `<number of delegated regexes> *
    /// delegate_dfa_size_limit`.
    pub fn delegate_dfa_size_limit(&mut self, limit: usize) -> &mut Self {
        self.0.delegate_dfa_size_limit = Some(limit);
        self
    }
}

impl fmt::Debug for Regex {
    /// Shows the original regular expression.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl fmt::Display for Regex {
    /// Shows the original regular expression
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl FromStr for Regex {
    type Err = Error;

    /// Attempts to parse a string into a regular expression
    fn from_str(s: &str) -> Result<Regex> {
        Regex::new(s)
    }
}

impl Regex {
    /// Parse and compile a regex with default options, see `RegexBuilder`.
    ///
    /// Returns an [`Error`](enum.Error.html) if the pattern could not be parsed.
    pub fn new(re: impl Into<String>) -> Result<Regex> {
        RegexBuilder::new().build(re)
    }

    fn new_with_source_and_options(source: RegexSource, options: RegexOptions) -> Result<Regex> {
        let (raw_tree, pattern) = match source {
            RegexSource::Pattern(pattern) => (Expr::parse_tree(pattern.as_str())?, Some(pattern)),
            RegexSource::ExprTree(tree) => (tree, None),
        };

        // wrapper to search for re at arbitrary start position,
        // and to capture the match bounds
        let tree = ExprTree {
            expr: Expr::Concat(vec![
                Expr::Repeat {
                    child: Box::new(Expr::Any { newline: true }),
                    lo: 0,
                    hi: usize::MAX,
                    greedy: false,
                },
                Expr::Group(Box::new(raw_tree.expr)),
            ]),
            ..raw_tree
        };

        let info = analyze(&tree)?;
        debug_assert_eq!(info.hard, info.children[1].children[0].hard);

        let prog = compile_with_options(
            &info,
            RegexOptions {
                backtrack_limit: options.backtrack_limit,
                delegate_dfa_size_limit: options.delegate_dfa_size_limit,
                delegate_size_limit: options.delegate_size_limit,
            },
        )?;
        let n_groups = info.end_group;

        let raw_tree = ExprTree {
            expr: match tree.expr {
                Expr::Concat(children) => match children.into_iter().last() {
                    Some(Expr::Group(raw_expr)) => *raw_expr,
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            ..tree
        };

        Ok(Regex {
            n_groups,
            pattern,
            tree: raw_tree,
            prog,
            options,
            saves: SpinCache::default(),
        })
    }

    /// Returns the original string of this regex.
    pub fn as_str(&self) -> &str {
        self.pattern
            .as_ref()
            .expect("cannot get pattern as this regex is built from expr tree")
            .as_str()
    }

    /// Returns the expr tree of this regex.
    pub fn as_expr_tree(&self) -> &ExprTree {
        &self.tree
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
        let Regex {
            ref prog, options, ..
        } = self;
        let result = vm::run(
            prog,
            text,
            0..text.len(),
            0,
            options.backtrack_limit,
            Some(0),
        )?;
        Ok(result.is_some())
    }

    /// Returns an iterator for each successive non-overlapping match in `text`.
    ///
    /// If you have capturing groups in your regex that you want to extract, use the [Regex::captures_iter()]
    /// method.
    ///
    /// # Example
    ///
    /// Find all words followed by an exclamation point:
    ///
    /// ```rust
    /// # use fancy_regex::Regex;
    ///
    /// let re = Regex::new(r"\w+(?=!)").unwrap();
    /// let mut matches = re.find_iter("so fancy! even with! iterators!");
    /// assert_eq!(matches.next().unwrap().unwrap().as_str(), "fancy");
    /// assert_eq!(matches.next().unwrap().unwrap().as_str(), "with");
    /// assert_eq!(matches.next().unwrap().unwrap().as_str(), "iterators");
    /// assert!(matches.next().is_none());
    /// ```
    pub fn find_iter<'r, 't>(&'r self, text: &'t str) -> Matches<'r, 't> {
        Matches {
            re: &self,
            text,
            last_end: 0,
            last_match: None,
        }
    }

    /// Find the first match in the input text.
    ///
    /// If you have capturing groups in your regex that you want to extract, use the [Regex::captures()]
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
        self.find_from_pos(text, 0)
    }

    /// Returns the first match in `text`, starting from the specified byte position `pos`.
    ///
    /// # Examples
    ///
    /// Finding match starting at a position:
    ///
    /// ```
    /// # use fancy_regex::Regex;
    /// let re = Regex::new(r"(?m:^)(\d+)").unwrap();
    /// let text = "1 test 123\n2 foo";
    /// let mat = re.find_from_pos(text, 7).unwrap().unwrap();
    ///
    /// assert_eq!(mat.start(), 11);
    /// assert_eq!(mat.end(), 12);
    /// ```
    ///
    /// Note that in some cases this is not the same as using the `find`
    /// method and passing a slice of the string, see [Regex::captures_from_pos()] for details.
    pub fn find_from_pos<'t>(&self, text: &'t str, pos: usize) -> Result<Option<Match<'t>>> {
        self.find_from_pos_with_option_flags(text, pos, 0)
    }

    fn find_from_pos_with_option_flags<'t>(
        &self,
        text: &'t str,
        pos: usize,
        option_flags: u32,
    ) -> Result<Option<Match<'t>>> {
        let Regex { prog, options, .. } = self;
        let result = vm::run(
            prog,
            text,
            pos..text.len(),
            option_flags,
            options.backtrack_limit,
            Some(1),
        )?;
        Ok(result.map(|saves| Match::new(text, saves[0], saves[1])))
    }

    /// Returns an iterator over all the non-overlapping capture groups matched in `text`.
    ///
    /// # Examples
    ///
    /// Finding all matches and capturing parts of each:
    ///
    /// ```rust
    /// # use fancy_regex::Regex;
    ///
    /// let re = Regex::new(r"(\d{4})-(\d{2})").unwrap();
    /// let text = "It was between 2018-04 and 2020-01";
    /// let mut all_captures = re.captures_iter(text);
    ///
    /// let first = all_captures.next().unwrap().unwrap();
    /// assert_eq!(first.get(1).unwrap().as_str(), "2018");
    /// assert_eq!(first.get(2).unwrap().as_str(), "04");
    /// assert_eq!(first.get(0).unwrap().as_str(), "2018-04");
    ///
    /// let second = all_captures.next().unwrap().unwrap();
    /// assert_eq!(second.get(1).unwrap().as_str(), "2020");
    /// assert_eq!(second.get(2).unwrap().as_str(), "01");
    /// assert_eq!(second.get(0).unwrap().as_str(), "2020-01");
    ///
    /// assert!(all_captures.next().is_none());
    /// ```
    pub fn captures_iter<'r, 't>(&'r self, text: &'t str) -> CaptureMatches<'r, 't> {
        CaptureMatches(self.find_iter(text))
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
    pub fn captures<'r, 't>(&'r self, text: &'t str) -> Result<Option<Captures<'r, 't>>> {
        self.captures_from_pos(text, 0)
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
    /// method and passing a slice of the string, see the capture that we get
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
    pub fn captures_from_pos<'r, 't>(
        &'r self,
        text: &'t str,
        pos: usize,
    ) -> Result<Option<Captures<'r, 't>>> {
        self.captures_within_range(text, pos..text.len())
    }

    /// Returns the capture groups for the first match in `text`
    /// within the specified byte range `range`.
    pub fn captures_within_range<'r, 't>(
        &'r self,
        text: &'t str,
        range: Range<usize>,
    ) -> Result<Option<Captures<'r, 't>>> {
        let named_groups = &self.tree.named_groups;
        let Regex {
            prog,
            n_groups,
            options,
            saves: locations,
            ..
        } = self;
        let mut saves = locations
            .try_lock()
            .map_or_else(|| CowSpin::Owned(Vec::new()), CowSpin::Borrowed);
        saves.clear();
        let result = vm::run_to(
            &mut saves,
            prog,
            text,
            range,
            0,
            options.backtrack_limit,
            Some(*n_groups),
        )?;
        Ok(result.then(|| Captures {
            text,
            saves,
            named_groups,
        }))
    }

    /// Returns the number of captures, including the implicit capture of the entire expression.
    pub fn captures_len(&self) -> usize {
        self.n_groups
    }

    /// Returns an iterator over the capture names.
    pub fn capture_names(&self) -> CaptureNames {
        let mut names = Vec::new();
        names.resize(self.captures_len(), None);
        for (name, &i) in self.tree.named_groups.iter() {
            names[i] = Some(name.as_str());
        }
        CaptureNames(names.into_iter())
    }

    // for debugging only
    #[doc(hidden)]
    pub fn debug_print(&self) {
        self.prog.debug_print()
    }

    /// Replaces the leftmost-first match with the replacement provided.
    /// The replacement can be a regular string (where `$N` and `$name` are
    /// expanded to match capture groups) or a function that takes the matches'
    /// `Captures` and returns the replaced string.
    ///
    /// If no match is found, then a copy of the string is returned unchanged.
    ///
    /// # Replacement string syntax
    ///
    /// All instances of `$name` in the replacement text is replaced with the
    /// corresponding capture group `name`.
    ///
    /// `name` may be an integer corresponding to the index of the
    /// capture group (counted by order of opening parenthesis where `0` is the
    /// entire match) or it can be a name (consisting of letters, digits or
    /// underscores) corresponding to a named capture group.
    ///
    /// If `name` isn't a valid capture group (whether the name doesn't exist
    /// or isn't a valid index), then it is replaced with the empty string.
    ///
    /// The longest possible name is used. e.g., `$1a` looks up the capture
    /// group named `1a` and not the capture group at index `1`. To exert more
    /// precise control over the name, use braces, e.g., `${1}a`.
    ///
    /// To write a literal `$` use `$$`.
    ///
    /// # Examples
    ///
    /// Note that this function is polymorphic with respect to the replacement.
    /// In typical usage, this can just be a normal string:
    ///
    /// ```rust
    /// # use fancy_regex::Regex;
    /// let re = Regex::new("[^01]+").unwrap();
    /// assert_eq!(re.replace("1078910", ""), "1010");
    /// ```
    ///
    /// But anything satisfying the `Replacer` trait will work. For example,
    /// a closure of type `|&Captures| -> String` provides direct access to the
    /// captures corresponding to a match. This allows one to access
    /// capturing group matches easily:
    ///
    /// ```rust
    /// # use fancy_regex::{Regex, Captures};
    /// let re = Regex::new(r"([^,\s]+),\s+(\S+)").unwrap();
    /// let result = re.replace("Springsteen, Bruce", |caps: &Captures| {
    ///     format!("{} {}", &caps[2], &caps[1])
    /// });
    /// assert_eq!(result, "Bruce Springsteen");
    /// ```
    ///
    /// But this is a bit cumbersome to use all the time. Instead, a simple
    /// syntax is supported that expands `$name` into the corresponding capture
    /// group. Here's the last example, but using this expansion technique
    /// with named capture groups:
    ///
    /// ```rust
    /// # use fancy_regex::Regex;
    /// let re = Regex::new(r"(?P<last>[^,\s]+),\s+(?P<first>\S+)").unwrap();
    /// let result = re.replace("Springsteen, Bruce", "$first $last");
    /// assert_eq!(result, "Bruce Springsteen");
    /// ```
    ///
    /// Note that using `$2` instead of `$first` or `$1` instead of `$last`
    /// would produce the same result. To write a literal `$` use `$$`.
    ///
    /// Sometimes the replacement string requires use of curly braces to
    /// delineate a capture group replacement and surrounding literal text.
    /// For example, if we wanted to join two words together with an
    /// underscore:
    ///
    /// ```rust
    /// # use fancy_regex::Regex;
    /// let re = Regex::new(r"(?P<first>\w+)\s+(?P<second>\w+)").unwrap();
    /// let result = re.replace("deep fried", "${first}_$second");
    /// assert_eq!(result, "deep_fried");
    /// ```
    ///
    /// Without the curly braces, the capture group name `first_` would be
    /// used, and since it doesn't exist, it would be replaced with the empty
    /// string.
    ///
    /// Finally, sometimes you just want to replace a literal string with no
    /// regard for capturing group expansion. This can be done by wrapping a
    /// byte string with `NoExpand`:
    ///
    /// ```rust
    /// # use fancy_regex::Regex;
    /// use fancy_regex::NoExpand;
    ///
    /// let re = Regex::new(r"(?P<last>[^,\s]+),\s+(\S+)").unwrap();
    /// let result = re.replace("Springsteen, Bruce", NoExpand("$2 $last"));
    /// assert_eq!(result, "$2 $last");
    /// ```
    pub fn replace<'t, R: Replacer>(&self, text: &'t str, rep: R) -> Cow<'t, str> {
        self.replacen(text, 1, rep)
    }

    /// Replaces all non-overlapping matches in `text` with the replacement
    /// provided. This is the same as calling `replacen` with `limit` set to
    /// `0`.
    ///
    /// See the documentation for `replace` for details on how to access
    /// capturing group matches in the replacement string.
    pub fn replace_all<'t, R: Replacer>(&self, text: &'t str, rep: R) -> Cow<'t, str> {
        self.replacen(text, 0, rep)
    }

    /// Replaces at most `limit` non-overlapping matches in `text` with the
    /// replacement provided. If `limit` is 0, then all non-overlapping matches
    /// are replaced.
    ///
    /// See the documentation for `replace` for details on how to access
    /// capturing group matches in the replacement string.
    pub fn replacen<'t, R: Replacer>(
        &self,
        text: &'t str,
        limit: usize,
        mut rep: R,
    ) -> Cow<'t, str> {
        // If we know that the replacement doesn't have any capture expansions,
        // then we can fast path. The fast path can make a tremendous
        // difference:
        //
        //   1) We use `find_iter` instead of `captures_iter`. Not asking for
        //      captures generally makes the regex engines faster.
        //   2) We don't need to look up all of the capture groups and do
        //      replacements inside the replacement string. We just push it
        //      at each match and be done with it.
        if let Some(rep) = rep.no_expansion() {
            let mut it = self.find_iter(text).enumerate().peekable();
            if it.peek().is_none() {
                return Cow::Borrowed(text);
            }
            let mut new = String::with_capacity(text.len());
            let mut last_match = 0;
            for (i, m) in it {
                let m = m.unwrap();
                if limit > 0 && i >= limit {
                    break;
                }
                new.push_str(&text[last_match..m.start()]);
                new.push_str(&rep);
                last_match = m.end();
            }
            new.push_str(&text[last_match..]);
            return Cow::Owned(new);
        }

        // The slower path, which we use if the replacement needs access to
        // capture groups.
        let mut it = self.captures_iter(text).enumerate().peekable();
        if it.peek().is_none() {
            return Cow::Borrowed(text);
        }
        let mut new = String::with_capacity(text.len());
        let mut last_match = 0;
        for (i, cap) in it {
            let cap = cap.unwrap();
            if limit > 0 && i >= limit {
                break;
            }
            // unwrap on 0 is OK because captures only reports matches
            let m = cap.get(0).unwrap();
            new.push_str(&text[last_match..m.start()]);
            rep.replace_append(&cap, &mut new);
            last_match = m.end();
        }
        new.push_str(&text[last_match..]);
        Cow::Owned(new)
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

    /// Returns the range over the starting and ending byte offsets of the match in text.
    #[inline]
    pub fn range(&self) -> Range<usize> {
        self.start..self.end
    }

    /// Returns the matched text.
    #[inline]
    pub fn as_str(&self) -> &'t str {
        &self.text[self.start..self.end]
    }

    /// Creates a new match from the given text and byte offsets.
    fn new(text: &'t str, start: usize, end: usize) -> Match<'t> {
        Match { text, start, end }
    }
}

impl<'t> From<Match<'t>> for &'t str {
    fn from(m: Match<'t>) -> &'t str {
        m.as_str()
    }
}

impl<'t> From<Match<'t>> for Range<usize> {
    fn from(m: Match<'t>) -> Range<usize> {
        m.range()
    }
}

#[allow(clippy::len_without_is_empty)] // follow regex's API
impl<'r, 't> Captures<'r, 't> {
    /// Get the capture group by its index in the regex.
    ///
    /// If there is no match for that group or the index does not correspond to a group, `None` is
    /// returned. The index 0 returns the whole match.
    pub fn get(&self, i: usize) -> Option<Match<'t>> {
        let Captures {
            text, ref saves, ..
        } = self;
        let slot = i * 2;
        if slot >= saves.len() {
            return None;
        }
        let lo = saves[slot];
        if lo == std::usize::MAX {
            return None;
        }
        let hi = saves[slot + 1];
        Some(Match {
            text,
            start: lo,
            end: hi,
        })
    }

    /// Returns the match for a named capture group.  Returns `None` the capture
    /// group did not match or if there is no group with the given name.
    pub fn name(&self, name: &str) -> Option<Match<'t>> {
        self.named_groups.get(name).and_then(|i| self.get(*i))
    }

    /// Expands all instances of `$group` in `replacement` to the corresponding
    /// capture group `name`, and writes them to the `dst` buffer given.
    ///
    /// `group` may be an integer corresponding to the index of the
    /// capture group (counted by order of opening parenthesis where `\0` is the
    /// entire match) or it can be a name (consisting of letters, digits or
    /// underscores) corresponding to a named capture group.
    ///
    /// If `group` isn't a valid capture group (whether the name doesn't exist
    /// or isn't a valid index), then it is replaced with the empty string.
    ///
    /// The longest possible name is used. e.g., `$1a` looks up the capture
    /// group named `1a` and not the capture group at index `1`. To exert more
    /// precise control over the name, use braces, e.g., `${1}a`.
    ///
    /// To write a literal `$`, use `$$`.
    ///
    /// For more control over expansion, see [`Expander`].
    ///
    /// [`Expander`]: expand/struct.Expander.html
    pub fn expand(&self, replacement: &str, dst: &mut String) {
        Expander::default().append_expansion(dst, replacement, self);
    }

    /// Iterate over the captured groups in order in which they appeared in the regex. The first
    /// capture corresponds to the whole match.
    pub fn iter<'c>(&'c self) -> SubCaptureMatches<'c, 't> {
        SubCaptureMatches { caps: self, i: 0 }
    }

    /// How many groups were captured. This is always at least 1 because group 0 returns the whole
    /// match.
    pub fn len(&self) -> usize {
        self.saves.len() / 2
    }
}

/// Get a group by index.
///
/// `'t` is the lifetime of the matched text.
///
/// The text can't outlive the `Captures` object if this method is
/// used, because of how `Index` is defined (normally `a[i]` is part
/// of `a` and can't outlive it); to do that, use `get()` instead.
///
/// # Panics
///
/// If there is no group at the given index.
impl<'r, 't> Index<usize> for Captures<'r, 't> {
    type Output = str;

    fn index(&self, i: usize) -> &str {
        self.get(i)
            .map(|m| m.as_str())
            .unwrap_or_else(|| panic!("no group at index '{}'", i))
    }
}

/// Get a group by name.
///
/// `'t` is the lifetime of the matched text and `'i` is the lifetime
/// of the group name (the index).
///
/// The text can't outlive the `Captures` object if this method is
/// used, because of how `Index` is defined (normally `a[i]` is part
/// of `a` and can't outlive it); to do that, use `name` instead.
///
/// # Panics
///
/// If there is no group named by the given value.
impl<'r, 't, 'i> Index<&'i str> for Captures<'r, 't> {
    type Output = str;

    fn index<'a>(&'a self, name: &'i str) -> &'a str {
        self.name(name)
            .map(|m| m.as_str())
            .unwrap_or_else(|| panic!("no group named '{}'", name))
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

/// Regular expression AST. This is public for now but may change.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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
    /// Keep matched text so far out of overall match
    KeepOut,
    /// Anchor to match at the position where the previous match ended
    ContinueFromPreviousMatchEnd,
    /// Conditional expression based on whether the numbered capture group matched or not
    BackrefExistsCondition(usize),
    /// If/Then/Else Condition. If there is no Then/Else, these will just be empty expressions.
    Conditional {
        /// The conditional expression to evaluate
        condition: Box<Expr>,
        /// What to execute if the condition is true
        true_branch: Box<Expr>,
        /// What to execute if the condition is false
        false_branch: Box<Expr>,
    },
}

/// Type of look-around assertion as used for a look-around expression.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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

/// An iterator over capture names in a [Regex].  The iterator
/// returns the name of each group, or [None] if the group has
/// no name.  Because capture group 0 cannot have a name, the
/// first item returned is always [None].
pub struct CaptureNames<'r>(std::vec::IntoIter<Option<&'r str>>);

impl Debug for CaptureNames<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("<CaptureNames>")
    }
}

impl<'r> Iterator for CaptureNames<'r> {
    type Item = Option<&'r str>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl Expr {
    /// Parse the regex and return an expression (AST) and a bit set with the indexes of groups
    /// that are referenced by backrefs.
    pub fn parse_tree(re: &str) -> Result<ExprTree> {
        ExprTree::parse(re)
    }

    fn to_ast(self, capture_index: &mut u32) -> regex_syntax::ast::Ast {
        use regex_syntax::ast::*;
        // XXX: implement span?
        let span = Span::splat(Position::new(0, 0, 0));

        let with_flag = |flag, ast| {
            if let Some(flag) = flag {
                Ast::Group(Box::new(Group {
                    span,
                    kind: GroupKind::NonCapturing(Flags {
                        span,
                        items: vec![FlagsItem {
                            span,
                            kind: FlagsItemKind::Flag(flag),
                        }],
                    }),
                    ast: Box::new(ast),
                }))
            } else {
                ast
            }
        };

        let mut fetch_add_capture_index = || {
            let index = *capture_index;
            *capture_index += 1;
            index
        };

        match self {
            Expr::Empty => Ast::Empty(Box::new(span)),
            Expr::Any { newline } => with_flag(
                newline.then_some(Flag::DotMatchesNewLine),
                Ast::Dot(Box::new(span)),
            ),
            Expr::Literal { val, casei } => with_flag(
                casei.then_some(Flag::CaseInsensitive),
                Ast::Concat(Box::new(Concat {
                    span,
                    asts: val
                        .chars()
                        .map(|c| {
                            Ast::Literal(Box::new(Literal {
                                span,
                                kind: LiteralKind::Verbatim, // does not matter
                                c,
                            }))
                        })
                        .collect(),
                })),
            ),
            Expr::StartText => Ast::Assertion(Box::new(Assertion {
                span,
                kind: AssertionKind::StartText,
            })),
            Expr::StartLine => with_flag(
                Some(Flag::MultiLine),
                Ast::Assertion(Box::new(Assertion {
                    span,
                    kind: AssertionKind::StartLine,
                })),
            ),
            Expr::EndText => Ast::Assertion(Box::new(Assertion {
                span,
                kind: AssertionKind::EndText,
            })),
            Expr::EndLine => with_flag(
                Some(Flag::MultiLine),
                Ast::Assertion(Box::new(Assertion {
                    span,
                    kind: AssertionKind::EndLine,
                })),
            ),
            Expr::Concat(children) => Ast::Concat(Box::new(Concat {
                span,
                asts: children
                    .into_iter()
                    .map(|child| child.to_ast(capture_index))
                    .collect(),
            })),
            Expr::Alt(children) => Ast::Alternation(Box::new(Alternation {
                span,
                asts: children
                    .into_iter()
                    .map(|child| child.to_ast(capture_index))
                    .collect(),
            })),
            Expr::Group(child) => Ast::Group(Box::new(Group {
                span,
                kind: GroupKind::CaptureIndex(fetch_add_capture_index()),
                ast: Box::new(child.to_ast(capture_index)),
            })),
            Expr::Repeat {
                child,
                lo,
                hi,
                greedy,
            } => Ast::Repetition(Box::new(Repetition {
                span,
                op: RepetitionOp {
                    span,
                    kind: match (lo, hi) {
                        (0, 1) => RepetitionKind::ZeroOrOne,
                        (0, usize::MAX) => RepetitionKind::ZeroOrMore,
                        (1, usize::MAX) => RepetitionKind::OneOrMore,
                        (lo, hi) if lo == hi => {
                            RepetitionKind::Range(RepetitionRange::Exactly(lo.try_into().unwrap()))
                        }
                        (lo, usize::MAX) => {
                            RepetitionKind::Range(RepetitionRange::AtLeast(lo.try_into().unwrap()))
                        }
                        (lo, hi) => RepetitionKind::Range(RepetitionRange::Bounded(
                            lo.try_into().unwrap(),
                            hi.try_into().unwrap(),
                        )),
                    },
                },
                greedy,
                ast: Box::new(child.to_ast(capture_index)),
            })),
            Expr::Delegate { inner, casei, .. } => with_flag(
                casei.then_some(Flag::CaseInsensitive),
                parse_ast(&inner, capture_index),
            ),
            _ => panic!("attempting to format hard expr"),
        }
    }

    /// Convert expression to a [`regex_syntax::hir::Hir`].
    pub fn to_hir(self) -> regex_syntax::hir::Hir {
        let mut capture_index = 1;
        let ast = self.to_ast(&mut capture_index);
        let mut translator = regex_syntax::hir::translate::Translator::new();
        // using empty pattern does not matter
        // as it cannot error
        translator.translate("", &ast).unwrap()
    }
}

fn offset_capture_index(ast: &mut regex_syntax::ast::Ast, capture_index: u32) -> u32 {
    use regex_syntax::ast::*;

    let recur = |ast| offset_capture_index(ast, capture_index);

    match ast {
        Ast::Alternation(children) => children.asts.iter_mut().map(recur).max(),
        Ast::Concat(children) => children.asts.iter_mut().map(recur).max(),
        Ast::Repetition(child) => Some(recur(&mut child.ast)),
        Ast::Group(child) => match &mut child.kind {
            GroupKind::CaptureIndex(index) => {
                *index += capture_index;
                Some(*index + 1)
            }
            _ => None,
        },
        Ast::Assertion(_)
        | Ast::ClassBracketed(_)
        | Ast::ClassPerl(_)
        | Ast::ClassUnicode(_)
        | Ast::Dot(_)
        | Ast::Empty(_)
        | Ast::Flags(_)
        | Ast::Literal(_) => None,
    }
    .unwrap_or(capture_index)
}

fn parse_ast(pattern: &str, capture_index: &mut u32) -> regex_syntax::ast::Ast {
    let mut ast = regex_syntax::ast::parse::Parser::new()
        .parse(pattern)
        .unwrap();
    *capture_index = offset_capture_index(&mut ast, *capture_index);
    ast
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // XXX: clone
        write!(f, "{}", self.clone().to_hir())
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

/// Returns the smallest possible index of the next valid UTF-8 sequence
/// starting after `i`.
/// Adapted from a function with the same name in the `regex` crate.
fn next_utf8(text: &str, i: usize) -> usize {
    let b = match text.as_bytes().get(i) {
        None => return i + 1,
        Some(&b) => b,
    };
    i + codepoint_len(b)
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
    pub use crate::vm::{run_default_from_pos, run_trace_from_pos, Insn, Prog};
}

#[cfg(test)]
mod tests {
    use crate::RegexBuilder;
    use crate::parse::make_literal;
    use crate::Expr;
    use crate::Regex;
    //use detect_possible_backref;

    // tests for to_str

    fn to_str(e: Expr) -> String {
        format!("{}", e)
    }

    #[test]
    fn to_str_concat_alt() {
        let e = Expr::Concat(vec![
            Expr::Alt(vec![make_literal("a"), make_literal("b")]),
            make_literal("c"),
        ]);
        assert_eq!(to_str(e), "(?:[ab]c)");
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
        assert_eq!(to_str(e), "([ab])");
    }

    #[test]
    fn as_str_debug() {
        let s = r"(a+)b\1";
        let regex = Regex::new(s).unwrap();
        assert_eq!(s, regex.as_str());
        assert_eq!(s, format!("{:?}", regex));
    }

    #[test]
    fn display() {
        let s = r"(a+)b\1";
        let regex = Regex::new(s).unwrap();
        assert_eq!(s, format!("{}", regex));
    }

    #[test]
    fn from_str() {
        let s = r"(a+)b\1";
        let regex = s.parse::<Regex>().unwrap();
        assert_eq!(regex.as_str(), s);
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
        assert_eq!(to_str(repeat(2, 2, false)), "a{2}");
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

    #[test]
    fn to_str_multiline() {
        let tree = Expr::parse_tree("(?m)^yes$").unwrap();
        let expr = tree.expr;
        assert_eq!(to_str(expr), "(?:(?m:^)(?:yes)(?m:$))");
    }

    #[test]
    fn expr_roundtrip() {
        let tree = Expr::parse_tree("(?m)^yes$").unwrap();
        let regex = RegexBuilder::new().build_from_expr_tree(tree.clone()).unwrap();
        assert_eq!(regex.as_expr_tree(), &tree);
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
