// Copyright 2025 The Fancy Regex Authors.
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

//! RegexSet API for matching multiple patterns against the same input.
//!
//! This module provides [`RegexSet`], which allows efficient matching of multiple
//! regular expression patterns against the same input text. This is particularly
//! useful for applications like syntax highlighting, where many patterns need to
//! be matched against each line of text.
//!
//! **IMPORTANT NOTE:** The `RegexSet` here differs from what the `regex` crate
//! provides in that `fancy-regex` will only return the left-most, highest priority
//! match, with a non-overlapping search, as opposed to all overlapping matches.
//!
//! # Examples
//!
//! Basic usage:
//!
//! ```rust
//! use fancy_regex::RegexSet;
//!
//! # fn main() -> Result<(), fancy_regex::Error> {
//! let set = RegexSet::new(&[
//!     r"\d+",              // Pattern 0: numbers
//!     r"\w+",              // Pattern 1: words
//!     r"(?<=\$)\d+\.\d+",  // Pattern 2: prices (with lookbehind)
//! ])?;
//!
//! let text = "The price is $29.99 today";
//!
//! for result in set.matches(text) {
//!     let m = result?;
//!     println!("Pattern {} matched '{}' at {}..{}",
//!         m.pattern(), m.as_str(), m.start(), m.end());
//! }
//! # Ok(())
//! # }
//! ```
//!
//! # Performance
//!
//! The `RegexSet` uses a hybrid approach to achieve good performance:
//!
//! - **Easy patterns** (those without backreferences, lookaround, etc.) are
//!   combined into a single multi-pattern DFA for parallel evaluation. This
//!   provides very fast matching with linear time complexity.
//!
//! - **Hard patterns** (those with backreferences, lookaround, etc.) are
//!   evaluated individually using a backtracking VM. These may have exponential
//!   time complexity in pathological cases.
//!
//! For best performance, try to design patterns that can be delegated to the
//! DFA when possible.
//!
//! # Priority and Non-Overlapping Matches
//!
//! The iterator returns non-overlapping matches in order of their start position.
//! When multiple patterns match at the same position, the pattern with the
//! lowest index (specified first in the constructor) wins. After yielding a match
//! at position `pos` with length `len`, the next match starts searching from
//! `pos + max(1, len)`, which prevents infinite loops on zero-width matches.

use alloc::boxed::Box;
use alloc::string::ToString;
use alloc::sync::Arc;
use alloc::vec;
use alloc::vec::Vec;
use core::ops::Range;

use crate::RegexOptionsBuilder;

use regex_automata::meta::Regex as RaRegex;
use regex_automata::Input as RaInput;
use regex_automata::PatternSet;

use crate::compile::options_to_rabuilder;
use crate::CompileError;
use crate::Error;
use crate::{Captures, Regex, RegexOptions, Result};

/// A compiled set of regular expressions.
///
/// A `RegexSet` allows you to match multiple patterns against the same input
/// text efficiently. It's particularly useful for applications like syntax
/// highlighting or token scanning where you need to check many patterns against
/// each piece of text.
///
/// The set analyzes patterns at compile time and uses different strategies for
/// different types of patterns:
/// - Simple patterns are combined into a single high-performance DFA
/// - Complex patterns (with backreferences, lookaround, etc.) use backtracking
///
/// **IMPORTANT NOTE:** The `RegexSet` here differs from what the `regex` crate
/// provides in that `fancy-regex` will only return the left-most, highest priority
/// match, with a non-overlapping search, as opposed to all overlapping matches.
///
/// # Examples
///
/// Basic matching:
///
/// ```rust
/// use fancy_regex::RegexSet;
///
/// # fn main() -> Result<(), fancy_regex::Error> {
/// let set = RegexSet::new(&[r"\d+", r"[a-z]+", r"[A-Z]+"])?;
///
/// let text = "abc 123 XYZ";
/// for m in set.matches(text) {
///     let m = m?;
///     println!("Pattern {} matched: {}", m.pattern(), m.as_str());
/// }
/// # Ok(())
/// # }
/// ```
///
/// The `RegexSet` is cheaply cloneable (via `Arc`) and can be used from
/// multiple threads:
///
/// ```rust
/// use fancy_regex::RegexSet;
/// use std::sync::Arc;
///
/// # fn main() -> Result<(), fancy_regex::Error> {
/// let set = Arc::new(RegexSet::new(&[r"\d+"])?);
/// let set_clone = Arc::clone(&set);
///
/// // Use from different threads...
/// # Ok(())
/// # }
/// ```
#[derive(Clone, Debug)]
pub struct RegexSet {
    inner: Arc<RegexSetImpl>,
}

#[derive(Debug)]
struct RegexSetImpl {
    easy_patterns: Option<EasyPatternSet>,
    patterns: Vec<Pattern>,
}

#[derive(Debug)]
struct EasyPatternSet {
    dfa: RaRegex,
    /// Indices into RegexSetImpl.patterns for easy patterns only
    pattern_indices: Vec<usize>,
}

#[derive(Debug, Clone)]
struct Pattern {
    pattern_id: usize,
    regex: Regex,
}

impl RegexSet {
    /// Create a new RegexSet from an iterator of patterns using default options.
    ///
    /// All patterns will use the same default configuration:
    /// - Case sensitive
    /// - Multi-line mode disabled
    /// - Dot does not match newline
    /// - Unicode mode enabled
    ///
    /// # Errors
    ///
    /// Returns an error if any pattern fails to compile.
    pub fn new<I, S>(patterns: I) -> Result<Self>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        let builder = RegexOptionsBuilder::new();
        Self::new_with_options(patterns, &builder)
    }

    /// Create a new RegexSet from an iterator of patterns using specified options.
    ///
    /// # Errors
    ///
    /// Returns an error if any pattern fails to compile.
    pub fn new_with_options<I, S>(patterns: I, options_builder: &RegexOptionsBuilder) -> Result<Self>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        let regexes = patterns
            .into_iter()
            .map(|pattern| {
                options_builder.build(pattern.as_ref().to_string())
            })
        .collect::<Result<Vec<_>>>()?;
        Self::from_regexes(regexes)
    }

    /// Create a new RegexSet from pre-built `Regex` instances.
    ///
    /// Regex instances which are just thin wrappers around the `regex` crate
    /// i.e. "easy" patterns (those without backreferences, lookarounds, etc.)
    /// are combined into a single DFA, while hard patterns are evaluated individually.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use fancy_regex::{Regex, RegexBuilder, RegexSet};
    ///
    /// # fn main() -> Result<(), fancy_regex::Error> {
    /// // Create regexes with different options
    /// let re1 = RegexBuilder::new(r"hello")
    ///     .case_insensitive(true)
    ///     .build()?;
    /// let re2 = Regex::new(r"\d+")?;
    /// let re3 = Regex::new(r"(?<=\w)end")?; // lookbehind - fancy pattern
    ///
    /// // Combine them into a RegexSet
    /// let set = RegexSet::from_regexes([re1, re2, re3])?;
    ///
    /// let text = "HELLO 123 send";
    /// for m in set.matches(text) {
    ///     let m = m?;
    ///     println!("Pattern {} matched: {}", m.pattern(), m.as_str());
    /// }
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// # Errors
    ///
    /// Returns an error if the multi-pattern DFA construction fails for easy patterns.
    pub fn from_regexes<I>(regexes: I) -> Result<Self>
    where
        I: IntoIterator<Item = Regex>,
    {
        let regexes_vec: Vec<Regex> = regexes.into_iter().collect();
        
        if regexes_vec.is_empty() {
            return Ok(RegexSet {
                inner: Arc::new(RegexSetImpl {
                    easy_patterns: None,
                    patterns: Vec::new(),
                }),
            });
        }

        let mut easy_pattern_strings = Vec::new();
        let mut easy_pattern_indices = Vec::new();
        let mut patterns = Vec::new();

        // Analyze each regex and categorize as easy or hard
        for (index, regex) in regexes_vec.into_iter().enumerate() {
            match &regex.inner {
                crate::RegexImpl::Wrap { delegated_pattern, .. } => {
                    // Easy pattern - can be delegated to DFA
                    easy_pattern_strings.push(delegated_pattern.clone());
                    easy_pattern_indices.push(index);
                }
                crate::RegexImpl::Fancy { .. } => {
                    // Hard pattern - uses backtracking VM
                }
            }

            patterns.push(Pattern {
                pattern_id: index,
                regex,
            });
        }

        // Build multi-pattern DFA for easy patterns
        let easy_patterns = if !easy_pattern_strings.is_empty() {
            // Use default options for the DFA builder
            // Note: Individual regexes already have their options baked in
            let options = RegexOptions::default();
            let builder = options_to_rabuilder(&options);

            let dfa = builder
                .build_many(&easy_pattern_strings)
                .map_err(CompileError::InnerError)
                .map_err(|e| Error::CompileError(Box::new(e)))?;

            Some(EasyPatternSet {
                dfa,
                pattern_indices: easy_pattern_indices,
            })
        } else {
            None
        };

        Ok(RegexSet {
            inner: Arc::new(RegexSetImpl {
                easy_patterns,
                patterns,
            }),
        })
    }

    /// Returns the number of patterns in the set.
    pub fn len(&self) -> usize {
        self.inner.patterns.len()
    }

    /// Returns true if the set contains no patterns.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Create a new matches iterator for the given haystack.
    ///
    /// The iterator will find all non-overlapping matches in the haystack,
    /// returning them in order of their start position. When multiple patterns
    /// match at the same position, the pattern with the lowest index wins.
    ///
    /// # Arguments
    ///
    /// * `haystack` - The text to search in
    ///
    /// # Returns
    ///
    /// An iterator over matches in the haystack.
    pub fn matches<'h>(&'h self, haystack: &'h str) -> RegexSetMatches<'h> {
        self.matches_range(haystack, 0..haystack.len())
    }

    /// Create a new matches iterator with a specific byte range in the haystack.
    ///
    /// This is useful when you want to search only a portion of the haystack while
    /// still having access to the full text for features like lookbehind/lookahead.
    ///
    /// # Arguments
    ///
    /// * `haystack` - The full text
    /// * `range` - The byte range within the haystack to search
    ///
    /// # Panics
    ///
    /// Panics if the range is not within bounds or does not fall on UTF-8 boundaries.
    pub fn matches_range<'h>(
        &'h self,
        haystack: &'h str,
        range: Range<usize>,
    ) -> RegexSetMatches<'h> {
        assert!(range.start <= haystack.len() && range.end <= haystack.len());
        assert!(haystack.is_char_boundary(range.start) && haystack.is_char_boundary(range.end));

        RegexSetMatches {
            set: self,
            haystack,
            range: range.clone(),
            current_pos: range.start,
            pattern_cache: vec![None; self.inner.patterns.len()],
            easy_next_match: None,
            pattern_set: None,
        }
    }
}

/// A match from a RegexSet, including the pattern index and capture groups.
///
/// This type represents a single match found by a [`RegexSet`]. It provides
/// information about which pattern matched, the location of the match, and
/// access to any capture groups.
///
/// # Examples
///
/// ```rust
/// use fancy_regex::RegexSet;
///
/// # fn main() -> Result<(), fancy_regex::Error> {
/// let set = RegexSet::new(&[r"(\d+)-(\d+)", r"[a-z]+"])?;
/// let text = "abc 123-456";
///
/// for m in set.matches(text) {
///     let m = m?;
///     println!("Pattern {} matched '{}' at {}..{}",
///         m.pattern(), m.as_str(), m.start(), m.end());
///     
///     // Access capture groups
///     for cap in m.captures().iter() {
///         if let Some(cap) = cap {
///             println!("  Capture: {}", cap.as_str());
///         }
///     }
/// }
/// # Ok(())
/// # }
/// ```
#[derive(Debug, Clone)]
pub struct RegexSetMatch<'h> {
    pattern_index: usize,
    captures: Captures<'h>,
}

impl<'h> RegexSetMatch<'h> {
    /// Returns the index of the pattern that matched.
    pub fn pattern(&self) -> usize {
        self.pattern_index
    }

    /// Returns the start byte offset of the overall match.
    pub fn start(&self) -> usize {
        self.captures.get(0).map(|m| m.start()).unwrap_or(0)
    }

    /// Returns the end byte offset of the overall match.
    pub fn end(&self) -> usize {
        self.captures.get(0).map(|m| m.end()).unwrap_or(0)
    }

    /// Returns the matched text.
    pub fn as_str(&self) -> &'h str {
        self.captures.get(0).map(|m| m.as_str()).unwrap_or("")
    }

    /// Returns the range of the overall match.
    pub fn range(&self) -> Range<usize> {
        self.start()..self.end()
    }

    /// Returns a reference to the captures for this match.
    pub fn captures(&self) -> &Captures<'h> {
        &self.captures
    }

    /// Consumes self and returns the captures.
    pub fn into_captures(self) -> Captures<'h> {
        self.captures
    }
}

/// Iterator over matches in a haystack.
///
/// The iterator returns non-overlapping matches in order of their start position.
/// After yielding a match at position `pos` with length `len`, the next match
/// will start searching from `pos + max(1, len)`. This prevents infinite loops
/// on zero-width matches while still allowing them to be found.
///
/// When multiple patterns match at the same position, the pattern with the
/// lowest index (specified first in the constructor) is returned.
///
/// # Performance
///
/// The iterator uses an incremental search strategy for easy patterns:
/// - Finds the next match lazily using `dfa.find()` only when needed
/// - Uses `which_overlapping_matches()` to efficiently determine which patterns
///   match at a given position
/// - Only extracts capture groups for the selected pattern
/// - Avoids pre-computing all matches upfront, making it efficient when:
///   * Only consuming a few matches from a large input
///   * The input contains many matches but you only need the first few
///   * Processing input line-by-line (e.g., syntax highlighting)
#[derive(Debug)]
pub struct RegexSetMatches<'h> {
    set: &'h RegexSet,
    haystack: &'h str,
    /// The original range in the haystack to search within
    range: Range<usize>,
    current_pos: usize,
    // Cache of next match for each pattern: (start_pos, end_pos, captures)
    pattern_cache: Vec<Option<(usize, usize, Captures<'h>)>>,
    // For easy patterns: stores the DFA pattern index from the next dfa.find()
    easy_next_match: Option<(usize, Range<usize>)>,
    // Reusable PatternSet for which_overlapping_matches
    pattern_set: Option<PatternSet>,
}

impl<'h> Iterator for RegexSetMatches<'h> {
    type Item = Result<RegexSetMatch<'h>>;

    /// Returns the next match, or None if no more matches exist.
    ///
    /// Returns an error if:
    /// - A pattern exceeds its backtracking limit
    /// - Any other runtime error occurs during matching
    fn next(&mut self) -> Option<Self::Item> {
        if self.current_pos > self.range.end {
            return None;
        }

        loop {
            let mut earliest_match: Option<(usize, usize, RegexSetMatch<'h>)> = None;

            // For easy patterns, use the DFA to find the next match position efficiently
            if self.easy_next_match.is_none() {
                if let Some(ref easy_set) = self.set.inner.easy_patterns {
                    let input = RaInput::new(self.haystack).span(self.current_pos..self.range.end);

                    if let Some(mat) = easy_set.dfa.find(input) {
                        let dfa_pattern_idx = mat.pattern().as_usize();
                        let range = mat.start()..mat.end();
                        self.easy_next_match = Some((dfa_pattern_idx, range));
                    }
                }
            }

            // If we have a possible DFA match, check the matching easy patterns at that position
            if let Some((dfa_pattern_idx, ref dfa_range)) = self.easy_next_match {
                if dfa_range.start >= self.current_pos {
                    match self.check_easy_patterns_at_position(
                        dfa_range.start,
                        dfa_pattern_idx,
                        &dfa_range.clone(),
                    ) {
                        Ok(Some(m)) => {
                            earliest_match = Some((m.start(), m.pattern(), m));
                        }
                        Ok(None) => {
                            // No match, invalidate and continue
                            self.easy_next_match = None;
                            continue;
                        }
                        Err(e) => {
                            // Stop on first error: If an error is encountered, return it, and set the
                            // current position beyond the range end, so that the next next() call will
                            // return None, to prevent an infinite loop.
                            self.current_pos = self.range.end + 1;
                            return Some(Err(e));
                        }
                    }
                } else {
                    // Match is behind us, invalidate
                    self.easy_next_match = None;
                    continue;
                }
            }

            // Check all other patterns (hard patterns)
            for (i, pattern) in self.set.inner.patterns.iter().enumerate() {
                // Skip if this pattern is an easy pattern that we just checked above
                if let Some(ref easy_set) = self.set.inner.easy_patterns {
                    if easy_set.pattern_indices.contains(&pattern.pattern_id) {
                        // This is an easy pattern, already handled above
                        continue;
                    }
                }

                // Search this pattern if not cached
                if self.pattern_cache[i].is_none() {
                    match self.search_pattern(pattern) {
                        Ok(result) => {
                            self.pattern_cache[i] = result;
                        }
                        Err(e) => {
                            // Stop on first error: If an error is encountered, return it, and set the
                            // current position beyond the range end, so that the next next() call will
                            // return None, to prevent an infinite loop.
                            self.current_pos = self.range.end + 1;
                            return Some(Err(e));
                        }
                    }
                }

                // Check if this pattern has a match at or after current position
                if let Some((start, _end, ref captures)) = self.pattern_cache[i] {
                    if start >= self.current_pos {
                        let key = (start, pattern.pattern_id);
                        if earliest_match.is_none()
                            || key
                                < (
                                    earliest_match.as_ref().unwrap().0,
                                    earliest_match.as_ref().unwrap().1,
                                )
                        {
                            earliest_match = Some((
                                start,
                                pattern.pattern_id,
                                RegexSetMatch {
                                    pattern_index: pattern.pattern_id,
                                    captures: captures.clone(),
                                },
                            ));
                        }
                    }
                }
            }

            match earliest_match {
                Some((_, _, match_result)) => {
                    // Advance position for next iteration
                    let match_len = match_result.end() - match_result.start();
                    if match_len == 0 {
                        // This is an empty match. To ensure we make progress, start
                        // the next search at the beginning of the next utf8 codepoint
                        // following this one, i.e. one character ahead.
                        self.current_pos = crate::next_utf8(self.haystack, match_result.end());
                    } else {
                        self.current_pos = match_result.end();
                    }

                    // Invalidate cache entries that are now behind us
                    self.invalidate_cache_before(self.current_pos);

                    return Some(Ok(match_result));
                }
                None => {
                    // No matches found
                    return None;
                }
            }
        }
    }
}

impl<'h> RegexSetMatches<'h> {
    /// Check all easy patterns at a specific position to find the best match.
    ///
    /// Uses which_overlapping_matches to find all patterns that match at the position,
    /// then selects the one with the lowest index and extracts its captures.
    fn check_easy_patterns_at_position(
        &mut self,
        pos: usize,
        dfa_pattern_idx: usize,
        dfa_range: &Range<usize>,
    ) -> Result<Option<RegexSetMatch<'h>>> {
        if let Some(ref easy_set) = self.set.inner.easy_patterns {
            // Initialize pattern_set if needed
            if self.pattern_set.is_none() {
                self.pattern_set = Some(PatternSet::new(easy_set.pattern_indices.len()));
            }

            // Use which_overlapping_matches to find all patterns that match starting at pos
            let input = RaInput::new(self.haystack).span(pos..self.range.end);

            let pattern_set = self.pattern_set.as_mut().unwrap();
            pattern_set.clear();
            easy_set.dfa.which_overlapping_matches(&input, pattern_set);

            // Find the pattern with the lowest index that matches at this position
            if let Some(dfa_pattern_id) = pattern_set.iter().next() {
                let dfa_idx = dfa_pattern_id.as_usize();
                // Map DFA pattern index to actual pattern index
                let pattern_idx = easy_set.pattern_indices[dfa_idx];
                let pattern = &self.set.inner.patterns[pattern_idx];

                // Determine the match range
                let range = if dfa_idx == dfa_pattern_idx && dfa_range.start == pos {
                    // Use cached range from DFA
                    dfa_range.clone()
                } else {
                    // Need to find the actual range for this pattern
                    let search_input = RaInput::new(self.haystack)
                        .range(self.range.clone())
                        .span(pos..self.range.end);

                    if let Some(mat) = easy_set.dfa.find(search_input) {
                        if mat.pattern().as_usize() == dfa_idx && mat.start() == pos {
                            mat.start()..mat.end()
                        } else {
                            // DFA found a different pattern, skip this
                            return Ok(None);
                        }
                    } else {
                        return Ok(None);
                    }
                };

                // Use the pattern's Regex to extract captures
                match pattern
                    .regex
                    .captures_from_pos(self.haystack, range.start)?
                {
                    Some(captures) => {
                        return Ok(Some(RegexSetMatch {
                            pattern_index: pattern.pattern_id,
                            captures,
                        }));
                    }
                    None => return Ok(None),
                }
            }
        }

        Ok(None)
    }

    /// Search a pattern starting from current position.
    fn search_pattern(&self, pattern: &Pattern) -> Result<Option<(usize, usize, Captures<'h>)>> {
        match pattern
            .regex
            .captures_from_pos(self.haystack, self.current_pos)?
        {
            Some(captures) => {
                let group0 = captures
                    .get(0)
                    .expect("captures should always have group 0");
                Ok(Some((group0.start(), group0.end(), captures)))
            }
            None => Ok(None),
        }
    }

    fn invalidate_cache_before(&mut self, pos: usize) {
        // Remove easy match if it's before pos
        if let Some((_, ref range)) = self.easy_next_match {
            if range.start < pos {
                self.easy_next_match = None;
            }
        }

        // Remove cache entries before pos
        for cached in &mut self.pattern_cache {
            if let Some((start, _, _)) = cached {
                if *start < pos {
                    *cached = None;
                }
            }
        }
    }
}
