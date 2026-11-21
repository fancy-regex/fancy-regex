// Copyright 2026 The Fancy Regex Authors.
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

//! Sharing of compiled regex-automata engines between regexes.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use regex_automata::meta::Regex as RaRegex;

use crate::compile::{CompileOptions, DelegateUsage};
use crate::{BytesMode, Result};

/// A cache of compiled regex-automata engines, shared between regexes.
///
/// fancy-regex builds a regex-automata engine for each easy pattern, and for each
/// easy fragment of a fancy pattern. Building these engines dominates compile time
/// and memory. Applications that compile many patterns, such as syntax highlighters
/// loading TextMate or Sublime Text grammars, often compile the same pattern or the
/// same fragment many times.
///
/// Regexes built with the same `EngineCache` (see
/// [`RegexOptionsBuilder::engine_cache`](crate::RegexOptionsBuilder::engine_cache))
/// reuse the engine already built for an identical expression with identical engine
/// settings, instead of building another copy. Matching behavior is unchanged, and
/// each regex keeps its own search cache, so searches don't contend with each other.
///
/// Cloning an `EngineCache` gives another handle to the same cache. Engines stay in
/// the cache, even after the regexes that use them are dropped, until every handle
/// to the cache is dropped.
///
/// Requires the `std` feature.
///
/// # Example
///
/// ```rust
/// use fancy_regex::{EngineCache, RegexOptionsBuilder};
///
/// let cache = EngineCache::new();
/// let mut options = RegexOptionsBuilder::new();
/// options.engine_cache(&cache);
///
/// let a = options.build(r"\b(\w+)\s*=".to_string()).unwrap();
/// // Reuses the engine built for `a`.
/// let b = options.build(r"\b(\w+)\s*=".to_string()).unwrap();
/// assert!(b.is_match("x = 1").unwrap());
/// ```
#[derive(Clone, Default)]
pub struct EngineCache {
    engines: Arc<Mutex<HashMap<EngineKey, RaRegex>>>,
}

/// Everything that determines how an engine is built. The delegate pattern string
/// already encodes the syntax flags (see `options_to_rabuilder`), so only the
/// settings passed to the regex-automata builder need to be added here.
#[derive(Clone, PartialEq, Eq, Hash)]
struct EngineKey {
    pattern: String,
    usage: DelegateUsage,
    /// The top-level engine of an easy pattern keeps the capture slots of groups
    /// inside `{0}` repetitions, VM delegates don't (see `to_hir`).
    top_level: bool,
    utf8: bool,
    unicode: bool,
    delegate_size_limit: Option<usize>,
    delegate_dfa_size_limit: Option<usize>,
}

impl EngineCache {
    /// Create a new, empty engine cache.
    pub fn new() -> Self {
        Self::default()
    }

    /// Return the cached engine for `pattern`, or build it with `build` and cache it.
    /// The lock isn't held while building, so if two threads build the same engine
    /// at the same time, the first one to finish is kept.
    pub(crate) fn get_or_build(
        &self,
        pattern: &str,
        options: &CompileOptions,
        usage: DelegateUsage,
        top_level: bool,
        build: impl FnOnce() -> Result<RaRegex>,
    ) -> Result<RaRegex> {
        let key = EngineKey {
            pattern: pattern.to_string(),
            usage,
            top_level,
            utf8: matches!(options.bytes_mode, BytesMode::Unicode),
            unicode: options.unicode,
            delegate_size_limit: options.delegate_size_limit,
            delegate_dfa_size_limit: options.delegate_dfa_size_limit,
        };
        if let Some(engine) = self.lock().get(&key) {
            return Ok(engine.clone());
        }
        let engine = build()?;
        Ok(self.lock().entry(key).or_insert(engine).clone())
    }

    fn lock(&self) -> std::sync::MutexGuard<'_, HashMap<EngineKey, RaRegex>> {
        // The map is never left in an inconsistent state, so a panic in another
        // thread while it held the lock doesn't matter.
        self.engines
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    #[cfg(test)]
    fn len(&self) -> usize {
        self.lock().len()
    }
}

impl core::fmt::Debug for EngineCache {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("EngineCache")
            .field("engines", &self.lock().len())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::Insn;
    use crate::{Regex, RegexBuilder, RegexImpl};

    /// The top-level engine of an easy regex, or the delegate engines of a fancy one.
    fn engines(regex: &Regex) -> Vec<RaRegex> {
        match &regex.inner {
            RegexImpl::Wrap { inner, .. } => vec![inner.clone()],
            RegexImpl::Fancy { prog, .. } => prog
                .body
                .iter()
                .filter_map(|insn| match insn {
                    Insn::Delegate(delegate) => Some(delegate.inner.clone()),
                    _ => None,
                })
                .collect(),
        }
    }

    /// `RaRegex::clone` shares the compiled engine (including its config) but not the
    /// search cache, so engines are the same if their config is.
    fn same_engine(a: &RaRegex, b: &RaRegex) -> bool {
        core::ptr::eq(a.get_config(), b.get_config())
    }

    fn build(pattern: &str, cache: &EngineCache) -> Regex {
        RegexBuilder::new(pattern)
            .engine_cache(cache)
            .build()
            .unwrap()
    }

    #[test]
    fn identical_patterns_share_an_engine() {
        let cache = EngineCache::new();
        let a = build(r"\b(\w+)\s*=", &cache);
        let b = build(r"\b(\w+)\s*=", &cache);
        assert!(same_engine(&engines(&a)[0], &engines(&b)[0]));
        assert_eq!(cache.len(), 1);

        // Without the cache, each regex builds its own engine.
        let c = Regex::new(r"\b(\w+)\s*=").unwrap();
        assert!(!same_engine(&engines(&a)[0], &engines(&c)[0]));

        let caps = b.captures("let x = 1").unwrap().unwrap();
        assert_eq!(caps.get(1).unwrap().as_str(), "x");
    }

    #[test]
    fn different_fancy_patterns_share_delegates() {
        let cache = EngineCache::new();
        let a = build(r"(?<=let )(\w+)\s*=", &cache);
        let b = build(r"(?<!fn )(\w+)\s*=", &cache);
        let (a_engines, b_engines) = (engines(&a), engines(&b));
        assert!(!a_engines.is_empty());
        assert!(a_engines
            .iter()
            .any(|x| b_engines.iter().any(|y| same_engine(x, y))));

        assert_eq!(a.find("let x = 1").unwrap().unwrap().as_str(), "x =");
        assert_eq!(b.find("x = 1").unwrap().unwrap().as_str(), "x =");
    }

    #[test]
    fn engine_settings_are_part_of_the_key() {
        let cache = EngineCache::new();
        let unicode = build(r"\w+", &cache);
        let ascii = RegexBuilder::new(r"\w+")
            .unicode_mode(false)
            .engine_cache(&cache)
            .build()
            .unwrap();
        let limited = RegexBuilder::new(r"\w+")
            .delegate_size_limit(1 << 20)
            .engine_cache(&cache)
            .build()
            .unwrap();
        assert_eq!(cache.len(), 3);
        assert!(unicode.is_match("é").unwrap());
        assert!(!ascii.is_match("é").unwrap());
        assert!(limited.is_match("é").unwrap());
    }

    #[test]
    fn failed_builds_are_not_cached() {
        let cache = EngineCache::new();
        let result = RegexBuilder::new(r"\w{100}")
            .delegate_size_limit(1)
            .engine_cache(&cache)
            .build();
        assert!(result.is_err());
        assert_eq!(cache.len(), 0);
    }
}
