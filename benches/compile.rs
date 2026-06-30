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

//! End-to-end *compilation* benchmarks: how long it takes to turn a pattern
//! string into a finished `Regex` / `RegexSet`.
//!
//! These complement `bench.rs`, which measures parsing and matching. Here every
//! iteration runs the full `Regex::new` pipeline (parse -> optimize -> analyze ->
//! compile, including building the underlying regex-automata engines).
//!
//! For "easy" patterns that fancy-regex delegates wholesale to regex-automata, a
//! `regex::Regex::new` row is included as a side-by-side baseline so the
//! fancy-regex overhead on top of the engine build is visible.

#[macro_use]
extern crate criterion;

use criterion::{black_box, Criterion};

use fancy_regex::{Regex, RegexSet};
use regex::Regex as StdRegex;

// ---------------------------------------------------------------------------
// Corpus
// ---------------------------------------------------------------------------

/// Easy pattern, fully delegated (the same literal-heavy pattern used in `bench.rs`).
/// Exercises bottleneck #1 (re-parse on the easy/Wrap path).
const EASY_LITERAL: &str =
    r"^\\([!-/:-@\[-`\{-~aftnrv]|[0-7]{1,3}|x[0-9a-fA-F]{2}|x\{[0-9a-fA-F]{1,6}\})";

/// A second easy pattern resembling a real-world tokenizer rule.
const EASY_EMAIL: &str = r"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}";

/// Hard pattern (lookarounds + backreference) interleaved with easy runs, so the
/// compiler emits several `Insn::Delegate` engines. Exercises bottleneck #2.
const DELEGATE_HEAVY: &str = r"(\d{3})(?=x)[a-z]+(?<=ab)\w+\1[A-Z]{2}(?!q)\s+foo";

/// Build a pattern with many fancy-separated easy runs to stress the
/// "one meta engine per easy run" cost.
fn delegate_stress() -> String {
    let mut s = String::new();
    for _ in 0..20 {
        // each `[a-z]{2}` easy run is separated by a lookahead (a hard part),
        // forcing a separate delegate engine for each run.
        s.push_str("[a-z]{2}(?=x)");
    }
    s
}

/// A large alternation of literals — stays "easy" (Wrap), stressing the engine's
/// literal extraction / prefilter build on the top-level (unanchored) engine.
fn large_alternation() -> String {
    let mut s = String::from(r"\b(?:");
    for i in 0..100 {
        if i != 0 {
            s.push('|');
        }
        s.push_str("word");
        s.push_str(&i.to_string());
    }
    s.push_str(r")\b");
    s
}

/// Patterns for the RegexSet bench: a mix of easy and hard (fancy) patterns.
fn regexset_patterns() -> Vec<String> {
    vec![
        r"\d{4}-\d{2}-\d{2}".to_string(),
        r"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}".to_string(),
        r"(?<=\$)\d+(?:\.\d{2})?".to_string(),
        r"(\w+)\s+\1".to_string(),
        r"(?:https?|ftp)://\S+".to_string(),
        r"#[0-9a-fA-F]{6}\b".to_string(),
        r"foo(?=bar)".to_string(),
        r"\b[A-Z][a-z]+\b".to_string(),
    ]
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

fn compile_easy(c: &mut Criterion) {
    let mut group = c.benchmark_group("compile_easy");
    group.bench_function("fancy/literal", |b| {
        b.iter(|| Regex::new(black_box(EASY_LITERAL)).unwrap())
    });
    group.bench_function("regex/literal", |b| {
        b.iter(|| StdRegex::new(black_box(EASY_LITERAL)).unwrap())
    });
    group.bench_function("fancy/email", |b| {
        b.iter(|| Regex::new(black_box(EASY_EMAIL)).unwrap())
    });
    group.bench_function("regex/email", |b| {
        b.iter(|| StdRegex::new(black_box(EASY_EMAIL)).unwrap())
    });
    group.finish();
}

fn compile_large_alternation(c: &mut Criterion) {
    let pat = large_alternation();
    let mut group = c.benchmark_group("compile_large_alternation");
    group.bench_function("fancy", |b| b.iter(|| Regex::new(black_box(&pat)).unwrap()));
    group.bench_function("regex", |b| {
        b.iter(|| StdRegex::new(black_box(&pat)).unwrap())
    });
    group.finish();
}

fn compile_delegate_heavy(c: &mut Criterion) {
    let stress = delegate_stress();
    let mut group = c.benchmark_group("compile_delegate_heavy");
    // No regex baseline: these use fancy features regex-automata can't compile.
    group.bench_function("fancy/realistic", |b| {
        b.iter(|| Regex::new(black_box(DELEGATE_HEAVY)).unwrap())
    });
    group.bench_function("fancy/stress_20_delegates", |b| {
        b.iter(|| Regex::new(black_box(&stress)).unwrap())
    });
    group.finish();
}

fn compile_regexset(c: &mut Criterion) {
    let pats = regexset_patterns();
    let mut group = c.benchmark_group("compile_regexset");
    group.sample_size(30);
    group.bench_function("fancy_set_8", |b| {
        b.iter(|| RegexSet::new(black_box(&pats)).unwrap())
    });
    group.finish();
}

criterion_group!(
    name = compile_benches;
    config = Criterion::default();
    targets =
        compile_easy,
        compile_large_alternation,
        compile_delegate_heavy,
        compile_regexset,
);
criterion_main!(compile_benches);
