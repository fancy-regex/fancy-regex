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

#[macro_use]
extern crate criterion;

use criterion::{black_box, Criterion};
use std::time::Duration;

use fancy_regex::internal::{
    analyze, compile, optimize, run_default, AnalyzeContext, CompileOptions,
};
use fancy_regex::seek_pattern_is_useful;
use fancy_regex::Expr;
use fancy_regex::Regex as FancyRegex;
use fancy_regex::RegexOptionsBuilder;
use regex::Regex;

fn parse_lifetime_re(c: &mut Criterion) {
    c.bench_function("parse_lifetime_re", |b| {
        b.iter(|| Expr::parse_tree("\\'[a-zA-Z_][a-zA-Z0-9_]*(?!\\')\\b").unwrap())
    });
}

fn parse_literal_re(c: &mut Criterion) {
    c.bench_function("parse_literal_re", |b| {
        b.iter(|| Expr::parse_tree("^\\\\([!-/:-@\\[-`\\{-~aftnrv]|[0-7]{1,3}|x[0-9a-fA-F]{2}|x\\{[0-9a-fA-F]{1,6}\\})").unwrap())
    });
}

fn parse_literal_re_regex(c: &mut Criterion) {
    c.bench_function("parse_literal_re_regex", |b| {
        b.iter(|| Regex::new("^\\\\([!-/:-@\\[-`\\{-~aftnrv]|[0-7]{1,3}|x[0-9a-fA-F]{2}|x\\{[0-9a-fA-F]{1,6}\\})").unwrap())
    });
}

fn parse_misc(c: &mut Criterion) {
    c.bench_function("parse_misc", |b| {
        b.iter(|| Expr::parse_tree("^\\p{L}|\\p{N}|\\s|.|\\d").unwrap())
    });
}

fn analyze_literal_re(c: &mut Criterion) {
    let re = "^\\\\([!-/:-@\\[-`\\{-~aftnrv]|[0-7]{1,3}|x[0-9a-fA-F]{2}|x\\{[0-9a-fA-F]{1,6}\\})";
    let tree = Expr::parse_tree(re).unwrap();
    c.bench_function("analyze_literal_re", |b| {
        b.iter(|| analyze(&tree, AnalyzeContext::default()).unwrap())
    });
}

fn run_backtrack(c: &mut Criterion) {
    let tree = Expr::parse_tree("^.*?(([ab]+)\\1b)").unwrap();
    let a = analyze(
        &tree,
        AnalyzeContext {
            explicit_capture_group_0: true,
            ..Default::default()
        },
    )
    .unwrap();
    let p = compile(
        &a,
        CompileOptions {
            anchored: true,
            contains_subroutines: tree.contains_subroutines,
            ..CompileOptions::default()
        },
    )
    .unwrap();
    c.bench_function("run_backtrack", |b| {
        b.iter(|| {
            let result = run_default(&p, "babab", 0).unwrap();
            assert_eq!(result, Some(vec![0, 5, 0, 2]));
            return result;
        })
    });
}

// The following regex is a pathological case for backtracking
// implementations, see README.md:
fn run_tricky(c: &mut Criterion) {
    let tree = Expr::parse_tree("(a|b|ab)*bc").unwrap();
    let a = analyze(&tree, AnalyzeContext::default()).unwrap();
    let p = compile(
        &a,
        CompileOptions {
            contains_subroutines: tree.contains_subroutines,
            ..CompileOptions::default()
        },
    )
    .unwrap();
    let mut s = String::new();
    for _ in 0..28 {
        s.push_str("ab");
    }
    s.push_str("ac");
    c.bench_function("run_tricky", |b| b.iter(|| run_default(&p, &s, 0).unwrap()));
}

fn run_class_seq(c: &mut Criterion) {
    fn compile_pattern(pattern: &str) -> fancy_regex::internal::Prog {
        let tree = Expr::parse_tree(pattern).unwrap();
        let analysis = analyze(&tree, AnalyzeContext::default()).unwrap();
        compile(
            &analysis,
            CompileOptions {
                anchored: true,
                contains_subroutines: tree.contains_subroutines,
                ..CompileOptions::default()
            },
        )
        .unwrap()
    }

    // The successful hard lookahead makes the easy suffix a VM delegate on
    // main and a ClassSeq on this branch, exercising the production path the
    // optimization replaces.
    let plain = compile_pattern(r"(?=.)[^\w]return");
    let captures = compile_pattern(r"(?=.)\s*(async)?");
    let mut group = c.benchmark_group("run_class_seq");
    group.bench_function("plain", |b| {
        b.iter(|| run_default(&plain, black_box("#return"), 0).unwrap())
    });
    group.bench_function("plain_miss", |b| {
        b.iter(|| run_default(&plain, black_box("#returx"), 0).unwrap())
    });
    group.bench_function("captures_optional", |b| {
        b.iter(|| run_default(&captures, black_box("  async"), 0).unwrap())
    });
    let rollback = compile_pattern(r"(?=.)(a)?a");
    group.bench_function("optional_rollback", |b| {
        b.iter(|| run_default(&rollback, black_box("a"), 0).unwrap())
    });
    group.finish();
}

fn run_backtrack_limit(c: &mut Criterion) {
    let tree = Expr::parse_tree("(?i)(a|b|ab)*(?>c)").unwrap();
    let a = analyze(&tree, AnalyzeContext::default()).unwrap();
    let p = compile(
        &a,
        CompileOptions {
            contains_subroutines: tree.contains_subroutines,
            ..CompileOptions::default()
        },
    )
    .unwrap();
    let s = "abababababababababababababababababababababababababababab";
    c.bench_function("run_backtrack_limit", |b| {
        b.iter(|| run_default(&p, &s, 0).unwrap_err())
    });
}

#[cfg(feature = "variable-lookbehinds")]
fn const_size_lookbehind(c: &mut Criterion) {
    // Benchmark const-size lookbehind (should use simple GoBack)
    let tree = Expr::parse_tree(r"(?<=ab)x").unwrap();
    let a = analyze(&tree, AnalyzeContext::default()).unwrap();
    let p = compile(
        &a,
        CompileOptions {
            contains_subroutines: tree.contains_subroutines,
            ..CompileOptions::default()
        },
    )
    .unwrap();
    let input = "abx";
    c.bench_function("const_size_lookbehind", |b| {
        b.iter(|| run_default(&p, input, 0).unwrap())
    });
}

#[cfg(feature = "variable-lookbehinds")]
fn variable_size_lookbehind(c: &mut Criterion) {
    // Benchmark variable-size lookbehind (uses reverse DFA)
    let tree = Expr::parse_tree(r"(?<=a+b+)x").unwrap();
    let a = analyze(&tree, AnalyzeContext::default()).unwrap();
    let p = compile(
        &a,
        CompileOptions {
            contains_subroutines: tree.contains_subroutines,
            ..CompileOptions::default()
        },
    )
    .unwrap();
    let input = "aaabbbbx";
    c.bench_function("variable_size_lookbehind", |b| {
        b.iter(|| run_default(&p, input, 0).unwrap())
    });
}

#[cfg(feature = "variable-lookbehinds")]
fn variable_size_alt_lookbehind(c: &mut Criterion) {
    // Benchmark variable-size lookbehind with alternation
    let tree = Expr::parse_tree(r"(?<=a|bc)x").unwrap();
    let a = analyze(&tree, AnalyzeContext::default()).unwrap();
    let p = compile(
        &a,
        CompileOptions {
            contains_subroutines: tree.contains_subroutines,
            ..CompileOptions::default()
        },
    )
    .unwrap();
    let input = "bcx";
    c.bench_function("variable_size_alt_lookbehind", |b| {
        b.iter(|| run_default(&p, input, 0).unwrap())
    });
}

/// The 16-branch JSX tag-start guard from the TypeScript TextMate grammar.
/// The pattern is open, so the lookbehind is attempted at almost every position.
#[cfg(feature = "variable-lookbehinds")]
const LOOKBEHIND_ALT_WIDE: &str = r"(?<!\+\+|--)(?<=[(*,:=>?\[{]|&&|\|\||\?|\*/|^await|[^$._[:alnum:]]await|^return|[^$._[:alnum:]]return|^default|[^$._[:alnum:]]default|^yield|[^$._[:alnum:]]yield|^)\s*<[a-z]+";

#[cfg(feature = "variable-lookbehinds")]
fn lookbehind_alternation(c: &mut Criterion) {
    let wide = FancyRegex::new(LOOKBEHIND_ALT_WIDE).unwrap();
    let wide_hay =
        "  return <div>\n    {items.map((x) => <li key={x}>{x}</li>)}\n  </div>;\n".repeat(200);
    c.bench_function("lookbehind_alternation_wide_16", |b| {
        b.iter(|| wide.find_iter(&wide_hay).count())
    });
}

#[cfg(feature = "variable-lookbehinds")]
criterion_group!(
    name = lookbehind_benches;
    config = Criterion::default();
    targets = const_size_lookbehind,
    variable_size_lookbehind,
    variable_size_alt_lookbehind,
    lookbehind_alternation,
);

criterion_group!(
    name = benches;
    config = Criterion::default().warm_up_time(Duration::from_secs(10));
    targets = parse_lifetime_re,
    parse_literal_re,
    parse_literal_re_regex,
    parse_misc,
    analyze_literal_re,
    run_backtrack,
    run_tricky,
    run_class_seq,
);
criterion_group!(
    name = slow_benches;
    config = Criterion::default().sample_size(10);
    targets = run_backtrack_limit,
);

fn continue_from_end_of_prev_match_short_haystack(c: &mut Criterion) {
    // Benchmark \G with a short haystack that doesn't match
    let tree = Expr::parse_tree(r"\Gfoo").unwrap();
    let a = analyze(&tree, AnalyzeContext::default()).unwrap();
    let p = compile(
        &a,
        CompileOptions {
            contains_subroutines: tree.contains_subroutines,
            ..CompileOptions::default()
        },
    )
    .unwrap();
    let input = "bar"; // 3 bytes, doesn't match
    c.bench_function("continue_from_end_of_prev_match_short_haystack", |b| {
        b.iter(|| run_default(&p, input, 0).unwrap())
    });
}

fn continue_from_end_of_prev_match_long_haystack(c: &mut Criterion) {
    // Benchmark \G with a long haystack that doesn't match
    let tree = Expr::parse_tree(r"\Gfoo").unwrap();
    let a = analyze(&tree, AnalyzeContext::default()).unwrap();
    let p = compile(
        &a,
        CompileOptions {
            contains_subroutines: tree.contains_subroutines,
            ..CompileOptions::default()
        },
    )
    .unwrap();
    let mut input = String::new();
    for _ in 0..10000 {
        input.push('x');
    }
    c.bench_function("continue_from_end_of_prev_match_long_haystack", |b| {
        b.iter(|| run_default(&p, &input, 0).unwrap())
    });
}

criterion_group!(
    name = continue_from_end_of_prev_match_benches;
    config = Criterion::default();
    targets = continue_from_end_of_prev_match_short_haystack,
    continue_from_end_of_prev_match_long_haystack,
);

/// Shared logic for the backref-in-long-haystack seek benchmarks.
///
/// Compiles `(abc)\1` with the given `seek` setting, builds a 10,000-`x` haystack
/// (optionally with `"abcabc"` appended), and registers a criterion benchmark under
/// `name`.
fn bench_backref_in_long_haystack(c: &mut Criterion, name: &str, seek: bool, with_match: bool) {
    // Pattern with a backref — when seek is true the approximation inlines the group
    // body ("abc"), so the engine jumps directly to "abc" occurrences and skips the rest.
    let tree = Expr::parse_tree(r"(abc)\1").unwrap();
    let a = analyze(&tree, AnalyzeContext::default()).unwrap();
    let p = compile(
        &a,
        CompileOptions {
            contains_subroutines: tree.contains_subroutines,
            seek_filter: if seek {
                Some(seek_pattern_is_useful)
            } else {
                None
            },
            ..CompileOptions::default()
        },
    )
    .unwrap();
    let mut haystack = String::new();
    for _ in 0..10_000 {
        haystack.push('x');
    }
    if with_match {
        // Place the match target at the very end so the seek pre-filter must scan
        // the entire haystack before finding the single candidate position.
        haystack.push_str("abcabc");
    }
    c.bench_function(name, |b| b.iter(|| run_default(&p, &haystack, 0).unwrap()));
}

fn seek_backref_in_long_haystack(c: &mut Criterion) {
    bench_backref_in_long_haystack(c, "seek_backref_in_long_haystack", true, true);
}

fn seek_backref_in_long_haystack_no_match(c: &mut Criterion) {
    bench_backref_in_long_haystack(c, "seek_backref_in_long_haystack_no_match", true, false);
}

fn no_seek_backref_in_long_haystack(c: &mut Criterion) {
    bench_backref_in_long_haystack(c, "no_seek_backref_in_long_haystack", false, true);
}

fn no_seek_backref_in_long_haystack_no_match(c: &mut Criterion) {
    bench_backref_in_long_haystack(c, "no_seek_backref_in_long_haystack_no_match", false, false);
}

criterion_group!(
    name = seek_benches;
    config = Criterion::default();
    targets = seek_backref_in_long_haystack,
    seek_backref_in_long_haystack_no_match,
    no_seek_backref_in_long_haystack,
    no_seek_backref_in_long_haystack_no_match,
);

/// Shared logic for the worst-case seek benchmarks.
///
/// Compiles `(\d{3})\1` with the given `seek` setting and builds a haystack of
/// `"1234567890"` repeated 100 times (optionally followed by `"00000"` to place a
/// match at the very end).  Because every position in the all-digits haystack is
/// a candidate for `\d{3}`, the seek pre-filter cannot skip any positions — it
/// becomes pure overhead compared to running without seek.
fn bench_digit_backref_worst_case(c: &mut Criterion, name: &str, seek: bool, with_match: bool) {
    let tree = Expr::parse_tree(r"(\d{3})\1").unwrap();
    let a = analyze(&tree, AnalyzeContext::default()).unwrap();
    let p = compile(
        &a,
        CompileOptions {
            contains_subroutines: tree.contains_subroutines,
            seek_filter: if seek {
                Some(seek_pattern_is_useful)
            } else {
                None
            },
            ..CompileOptions::default()
        },
    )
    .unwrap();
    let mut haystack = "1234567890".repeat(100);
    if with_match {
        // Append 5 zeros so the string ends with 6 zeros giving a match ("000" + "000")
        // near the very end, forcing the seek pre-filter to traverse the whole haystack.
        haystack.push_str("00000");
    }
    c.bench_function(name, |b| b.iter(|| run_default(&p, &haystack, 0).unwrap()));
}

fn seek_digit_backref_worst_case(c: &mut Criterion) {
    bench_digit_backref_worst_case(c, "seek_digit_backref_worst_case", true, true);
}

fn seek_digit_backref_worst_case_no_match(c: &mut Criterion) {
    bench_digit_backref_worst_case(c, "seek_digit_backref_worst_case_no_match", true, false);
}

fn no_seek_digit_backref_worst_case(c: &mut Criterion) {
    bench_digit_backref_worst_case(c, "no_seek_digit_backref_worst_case", false, true);
}

fn no_seek_digit_backref_worst_case_no_match(c: &mut Criterion) {
    bench_digit_backref_worst_case(c, "no_seek_digit_backref_worst_case_no_match", false, false);
}

criterion_group!(
    name = seek_worst_case_benches;
    config = Criterion::default();
    targets = seek_digit_backref_worst_case,
    seek_digit_backref_worst_case_no_match,
    no_seek_digit_backref_worst_case,
    no_seek_digit_backref_worst_case_no_match,
);

/// A parenthesised tuple on the left-hand side of an assignment, e.g. `(a, b) = ...`.
const SEEK_RECURSION: &str = r"(?<tuple>\((?:[^()]|\g<tuple>)+\))\s*(?!=[=>])(?==)";

/// How well the seek approximation of a recursive subroutine filters candidate positions.
/// `nested_near_miss` is the interesting one: `(?!=[=>])` makes every line a near miss, so
/// each candidate the pre-filter lets through costs a failed VM attempt plus another seek.
fn seek_recursion(c: &mut Criterion) {
    let lines = 200;
    let haystacks = [
        ("assignments", "(alpha, beta) = compute(x)\n", lines),
        ("calls_only", "result = compute(alpha, beta);\n", 0),
        (
            "nested_near_miss",
            "outer(inner(deep(alpha)), beta) == other(gamma);\n",
            0,
        ),
        (
            "no_parens",
            "the quick brown fox jumps over the lazy dog ",
            0,
        ),
    ];
    let mut group = c.benchmark_group("seek_recursion");
    for (name, line, expected) in haystacks {
        let haystack = line.repeat(lines);
        for (label, seek) in [("no_seek", false), ("seek", true)] {
            let mut options = RegexOptionsBuilder::new();
            options.seek(seek);
            let re = options.build(SEEK_RECURSION.to_string()).unwrap();
            group.bench_function(format!("{name}/{label}"), |b| {
                b.iter(|| {
                    let count = re.find_iter(&haystack).filter_map(Result::ok).count();
                    assert_eq!(count, expected);
                    count
                })
            });
        }
    }
    group.finish();
}

criterion_group!(
    name = seek_recursion_benches;
    config = Criterion::default();
    targets = seek_recursion,
);

/// Shared logic for the case-insensitive Unicode backref benchmarks.
///
/// `(?i)(\p{Greek}+) \1` forces every backref comparison to go through the
/// non-ASCII case-folding path (`matches_literal_casei_unicode`). The haystack
/// is a run of Greek word pairs that never match the backref, optionally
/// followed by one case-folded pair at the very end, so the VM performs many
/// folded comparisons per iteration.
fn bench_casei_unicode_backref(c: &mut Criterion, name: &str, with_match: bool) {
    let re = FancyRegex::new(r"(?i)(\p{Greek}+) \1").unwrap();
    // The two words share no letters, so no suffix-of-one / prefix-of-the-next
    // can satisfy the backref and the pattern never matches within the run.
    let mut haystack = "αβγδ εζηθ ".repeat(20);
    if with_match {
        haystack.push_str("ωμεγα ΩΜΕΓΑ");
    }
    c.bench_function(name, |b| {
        b.iter(|| {
            let found = re.find(&haystack).unwrap();
            assert_eq!(found.is_some(), with_match);
            found
        })
    });
}

fn casei_unicode_backref(c: &mut Criterion) {
    bench_casei_unicode_backref(c, "casei_unicode_backref", true);
}

fn casei_unicode_backref_no_match(c: &mut Criterion) {
    bench_casei_unicode_backref(c, "casei_unicode_backref_no_match", false);
}

/// `find_iter` over a hard (VM-executed) pattern with many matches, to expose
/// the per-`run` scratch allocation overhead paid on every iteration step.
fn find_iter_fancy_many_matches(c: &mut Criterion) {
    let re = FancyRegex::new(r"(?<=@)\w+").unwrap();
    let haystack = "user@alpha beta@gamma ".repeat(500);
    c.bench_function("find_iter_fancy_many_matches", |b| {
        b.iter(|| {
            let count = re.find_iter(&haystack).filter_map(Result::ok).count();
            assert_eq!(count, 1000);
            count
        })
    });
}

/// `find_iter` over an easy pattern that `optimize()` rewrites with an explicit
/// capture group 0 (trailing lookahead). The Wrap fast path currently allocates
/// full captures per step just to read group 1's span.
fn find_iter_wrap_explicit_group0(c: &mut Criterion) {
    let re = FancyRegex::new(r"\w+(?=!)").unwrap();
    let haystack = "hello! world? foo! bar ".repeat(500);
    c.bench_function("find_iter_wrap_explicit_group0", |b| {
        b.iter(|| {
            let count = re.find_iter(&haystack).filter_map(Result::ok).count();
            assert_eq!(count, 1000);
            count
        })
    });
}

/// `is_match` on a hard pattern; the VM allocates and discards its saves
/// vector on every call.
fn is_match_fancy(c: &mut Criterion) {
    let re = FancyRegex::new(r"\b(\w{3})\1\b").unwrap();
    let haystack = "one two three four five six seven abcabc";
    c.bench_function("is_match_fancy", |b| {
        b.iter(|| {
            let matched = re.is_match(haystack).unwrap();
            assert!(matched);
            matched
        })
    });
}

criterion_group!(
    name = api_benches;
    config = Criterion::default().sample_size(30);
    targets = casei_unicode_backref,
    casei_unicode_backref_no_match,
    find_iter_fancy_many_matches,
    find_iter_wrap_explicit_group0,
    is_match_fancy,
);

/// Shared logic for the optimized-vs-unoptimized concat-repeat benchmarks.
///
/// Compiles `(\w)(?:\s*\w?\s*)+\1` with or without calling `optimize()` on
/// the parsed tree. The backreference makes the whole pattern hard, so the
/// VM executes the ambiguous `(?:\s*\w?\s*)+` middle section. With
/// optimization, that section is rewritten to the unambiguous `\s*(?:\w\s*)*`,
/// reducing the number of backtracking paths the VM must explore.
fn bench_concat_repeat_optimization(c: &mut Criterion, name: &str, with_optimize: bool) {
    let pattern = r"(\w)(?:\s*\w?\s*)+\1";
    let mut tree = Expr::parse_tree(pattern).unwrap();
    if with_optimize {
        optimize(&mut tree);
    }
    let a = analyze(
        &tree,
        AnalyzeContext {
            explicit_capture_group_0: false,
            ..Default::default()
        },
    )
    .unwrap();
    let p = compile(
        &a,
        CompileOptions {
            contains_subroutines: tree.contains_subroutines,
            ..CompileOptions::default()
        },
    )
    .unwrap();
    // Haystack: 20 repetitions of "x " then a final "x" — the whole string
    // matches because the captured 'x' equals the final \1.
    let mut haystack = "x ".repeat(20);
    haystack.push('x');
    c.bench_function(name, |b| b.iter(|| run_default(&p, &haystack, 0).unwrap()));
}

fn run_concat_repeat_optimized(c: &mut Criterion) {
    bench_concat_repeat_optimization(c, "run_concat_repeat_optimized", true);
}

fn run_concat_repeat_unoptimized(c: &mut Criterion) {
    bench_concat_repeat_optimization(c, "run_concat_repeat_unoptimized", false);
}

criterion_group!(
    name = optimize_benches;
    config = Criterion::default();
    targets = run_concat_repeat_optimized,
    run_concat_repeat_unoptimized,
);

#[cfg(feature = "variable-lookbehinds")]
criterion_main!(
    benches,
    slow_benches,
    lookbehind_benches,
    continue_from_end_of_prev_match_benches,
    seek_benches,
    seek_worst_case_benches,
    seek_recursion_benches,
    api_benches,
    optimize_benches
);

#[cfg(not(feature = "variable-lookbehinds"))]
criterion_main!(
    benches,
    slow_benches,
    continue_from_end_of_prev_match_benches,
    seek_benches,
    seek_worst_case_benches,
    seek_recursion_benches,
    api_benches,
    optimize_benches
);
