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

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};

use fancy_regex::{Input, RegexBuilder, RegexInput, RegexSet};

fn scan_with_regexset(regex_set: &RegexSet, haystack: &str) -> usize {
    let mut count = 0usize;
    let mut pos = 0usize;
    let base_input = RegexInput::new(haystack);

    while pos < haystack.len() {
        let input = base_input.clone().from_pos(pos);
        let next_pos_if_empty_match = input.haystack().advance_position(pos);
        let Some(mut matches) = regex_set.find_input(input).unwrap() else {
            break;
        };
        let first_match = matches.next().unwrap().unwrap();
        count += 1;
        pos = if first_match.end() > pos {
            first_match.end()
        } else {
            next_pos_if_empty_match
        };
    }

    count
}

fn scan_with_individual_regexes(regexes: &[fancy_regex::Regex], haystack: &str) -> usize {
    let mut count = 0usize;
    let mut pos = 0usize;
    let mut cached_matches: Vec<Option<(usize, usize)>> = vec![None; regexes.len()];

    while pos < haystack.len() {
        let mut best: Option<(usize, usize, usize)> = None;

        for (pattern_index, re) in regexes.iter().enumerate() {
            let matched = match cached_matches[pattern_index] {
                Some((start, end)) if start >= pos => Some((start, end)),
                _ => {
                    let next = re
                        .find_from_pos(haystack, pos)
                        .unwrap()
                        .map(|matched| (matched.start(), matched.end()));
                    cached_matches[pattern_index] = next;
                    next
                }
            };
            let Some((matched_start, matched_end)) = matched else {
                continue;
            };
            let candidate = (matched_start, pattern_index, matched_end);
            if best
                .as_ref()
                .map_or(true, |(best_start, best_pattern_index, _)| {
                    candidate.0 < *best_start
                        || (candidate.0 == *best_start && candidate.1 < *best_pattern_index)
                })
            {
                best = Some(candidate);
            }
            // No later pattern can start earlier than pos, and a higher pattern index
            // can't beat the current best at the same position, so stop searching.
            if best
                .as_ref()
                .map_or(false, |&(best_start, _, _)| best_start == pos)
            {
                break;
            }
        }

        let Some((_, _, best_end)) = best else {
            break;
        };

        count += 1;
        pos = if best_end > pos { best_end } else { pos + 1 };
    }

    count
}

fn run_scenario_bench(c: &mut Criterion, scenario_name: &str, patterns: &[&str], haystack: &str) {
    let regex_set = RegexSet::new(patterns.iter().copied()).unwrap();
    let regexes: Vec<fancy_regex::Regex> = patterns
        .iter()
        .map(|pattern| RegexBuilder::new(pattern).seek(true).build().unwrap())
        .collect();

    let mut group = c.benchmark_group(scenario_name);
    group.bench_function(BenchmarkId::new("regexset", patterns.len()), |b| {
        b.iter(|| black_box(scan_with_regexset(&regex_set, black_box(haystack))))
    });
    group.bench_function(BenchmarkId::new("individual", patterns.len()), |b| {
        b.iter(|| black_box(scan_with_individual_regexes(&regexes, black_box(haystack))))
    });
    group.finish();
}

fn bench_regexset_easy(c: &mut Criterion) {
    let patterns = [
        r"//[^\n]*",
        r#""(?:[^"\\]|\\.)*""#,
        r"\b(?:fn|let|mut|if|else|return)\b",
        r"\b[0-9]+\b",
        r"[A-Za-z_][A-Za-z0-9_]*",
        r"\s+",
        r"[{}();=,+\-]",
        r"\b[A-F0-9]{8}\b",
    ];
    let line = r#"let value = 42; fn run() { return value + 1; } // comment "string" DEADBEEF"#;
    let haystack = line.repeat(250);
    run_scenario_bench(c, "regexset_vs_individual_easy", &patterns, &haystack);
}

fn bench_regexset_hard(c: &mut Criterion) {
    let patterns = [
        r"(?<=\$)\d+(?:\.\d{2})?",
        r"(?<=\bfoo)bar",
        r"\b([A-Za-z]{2,8})-\1\b",
        r"\b([0-9]{2})\1\b",
        r"(?<=ID:)[A-Z]{2}[0-9]{4}",
    ];
    let line = "foo bar foofoobar $19.99 test-test 1212 ID:AB1234 xx yy-yy 3434 foo bar ";
    let haystack = line.repeat(400);
    run_scenario_bench(c, "regexset_vs_individual_hard", &patterns, &haystack);
}

fn bench_regexset_mixed(c: &mut Criterion) {
    let patterns = [
        r"//[^\n]*",
        r#""(?:[^"\\]|\\.)*""#,
        r"\b(?:fn|let|mut|if|else|return)\b",
        r"\b[0-9]+\b",
        r"[A-Za-z_][A-Za-z0-9_]*",
        r"(?<=\$)\d+(?:\.\d{2})?",
        r"\b([A-Za-z]{2,8})-\1\b",
        r"(?<=ID:)[A-Z]{2}[0-9]{4}",
    ];
    let line = r#"let test = ID:AB1234; if foofoo == 1212 { return "$29.95"; } // note test-test "#;
    let haystack = line.repeat(300);
    run_scenario_bench(c, "regexset_vs_individual_mixed", &patterns, &haystack);
}

criterion_group!(
    regexset_vs_individual_benches,
    bench_regexset_easy,
    bench_regexset_hard,
    bench_regexset_mixed
);
criterion_main!(regexset_vs_individual_benches);
