// Benchmark comparing word-boundary performance before/after the change
// that made \b hard only inside a lookbehind.
//
// Three classes of pattern are measured:
//   1. `\b(?:alt|...)\b`            — word boundary OUTSIDE a lookbehind.
//      Was hard (whole alternation on the VM, one branch per alt);
//      now easy (single delegated engine).
//   2. `(?<=\b)\w+\b`              — word boundary INSIDE a lookbehind.
//      Hard both before and after the change.
//   3. `(?<=x)\w+\b`               — a lookbehind with NO word boundary.
//      Hard both before and after; should be unchanged.
#[macro_use]
extern crate criterion;

use criterion::{black_box, criterion_group, Criterion};

use fancy_regex::Regex;

/// Word-boundary-anchored alternation, no lookbehind.
/// This is the case the change specifically optimized: before, the whole
/// alternation was compiled on the VM (one branch per alternative); after,
/// it delegates to a single NFA engine.
fn wb_alt_no_lookbehind(c: &mut Criterion) {
    // 64 alternatives, each a distinct 3-letter word.
    let alts: Vec<String> = (0..64).map(|i| format!("w{:03}", i)).collect();
    let pattern = format!("\\b(?:{})\\b", alts.join("|"));
    let re = Regex::new(&pattern).unwrap();
    // Haystack: mostly non-matching words, with a few matches sprinkled in.
    let mut haystack = String::new();
    for i in 0..2000 {
        if i % 17 == 0 {
            haystack.push_str("w005 ");
        } else {
            haystack.push_str(&format!("qqq{} ", i));
        }
    }
    c.bench_function("wb_alt_no_lookbehind_64", |b| {
        b.iter(|| {
            let count = re
                .find_iter(black_box(&haystack))
                .filter_map(Result::ok)
                .count();
            black_box(count)
        })
    });
}

/// Word boundary INSIDE a lookbehind: hard both before and after the change.
fn wb_inside_lookbehind(c: &mut Criterion) {
    let re = Regex::new(r"(?<=\b)\w+\b").unwrap();
    let haystack = "the quick brown fox jumps over the lazy dog ".repeat(500);
    c.bench_function("wb_inside_lookbehind", |b| {
        b.iter(|| {
            let count = re
                .find_iter(black_box(&haystack))
                .filter_map(Result::ok)
                .count();
            black_box(count)
        })
    });
}

/// Lookbehind with NO word boundary: hard both before and after; control case.
fn lookbehind_no_wb(c: &mut Criterion) {
    let re = Regex::new(r"(?<=x)\w+\b").unwrap();
    let haystack = "xthe quick brown fox jumps over the lazy dog ".repeat(500);
    c.bench_function("lookbehind_no_wb", |b| {
        b.iter(|| {
            let count = re
                .find_iter(black_box(&haystack))
                .filter_map(Result::ok)
                .count();
            black_box(count)
        })
    });
}

criterion_group!(
    name = wb_perf;
    config = Criterion::default().sample_size(30);
    targets = wb_alt_no_lookbehind,
    wb_inside_lookbehind,
    lookbehind_no_wb,
);
criterion_main!(wb_perf);
