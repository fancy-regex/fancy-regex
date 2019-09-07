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

use criterion::Criterion;

use fancy_regex::internal::{analyze, compile, run};
use fancy_regex::Expr;
use regex::Regex;

fn parse_lifetime_re(c: &mut Criterion) {
    c.bench_function("parse_lifetime_re", |b| {
        b.iter(|| Expr::parse("\\'[a-zA-Z_][a-zA-Z0-9_]*(?!\\')\\b").unwrap())
    });
}

fn parse_literal_re(c: &mut Criterion) {
    c.bench_function("parse_literal_re", |b| {
        b.iter(|| Expr::parse("^\\\\([!-/:-@\\[-`\\{-~aftnrv]|[0-7]{1,3}|x[0-9a-fA-F]{2}|x\\{[0-9a-fA-F]{1,6}\\})").unwrap())
    });
}

fn parse_literal_re_regex(c: &mut Criterion) {
    c.bench_function("parse_literal_re_regex", |b| {
        b.iter(|| Regex::new("^\\\\([!-/:-@\\[-`\\{-~aftnrv]|[0-7]{1,3}|x[0-9a-fA-F]{2}|x\\{[0-9a-fA-F]{1,6}\\})").unwrap())
    });
}

fn parse_misc(c: &mut Criterion) {
    c.bench_function("parse_misc", |b| {
        b.iter(|| Expr::parse("^\\p{L}|\\p{N}|\\s|.|\\d").unwrap())
    });
}

fn analyze_literal_re(c: &mut Criterion) {
    let re = "^\\\\([!-/:-@\\[-`\\{-~aftnrv]|[0-7]{1,3}|x[0-9a-fA-F]{2}|x\\{[0-9a-fA-F]{1,6}\\})";
    let (e, br) = Expr::parse(re).unwrap();
    c.bench_function("analyze_literal_re", |b| {
        b.iter(|| analyze(&e, &br).unwrap())
    });
}

fn run_backtrack(c: &mut Criterion) {
    let (e, br) = Expr::parse("^.*?(([ab]+)\\1b)").unwrap();
    let a = analyze(&e, &br).unwrap();
    let p = compile(&a).unwrap();
    c.bench_function("run_backtrack", |b| {
        b.iter(|| run(&p, "babab", 0, 0).unwrap())
    });
}

// The following regex is a pathological case for backtracking
// implementations, see README.md:
fn run_tricky(c: &mut Criterion) {
    let (e, br) = Expr::parse("(a|b|ab)*bc").unwrap();
    let a = analyze(&e, &br).unwrap();
    let p = compile(&a).unwrap();
    let mut s = String::new();
    for _ in 0..28 {
        s.push_str("ab");
    }
    s.push_str("ac");
    c.bench_function("run_tricky", |b| b.iter(|| run(&p, &s, 0, 0).unwrap()));
}

criterion_group!(
    benches,
    parse_lifetime_re,
    parse_literal_re,
    parse_literal_re_regex,
    parse_misc,
    analyze_literal_re,
    run_backtrack,
    run_tricky
);
criterion_main!(benches);
