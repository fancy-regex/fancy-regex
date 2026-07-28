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

//! Measures the memory cost of compiling patterns, via a counting global
//! allocator. For each pattern it reports:
//!
//! * `peak`     – the high-water mark of *live* bytes during the compile
//!                (transient working set).
//! * `retained` – bytes still live after the compile, i.e. the steady-state
//!                size of the finished `Regex` / `RegexSet`.
//! * `total`    – cumulative bytes requested during the compile (churn).
//!
//! Run with: `cargo run --release --example compile_mem`

use std::alloc::{GlobalAlloc, Layout, System};
use std::hint::black_box;
use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};

use fancy_regex::{Regex, RegexSet};

struct Tracking;

static LIVE: AtomicUsize = AtomicUsize::new(0);
static PEAK: AtomicUsize = AtomicUsize::new(0);
static TOTAL: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for Tracking {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ptr = System.alloc(layout);
        if !ptr.is_null() {
            let size = layout.size();
            TOTAL.fetch_add(size, Relaxed);
            let live = LIVE.fetch_add(size, Relaxed) + size;
            // Bump the peak if we exceeded it (best-effort, racy under threads
            // but this example is single-threaded during measurement).
            PEAK.fetch_max(live, Relaxed);
        }
        ptr
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        System.dealloc(ptr, layout);
        LIVE.fetch_sub(layout.size(), Relaxed);
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        let new_ptr = System.realloc(ptr, layout, new_size);
        if !new_ptr.is_null() {
            let old = layout.size();
            if new_size >= old {
                let delta = new_size - old;
                TOTAL.fetch_add(delta, Relaxed);
                let live = LIVE.fetch_add(delta, Relaxed) + delta;
                PEAK.fetch_max(live, Relaxed);
            } else {
                LIVE.fetch_sub(old - new_size, Relaxed);
            }
        }
        new_ptr
    }
}

#[global_allocator]
static ALLOC: Tracking = Tracking;

/// Run `build` once with allocation tracking and print a labelled report.
/// The returned value is held until after measurement so `retained` reflects
/// the steady-state size of the compiled object.
fn measure<T>(label: &str, build: impl FnOnce() -> T) {
    let live_before = LIVE.load(Relaxed);
    let total_before = TOTAL.load(Relaxed);
    PEAK.store(live_before, Relaxed);

    let held = black_box(build());

    let peak = PEAK.load(Relaxed).saturating_sub(live_before);
    let retained = LIVE.load(Relaxed).saturating_sub(live_before);
    let total = TOTAL.load(Relaxed) - total_before;

    println!(
        "{:<28} peak {:>9} B   retained {:>9} B   total {:>9} B",
        label, peak, retained, total
    );

    drop(held);
}

const EASY_LITERAL: &str =
    r"^\\([!-/:-@\[-`\{-~aftnrv]|[0-7]{1,3}|x[0-9a-fA-F]{2}|x\{[0-9a-fA-F]{1,6}\})";
const EASY_EMAIL: &str = r"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}";
const DELEGATE_HEAVY: &str = r"(\d{3})(?=x)[a-z]+(?<=ab)\w+\1[A-Z]{2}(?!q)\s+foo";

fn delegate_stress() -> String {
    let mut s = String::new();
    for _ in 0..20 {
        s.push_str("[a-z]{2}(?=x)");
    }
    s
}

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

fn main() {
    // Warm up any lazy globals so they don't pollute the first measurement.
    drop(black_box(Regex::new("a").unwrap()));

    let stress = delegate_stress();
    let large_alt = large_alternation();
    let set_pats = regexset_patterns();

    println!("Compilation memory cost (release build recommended):\n");
    measure("easy/literal", || Regex::new(EASY_LITERAL).unwrap());
    measure("easy/email", || Regex::new(EASY_EMAIL).unwrap());
    measure("large_alternation", || Regex::new(&large_alt).unwrap());
    measure("delegate_heavy/realistic", || {
        Regex::new(DELEGATE_HEAVY).unwrap()
    });
    measure("delegate_heavy/stress20", || Regex::new(&stress).unwrap());
    measure("regexset/8", || RegexSet::new(&set_pats).unwrap());
}
