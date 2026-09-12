#![cfg(feature = "leftmost_longest")]

use fancy_regex::{BytesMode, Regex, RegexBuilder};

#[cfg_attr(feature = "track_caller", track_caller)]
pub fn assert_match_longest(re: &str, text: &str, expected: &str) {
    let str_re = RegexBuilder::new(re)
        .leftmost_longest(true)
        .build()
        .expect("regex should compile");
    let str_result = str_re.find(text).unwrap();
    assert!(
        str_result.is_some(),
        "Expected regex '{}' to match '{}' in leftmost-longest mode",
        re,
        text
    );
    assert_eq!(
        str_result.unwrap().as_str(),
        expected,
        "Expected leftmost-longest match of '{}' on '{}' to be '{}'",
        re,
        text,
        expected
    );
}

#[test]
fn leftmost_longest_basic() {
    assert_match_longest(r"(a|ab)c", "xabc", "abc");
    assert_match_longest(r"(ab|a)c", "xabc", "abc");
}

#[test]
fn leftmost_longest_greedy_star() {
    assert_match_longest(r"a.*b", "xaabxb", "aabxb");
}

#[test]
fn leftmost_longest_tie() {
    assert_match_longest(r"a|a", "xa", "a");
}

#[test]
fn leftmost_longest_no_match() {
    let re = RegexBuilder::new(r"(a|ab)c")
        .leftmost_longest(true)
        .build()
        .unwrap();
    assert!(re.find("xbc").unwrap().is_none());
}

#[test]
fn leftmost_longest_non_greedy_error() {
    let result = RegexBuilder::new(r"a.*?(b)").leftmost_longest(true).build();
    assert!(result.is_err());
}

#[test]
fn leftmost_longest_alt_basic() {
    assert_match_longest(r"a|ab", "ab", "ab");
}

fn leftmost_longest_re(re: &str) -> Regex {
    RegexBuilder::new(re)
        .leftmost_longest(true)
        .build()
        .expect("regex should compile in leftmost-longest mode")
}

#[test]
fn leftmost_longest_iter_does_not_skip_matches() {
    let spans: Vec<(usize, usize)> = leftmost_longest_re(r"a|ab")
        .find_iter("abaa ab")
        .map(|m| {
            let m = m.unwrap();
            (m.start(), m.end())
        })
        .collect();
    assert_eq!(spans, vec![(0, 2), (2, 3), (3, 4), (5, 7)]);
}

#[test]
fn leftmost_longest_iter_longest_at_leftmost_position() {
    let spans: Vec<(usize, usize)> = leftmost_longest_re(r"a|ab|abc")
        .find_iter("xabcabc")
        .map(|m| {
            let m = m.unwrap();
            (m.start(), m.end())
        })
        .collect();
    assert_eq!(spans, vec![(1, 4), (4, 7)]);
}

#[test]
fn leftmost_longest_iter_order_independent() {
    let text = "abaa ab";
    for re in [r"a|ab", r"ab|a", r"a|ab|a"] {
        let spans: Vec<(usize, usize)> = RegexBuilder::new(re)
            .leftmost_longest(true)
            .build()
            .unwrap()
            .find_iter(text)
            .map(|m| {
                let m = m.unwrap();
                (m.start(), m.end())
            })
            .collect();
        assert_eq!(
            spans,
            vec![(0, 2), (2, 3), (3, 4), (5, 7)],
            "pattern {re:?} produced different leftmost-longest results"
        );
    }
}

#[test]
fn leftmost_longest_find_from_pos_does_not_skip() {
    let re = leftmost_longest_re(r"a|ab");
    let text = "abaa ab";
    assert_eq!(re.find_from_pos(text, 0).unwrap().unwrap().as_str(), "ab");
    assert_eq!(re.find_from_pos(text, 2).unwrap().unwrap().as_str(), "a");
    assert_eq!(re.find_from_pos(text, 3).unwrap().unwrap().as_str(), "a");
    assert_eq!(re.find_from_pos(text, 5).unwrap().unwrap().as_str(), "ab");
}

#[test]
fn leftmost_longest_captures_iter_does_not_skip() {
    let re = leftmost_longest_re(r"(a|ab)");
    let captures: Vec<(usize, usize)> = re
        .captures_iter("abaa ab")
        .map(|c| {
            let c = c.unwrap();
            (c.get(0).unwrap().start(), c.get(0).unwrap().end())
        })
        .collect();
    assert_eq!(captures, vec![(0, 2), (2, 3), (3, 4), (5, 7)]);
}

#[test]
fn leftmost_longest_iter_does_not_skip_bytes_mode() {
    let spans: Vec<(usize, usize)> = RegexBuilder::new(r"a|ab")
        .leftmost_longest(true)
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap()
        .find_iter(&b"abaa ab"[..])
        .map(|m| {
            let m = m.unwrap();
            (m.start(), m.end())
        })
        .collect();
    assert_eq!(spans, vec![(0, 2), (2, 3), (3, 4), (5, 7)]);
}

#[test]
fn leftmost_longest_iter_does_not_skip_with_seek() {
    // Seek mode and leftmost-longest mode should produce identical results
    // since the seek pre-filter does not affect leftmost-longest semantics.
    let text = "abaa ab";
    let seek_spans: Vec<(usize, usize)> = RegexBuilder::new(r"a|ab")
        .leftmost_longest(true)
        .seek(true)
        .build()
        .unwrap()
        .find_iter(text)
        .map(|m| {
            let m = m.unwrap();
            (m.start(), m.end())
        })
        .collect();
    assert_eq!(seek_spans, vec![(0, 2), (2, 3), (3, 4), (5, 7)]);
}
