use fancy_regex::{Captures, Match, Result};

mod common;

#[test]
fn captures_after_lookbehind() {
    let captures = captures(
        r"\s*(?<=[() ])(@\w+)(\([^)]*\))?\s*",
        " @another(foo bar)   ",
    );
    assert_match(captures.get(1), "@another", 1, 9);
    assert_match(captures.get(2), "(foo bar)", 9, 18);
}

#[test]
fn captures_from_pos() {
    let regex = common::regex(r"(\d+)(?!a)");
    let captures = assert_captures(regex.captures_from_pos("1b 2a 3b", 3));

    assert_match(captures.get(0), "3", 6, 7);
    assert_match(captures.get(1), "3", 6, 7);
}

fn captures<'a>(re: &str, text: &'a str) -> Captures<'a> {
    let regex = common::regex(re);
    let result = regex.captures(text);
    assert_captures(result)
}

fn assert_captures(result: Result<Option<Captures<'_>>>) -> Captures<'_> {
    assert!(
        result.is_ok(),
        "Expected captures to succeed, but was {:?}",
        result
    );
    let captures = result.unwrap();
    assert!(
        captures.is_some(),
        "Expected captures, but was {:?}",
        captures
    );
    captures.unwrap()
}

fn assert_match(m: Option<Match<'_>>, expected_text: &str, start: usize, end: usize) {
    assert!(m.is_some(), "Expected match, but was {:?}", m);
    let m = m.unwrap();
    assert_eq!(m.as_str(), expected_text);
    assert_eq!(m.start(), start);
    assert_eq!(m.end(), end);
}
