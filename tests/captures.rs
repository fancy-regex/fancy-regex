extern crate fancy_regex;

use fancy_regex::Captures;

mod common;

#[test]
fn captures_after_lookbehind() {
    let captures = captures(
        r"\s*(?<=[() ])(@\w+)(\([^)]*\))?\s*",
        " @another(foo bar)   ",
    );
    assert_eq!(Some("@another"), captures.at(1));
    assert_eq!(Some("(foo bar)"), captures.at(2));
}

fn captures<'a>(re: &str, text: &'a str) -> Captures<'a> {
    let regex = common::regex(re);
    let result = regex.captures(text);
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
