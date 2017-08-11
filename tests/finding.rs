extern crate fancy_regex;

mod common;

#[test]
fn lookahead_anchoring() {
    assert_eq!(find(r"(?=x|a)", "a"), Some((0, 0)));
    assert_eq!(find(r"(?=x|a)", "bbba"), Some((3, 3)));
}

#[test]
fn lookbehind_anchoring() {
    assert_eq!(find(r"(?<=x|a)", "a"), Some((1, 1)));
    assert_eq!(find(r"(?<=x|a)", "ba"), Some((2, 2)));
    assert_eq!(find(r"(?<=^a)", "a"), Some((1, 1)));
    assert_eq!(find(r"(?<=^a)", "ba"), None);
}

fn find(re: &str, text: &str) -> Option<(usize, usize)> {
    let regex = common::regex(re);
    let result = regex.find(text);
    assert!(result.is_ok(), "Expected find to succeed, but was {:?}", result);
    result.unwrap()
}
