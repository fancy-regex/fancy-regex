extern crate fancy_regex;

mod common;

use fancy_regex::Regex;

#[test]
fn lookahead_grouping_single_expression() {
    // These would fail if the delegate expression was `^x|a` (if we didn't
    // group as `^(?:x|a)`).
    assert_eq!(find(r"(?=x|a)", "a"), Some((0, 0)));
    assert_eq!(find(r"(?=x|a)", "bbba"), Some((3, 3)));
}

#[test]
fn lookahead_grouping_multiple_expressions() {
    // These would fail if the delegate expression was `^ab|Bc` (if we didn't
    // preserve grouping of `(?:b|B)`).
    assert_eq!(find(r"(?=(?!x)a(?:b|B)c)", "aBc"), Some((0, 0)));
    assert_eq!(find(r"(?=(?!x)a(?:b|B)c)", "Bc"), None);
}

#[test]
fn lookbehind_grouping_single_expression() {
    assert_eq!(find(r"(?<=x|a)", "a"), Some((1, 1)));
    assert_eq!(find(r"(?<=x|a)", "ba"), Some((2, 2)));
    assert_eq!(find(r"(?<=^a)", "a"), Some((1, 1)));
    assert_eq!(find(r"(?<=^a)", "ba"), None);
}

#[test]
fn lookbehind_variable_sized_alt() {
    assert_eq!(find(r"(?<=a|bc)", "xxa"), Some((3, 3)));
    assert_eq!(find(r"(?<=a|bc)", "xxbc"), Some((4, 4)));
    assert_eq!(find(r"(?<=a|bc)", "xx"), None);
    assert_eq!(find(r"(?<=a|bc)", "xxb"), None);
    assert_eq!(find(r"(?<=a|bc)", "xxc"), None);

    assert!(Regex::new(r"(?<=a(?:b|cd))").is_err());
    assert!(Regex::new(r"(?<=a+b+))").is_err());
}

#[test]
fn negative_lookbehind_variable_sized_alt() {
    assert_eq!(find(r"(?<!a|bc)x", "axx"), Some((2, 3)));
    assert_eq!(find(r"(?<!a|bc)x", "bcxx"), Some((3, 4)));
    assert_eq!(find(r"(?<!a|bc)x", "x"), Some((0, 1)));
    assert_eq!(find(r"(?<!a|bc)x", "bx"), Some((1, 2)));
    assert_eq!(find(r"(?<!a|bc)x", "cx"), Some((1, 2)));
    assert_eq!(find(r"(?<!a|bc)x", "ax"), None);
    assert_eq!(find(r"(?<!a|bc)x", "bcx"), None);

    assert!(Regex::new(r"(?<!a(?:b|cd))").is_err());
    assert!(Regex::new(r"(?<!a+b+)").is_err());
}

#[test]
fn lookahead_looks_left() {
    assert_eq!(find(r"a(?=\b|_)", "a."), Some((0, 1)));
    assert_eq!(find(r"a(?=_|\b)", "a."), Some((0, 1)));
}

#[test]
fn negative_lookahead_fail() {
    // This was a tricky one. There's a negative lookahead that contains a
    // "hard" alternative (because of the lookahead). When the VM gets to the
    // point where the body of the negative lookahead matched, it needs to fail
    // the negative lookahead match. That means it needs to pop the stack until
    // before the negative lookahead, and then fail. But how many times it has
    // to pop is not fixed, it depends on the body/VM state.
    assert_eq!(find(r"(?!a(?=b)|x)", "ab"), Some((1, 1)));
    assert_eq!(find(r"(?!`(?:[^`]+(?=`)|x)`)", "`a`"), Some((1, 1)));
}

fn find(re: &str, text: &str) -> Option<(usize, usize)> {
    let regex = common::regex(re);
    let result = regex.find(text);
    assert!(
        result.is_ok(),
        "Expected find to succeed, but was {:?}",
        result
    );
    result.unwrap()
}
