use fancy_regex::{Captures, Match, Result};

mod common;

#[test]
fn capture_names() {
    let regex = common::regex("(?<foo>)()(?P<bar>)");
    let capture_names = regex.capture_names().collect::<Vec<_>>();
    assert_eq!(capture_names, vec![None, Some("foo"), None, Some("bar")]);
}

#[test]
fn captures_fancy() {
    let captures = captures(r"\s*(\w+)(?=\.)", "foo bar.");
    assert_eq!(captures.len(), 2);
    assert_match(captures.get(0), " bar", 3, 7);
    assert_match(captures.get(1), "bar", 4, 7);
    assert!(captures.get(2).is_none());
}

#[test]
fn captures_fancy_named() {
    let captures = captures(r"\s*(?<name>\w+)(?=\.)", "foo bar.");
    assert_eq!(captures.len(), 2);
    assert_match(captures.get(0), " bar", 3, 7);
    assert_match(captures.name("name"), "bar", 4, 7);
    assert!(captures.get(2).is_none());
}

#[test]
fn captures_fancy_unmatched_group() {
    let captures = captures(r"(\w+)(?=\.)|(\w+)(?=!)", "foo! bar.");
    assert_eq!(captures.len(), 3);
    assert_match(captures.get(0), "foo", 0, 3);
    assert!(captures.get(1).is_none());
    assert_match(captures.get(2), "foo", 0, 3);
}

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
    let text = "11 21 33";

    let regex = common::regex(r"(\d)\d");
    let captures = assert_captures(regex.captures_from_pos(text, 3));
    assert_eq!(captures.len(), 2);
    assert_match(captures.get(0), "21", 3, 5);
    assert_match(captures.get(1), "2", 3, 4);
    let matches: Vec<_> = captures.iter().collect();
    assert_eq!(matches.len(), 2);
    assert_match(matches[0], "21", 3, 5);
    assert_match(matches[1], "2", 3, 4);

    let regex = common::regex(r"(\d+)\1");
    let captures = assert_captures(regex.captures_from_pos(text, 3));
    assert_eq!(captures.len(), 2);
    assert_match(captures.get(0), "33", 6, 8);
    assert_match(captures.get(1), "3", 6, 7);
    let matches: Vec<_> = captures.iter().collect();
    assert_eq!(matches.len(), 2);
    assert_match(matches[0], "33", 6, 8);
    assert_match(matches[1], "3", 6, 7);

    let regex = common::regex(r"(?P<foo>\d+)\k<foo>");
    let captures = assert_captures(regex.captures_from_pos(text, 3));
    assert_eq!(captures.len(), 2);
    assert_match(captures.get(0), "33", 6, 8);
    assert_match(captures.name("foo"), "3", 6, 7);
    let matches: Vec<_> = captures.iter().collect();
    assert_eq!(matches.len(), 2);
    assert_match(matches[0], "33", 6, 8);
    assert_match(matches[1], "3", 6, 7);

    let regex = common::regex(r"(?P<foo>\d+)(?P=foo)");
    let captures = assert_captures(regex.captures_from_pos(text, 3));
    assert_eq!(captures.len(), 2);
    assert_match(captures.get(0), "33", 6, 8);
    assert_match(captures.name("foo"), "3", 6, 7);
    let matches: Vec<_> = captures.iter().collect();
    assert_eq!(matches.len(), 2);
    assert_match(matches[0], "33", 6, 8);
    assert_match(matches[1], "3", 6, 7);
}

#[test]
fn captures_from_pos_looking_left() {
    let regex = common::regex(r"\b(\w)");

    // This should *not* match because `\b` doesn't match between a and x
    let result = regex.captures_from_pos("ax", 1).unwrap();
    assert!(result.is_none());

    let captures = assert_captures(regex.captures_from_pos(".x", 1));
    assert_eq!(captures.len(), 2);
    assert_match(captures.get(0), "x", 1, 2);
    assert_match(captures.get(1), "x", 1, 2);
}

#[track_caller]
fn captures<'a>(re: &str, text: &'a str) -> Captures<'a> {
    let regex = common::regex(re);
    let result = regex.captures(text);
    assert_captures(result)
}

#[track_caller]
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

#[track_caller]
fn assert_match(m: Option<Match<'_>>, expected_text: &str, start: usize, end: usize) {
    assert!(m.is_some(), "Expected match, but was {:?}", m);
    let m = m.unwrap();
    assert_eq!(m.as_str(), expected_text);
    assert_eq!(m.start(), start);
    assert_eq!(m.end(), end);
}

#[test]
fn expand() {
    let regex = common::regex("(a)(b)(?<π>c)(?P<x>d)");
    let cap = regex.captures("abcd").unwrap().expect("matched");
    assert_expansion(&cap, "$0", "abcd");
    assert_expansion(&cap, "$1", "a");
    assert_expansion(&cap, "$2", "b");
    assert_expansion(&cap, "$3", "c");
    assert_expansion(&cap, "$4", "d");
    assert_expansion(&cap, "$π", "c");
    assert_expansion(&cap, "$x", "d");
    assert_expansion(&cap, "$0π", "");
    assert_expansion(&cap, "$1π", "");
    assert_expansion(&cap, "$2π", "");
    assert_expansion(&cap, "$3π", "");
    assert_expansion(&cap, "$4π", "");
    assert_expansion(&cap, "$ππ", "");
    assert_expansion(&cap, "$xπ", "");
    assert_expansion(&cap, "${0}π", "abcdπ");
    assert_expansion(&cap, "${1}π", "aπ");
    assert_expansion(&cap, "${2}π", "bπ");
    assert_expansion(&cap, "${3}π", "cπ");
    assert_expansion(&cap, "${4}π", "dπ");
    assert_expansion(&cap, "${π}π", "cπ");
    assert_expansion(&cap, "${x}π", "dπ");
    assert_expansion(&cap, "$", "$");
    assert_expansion(&cap, "$π√", "c√");
    assert_expansion(&cap, "$x√", "d√");
    assert_expansion(&cap, "$$π", "$π");
    assert_expansion(&cap, "${π", "${π");
    assert_backlash_expansion(&cap, "\\0", "abcd");
    assert_backlash_expansion(&cap, "\\1", "a");
    assert_backlash_expansion(&cap, "\\2", "b");
    assert_backlash_expansion(&cap, "\\3", "c");
    assert_backlash_expansion(&cap, "\\4", "d");
    assert_backlash_expansion(&cap, "\\π", "\\π");
    assert_backlash_expansion(&cap, "\\x", "\\x");
    assert_backlash_expansion(&cap, "\\0π", "abcdπ");
    assert_backlash_expansion(&cap, "\\1π", "aπ");
    assert_backlash_expansion(&cap, "\\2π", "bπ");
    assert_backlash_expansion(&cap, "\\3π", "cπ");
    assert_backlash_expansion(&cap, "\\4π", "dπ");
    assert_backlash_expansion(&cap, "\\ππ", "\\ππ");
    assert_backlash_expansion(&cap, "\\xπ", "\\xπ");
    assert_backlash_expansion(&cap, "\\g<0>π", "abcdπ");
    assert_backlash_expansion(&cap, "\\g<1>π", "aπ");
    assert_backlash_expansion(&cap, "\\g<2>π", "bπ");
    assert_backlash_expansion(&cap, "\\g<3>π", "cπ");
    assert_backlash_expansion(&cap, "\\g<4>π", "dπ");
    assert_backlash_expansion(&cap, "\\g<π>π", "cπ");
    assert_backlash_expansion(&cap, "\\g<x>π", "dπ");
    assert_backlash_expansion(&cap, "\\", "\\");
    assert_backlash_expansion(&cap, "\\\\π", "\\π");
    assert_backlash_expansion(&cap, "\\g<π", "\\g<π");
}

#[track_caller]
fn assert_expansion(cap: &Captures, replacement: &str, text: &str) {
    let mut buf = String::new();
    cap.expand(replacement, &mut buf);
    assert_eq!(buf, text);
}

#[track_caller]
fn assert_backlash_expansion(cap: &Captures, replacement: &str, text: &str) {
    let mut buf = String::new();
    cap.expand_backslash(replacement, &mut buf);
    assert_eq!(buf, text);
}
