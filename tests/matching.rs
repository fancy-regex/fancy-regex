use fancy_regex::{Captures, Error, RegexBuilder};

mod common;

#[test]
fn control_character_escapes() {
    assert_match(r"\a", "\x07");
    assert_match(r"\e", "\x1B");
    assert_match(r"\f", "\x0C");
    assert_match(r"\n", "\x0A");
    assert_match(r"\r", "\x0D");
    assert_match(r"\t", "\x09");
    assert_match(r"\v", "\x0B");
}

#[test]
fn character_class_escapes() {
    assert_match(r"[\[]", "[");
    assert_match(r"[\^]", "^");

    // The regex crate would reject the following because it's not necessary to escape them.
    // Other engines allow to escape any non-alphanumeric character.
    assert_match(r"[\<]", "<");
    assert_match(r"[\>]", ">");
    assert_match(r"[\.]", ".");
    assert_match(r"[\ ]", " ");

    // Character class escape
    assert_match(r"[\d]", "1");

    // Control characters
    assert_match(r"[\e]", "\x1B");
    assert_match(r"[\n]", "\x0A");

    // `]` can be unescaped if it's right after `[`
    assert_match(r"[]]", "]");
    // `]` can be unescaped even after `[^`
    assert_match(r"[^]]", "a");
}

#[test]
fn character_class_nested() {
    assert_match(r"[[a][bc]]", "c");
    assert_match(r"[a[^b]]", "c");
}

#[test]
fn character_class_intersection() {
    assert_match(r"[\w&&a-c]", "c");
    assert_no_match(r"[\w&&a-c]", "d");

    assert_match(r"[[0-9]&&[^4]]", "1");
    assert_no_match(r"[[0-9]&&[^4]]", "4");
}

#[test]
fn alternation_with_empty_arm() {
    assert_match(r"^(a|)$", "a");
    assert_match(r"^(a|)$", "");
    assert_match(r"^(|a)$", "a");
    assert_match(r"^(|a)$", "");
    assert_match(r"a|", "a");
    assert_match(r"a|", "");
    assert_match(r"|a", "a");
    assert_match(r"|a", "");
    assert_no_match(r"^(a|)$", "b");
}

#[test]
fn case_insensitive_character_class() {
    assert_match(r"^(?i)[a-z]+$", "aB");
}

#[test]
fn case_insensitive_escape() {
    // `\x61` is lowercase `a`
    assert_match(r"(?i)\x61", "A");

    // `\p{Ll}` is the "Letter, lowercase" category
    assert_match(r"(?i)\p{Ll}", "A");
}

#[test]
fn atomic_group() {
    assert_match(r"^a(?>bc|b)c$", "abcc");
    assert_no_match(r"^a(?>bc|b)c$", "abc");

    // Look-ahead forces use of VM
    assert_match(r"^a(bc(?=d)|b)cd$", "abcd");
    assert_no_match(r"^a(?>bc(?=d)|b)cd$", "abcd");
}

#[test]
fn backtrack_limit() {
    let re = RegexBuilder::new("(?i)(a|b|ab)*(?=c)")
        .backtrack_limit(100_000)
        .build()
        .unwrap();
    let s = "abababababababababababababababababababababababababababab";
    let result = re.is_match(s);
    assert!(result.is_err());
    match result.err() {
        Some(Error::BacktrackLimitExceeded) => {}
        _ => panic!("Expected Error::BacktrackLimitExceeded"),
    }
}

#[test]
fn end_of_hard_expression_cannot_be_delegated() {
    assert_match(r"(?!x)(?:a|ab)c", "abc");
    // If `(?:a|ab)` is delegated, there's no backtracking and `a` matches and `ab` is never tried.
    assert_match(r"((?!x)(?:a|ab))c", "abc");
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

#[track_caller]
fn assert_match(re: &str, text: &str) {
    let result = match_text(re, text);
    assert_eq!(
        result, true,
        "Expected regex '{}' to match text '{}'",
        re, text
    );
}

#[track_caller]
fn assert_no_match(re: &str, text: &str) {
    let result = match_text(re, text);
    assert_eq!(
        result, false,
        "Expected regex '{}' to not match text '{}'",
        re, text
    );
}

#[track_caller]
fn match_text(re: &str, text: &str) -> bool {
    let regex = common::regex(re);
    let result = regex.is_match(text);
    assert!(
        result.is_ok(),
        "Expected match to succeed, but was {:?}",
        result
    );
    result.unwrap()
}
