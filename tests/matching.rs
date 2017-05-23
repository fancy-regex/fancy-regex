extern crate fancy_regex;

use fancy_regex::Regex;


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

    // Character class escape
    assert_match(r"[\d]", "1");

    // Control characters
    assert_match(r"[\e]", "\x1B");
    assert_match(r"[\n]", "\x0A");
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


fn assert_match(re: &str, text: &str) {
    let result = match_text(re, text);
    assert_eq!(result, true, "Expected regex '{}' to match text '{}'", re, text);
}

fn assert_no_match(re: &str, text: &str) {
    let result = match_text(re, text);
    assert_eq!(result, false, "Expected regex '{}' to not match text '{}'", re, text);
}

fn match_text(re: &str, text: &str) -> bool {
    let parse_result = Regex::new(re);
    assert!(parse_result.is_ok(),
            "Expected regex '{}' to be compiled successfully, got {:?}", re, parse_result.err());

    let regex = parse_result.unwrap();
    let match_result = regex.is_match(text);
    assert!(match_result.is_ok(), "Expected match to succeed, but was {:?}", match_result);
    match_result.unwrap()
}
