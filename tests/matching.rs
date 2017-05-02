extern crate fancy_regex;

use fancy_regex::Regex;


#[test]
fn control_character_escapes() {
    assert_matches(r"\a", "\x07");
    assert_matches(r"\e", "\x1B");
    assert_matches(r"\f", "\x0C");
    assert_matches(r"\n", "\x0A");
    assert_matches(r"\r", "\x0D");
    assert_matches(r"\t", "\x09");
    assert_matches(r"\v", "\x0B");
}

#[test]
fn character_class_escapes() {
    assert_matches(r"[\[]", "[");
    assert_matches(r"[\^]", "^");

    // The regex crate would reject the following because it's not necessary to escape them.
    // Other engines allow to escape any non-alphanumeric character.
    assert_matches(r"[\<]", "<");
    assert_matches(r"[\>]", ">");
    assert_matches(r"[\.]", ".");

    // Character class escape
    assert_matches(r"[\d]", "1");

    // Control characters
    assert_matches(r"[\e]", "\x1B");
    assert_matches(r"[\n]", "\x0A");
}


fn assert_matches(re: &str, text: &str) {
    let parse_result = Regex::new(re);
    assert!(parse_result.is_ok(),
            "Expected regex '{}' to be compiled successfully, got {:?}", re, parse_result.err());

    let regex = parse_result.unwrap();
    let match_result = regex.is_match(text);
    assert!(match_result.is_ok(), "Expected match to succeed, but was {:?}", match_result);
    assert_eq!(match_result.ok(), Some(true), "Expected regex '{}' to match text '{}'", re, text);
}
