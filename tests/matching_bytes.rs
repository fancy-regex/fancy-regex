use fancy_regex::{BytesMode, Error, RegexBuilder, RuntimeError};

mod common;

#[test]
fn bytes_control_character_escapes() {
    assert_match_bytes(r"\a", b"\x07");
    assert_match_bytes(r"\e", b"\x1B");
    assert_match_bytes(r"\f", b"\x0C");
    assert_match_bytes(r"\n", b"\x0A");
    assert_match_bytes(r"\r", b"\x0D");
    assert_match_bytes(r"\t", b"\x09");
    assert_match_bytes(r"\v", b"\x0B");
}

#[test]
fn bytes_character_class_escapes() {
    assert_match_bytes(r"[\[]", b"[");
    assert_match_bytes(r"[\^]", b"^");
    assert_match_bytes(r"[\<]", b"<");
    assert_match_bytes(r"[\>]", b">");
    assert_match_bytes(r"[\ ]", b" ");
    assert_match_bytes(r"[\d]", b"1");
    assert_match_bytes(r"[\e]", b"\x1B");
    assert_match_bytes(r"[\n]", b"\x0A");
    assert_match_bytes(r"[]]", b"]");
    assert_match_bytes(r"[^]]", b"a");
}

#[test]
fn bytes_alternation_with_empty_arm() {
    assert_match_bytes(r"^(a|)$", b"a");
    assert_match_bytes(r"^(a|)$", b"");
    assert_match_bytes(r"^(|a)$", b"a");
    assert_match_bytes(r"^(|a)$", b"");
    assert_match_bytes(r"a|", b"a");
    assert_match_bytes(r"a|", b"");
    assert_match_bytes(r"|a", b"a");
    assert_match_bytes(r"|a", b"");
    assert_no_match_bytes(r"^(a|)$", b"b");
}

#[test]
fn bytes_case_insensitive() {
    assert_match_bytes(r"^(?i)[a-z]+$", b"aB");
}

#[test]
fn bytes_atomic_group() {
    assert_match_bytes(r"^a(?>bc|b)c$", b"abcc");
    assert_no_match_bytes(r"^a(?>bc|b)c$", b"abc");
    assert_match_bytes(r"^a(bc(?=d)|b)cd$", b"abcd");
    assert_no_match_bytes(r"^a(?>bc(?=d)|b)cd$", b"abcd");
}

#[test]
fn bytes_backtrack_limit() {
    let re = RegexBuilder::new(r"(?i)(a|b|ab)*(?>c)")
        .bytes_mode(BytesMode::Ascii)
        .backtrack_limit(100_000)
        .build()
        .expect("regex to compile successfully");
    let s = b"abababababababababababababababababababababababababababab";
    let result = re.is_match(s);
    assert!(result.is_err());
    match result.err() {
        Some(Error::RuntimeError(RuntimeError::BacktrackLimitExceeded)) => {}
        _ => panic!("Expected RuntimeError::BacktrackLimitExceeded"),
    }
}

#[test]
fn bytes_end_of_hard_expression_cannot_be_delegated() {
    assert_match_bytes(r"(?!x)(?:a|ab)c", b"abc");
    assert_match_bytes(r"((?!x)(?:a|ab))c", b"abc");
}

#[test]
fn bytes_backrefs() {
    assert_match_bytes(r"(abc)\1", b"abcabc");
    assert_match_bytes(r"(abc|def)\1", b"abcabc");
    assert_no_match_bytes(r"(abc|def)\1", b"abcdef");
    assert_match_bytes(r"(abc|def)\1", b"defdef");
}

#[test]
fn bytes_lookaheads() {
    assert_match_bytes(r"(?=c)", b"abcabc");
    assert_match_bytes(r"abc(?=abc)", b"abcabc");
    assert_no_match_bytes(r"abc(?=abc)", b"abcdef");
}

#[test]
fn bytes_hard_trailing_positive_lookaheads() {
    assert_match_bytes(r"(abc|def)(?=\1)", b"defdef");
    assert_match_bytes(r"(abc|def)(?=a(?!b))", b"abca");
    assert_no_match_bytes(r"(abc|def)(?=a(?!b))", b"abcabc");
}

#[test]
fn bytes_find_from_pos() {
    let re = RegexBuilder::new(r"\d+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let mat = re.find_from_pos(b"abc 123", 4).unwrap().unwrap();
    assert_eq!(mat.start(), 4);
    assert_eq!(mat.end(), 7);
    assert_eq!(mat.as_bytes(), b"123");
}

#[test]
fn bytes_find_from_pos_no_match() {
    let re = RegexBuilder::new(r"\d+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let result = re.find_from_pos(b"abc 123", 7).unwrap();
    assert!(result.is_none());
}

#[test]
fn bytes_non_utf8_input() {
    let re = RegexBuilder::new(r"\d+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let input = b"\x80\x81\x82 123";
    assert!(re.is_match(input).unwrap());

    let mat = re.find_from_pos(input, 0).unwrap().unwrap();
    assert_eq!(mat.as_bytes(), b"123");
}

#[test]
fn bytes_ascii_dot_matches_non_utf8() {
    let re = RegexBuilder::new(r".+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    assert!(re.is_match(b"\x80\x81\x82").unwrap());

    let re = RegexBuilder::new(r".")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    assert!(re.is_match(b"\xff").unwrap());
    assert!(re.is_match(b"\x80").unwrap());

    let re = RegexBuilder::new(r".*")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let mat = re.find_from_pos(b"\xff\xfe\xfd", 0).unwrap().unwrap();
    assert_eq!(mat.as_bytes(), b"\xff\xfe\xfd");
}

#[test]
fn bytes_unicode_bytes_dot_does_not_match_raw_bytes() {
    let re = RegexBuilder::new(r".")
        .bytes_mode(BytesMode::UnicodeBytes)
        .build()
        .unwrap();
    assert!(!re.is_match(b"\x80").unwrap());
    assert!(re.is_match(b"A").unwrap());
}

#[test]
fn bytes_unicode_bytes_char_classes_still_unicode() {
    let re = RegexBuilder::new(r"\w+")
        .bytes_mode(BytesMode::UnicodeBytes)
        .build()
        .unwrap();
    assert!(re.is_match("café".as_bytes()).unwrap());
}

#[test]
fn bytes_ascii_char_classes_are_ascii_only() {
    let re = RegexBuilder::new(r"^\w+$")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    assert!(re.is_match(b"hello").unwrap());
    assert!(!re.is_match("café".as_bytes()).unwrap());
}

#[test]
fn bytes_subroutines() {
    assert_match_bytes(r"^(a)\g<1>$", b"aa");
    assert_no_match_bytes(r"^(a)\g<1>$", b"ab");
    assert_match_bytes(r"(?<name>ab)\g<name>", b"abab");
    assert_no_match_bytes(r"(?<name>ab)\g<name>", b"abcd");
}

#[test]
fn bytes_recursive_subroutine() {
    let balanced_parens = r"^(?<foo>a|\(\g<foo>\))$";
    assert_match_bytes(balanced_parens, b"a");
    assert_match_bytes(balanced_parens, b"(a)");
    assert_match_bytes(balanced_parens, b"(((((a)))))");
    assert_no_match_bytes(balanced_parens, b"(a");
    assert_no_match_bytes(balanced_parens, b"((a)");
}

#[test]
fn bytes_conditional() {
    assert_match_bytes(r"(a)(?(1))", b"a");
    assert_match_bytes(r"(a)(b)?(?(2))", b"ab");
    assert_no_match_bytes(r"(a)(b)?(?(2))", b"a");
}

#[test]
fn bytes_general_newline_escape() {
    assert_match_bytes(r"\R", b"\r\n");
    assert_match_bytes(r"\R", b"\n");
    assert_match_bytes(r"\R", b"\r");
    assert_match_bytes(r"\R", b"\x0B");
    assert_match_bytes(r"\R", b"\x0C");
    assert_match_bytes(r"a\Rb", b"a\r\nb");
    assert_match_bytes(r"a\Rb", b"a\nb");
    assert_no_match_bytes(r"\R\n", b"\r\n");
    assert_no_match_bytes(r"^\R$", b"a");
}

#[test]
fn bytes_find_returns_matchbytes() {
    let re = RegexBuilder::new(r"\d+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let mat = re.find(b"abc 123").unwrap().unwrap();
    assert_eq!(mat.start(), 4);
    assert_eq!(mat.end(), 7);
    assert_eq!(mat.as_bytes(), b"123");
}

#[test]
fn bytes_find_no_match() {
    let re = RegexBuilder::new(r"\d+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let result = re.find(b"abc").unwrap();
    assert!(result.is_none());
}

#[test]
fn bytes_find_iter() {
    let re = RegexBuilder::new(r"\d+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let mut matches = re.find_iter(b"a1 b23 c456");
    let m1 = matches.next().unwrap().unwrap();
    assert_eq!(m1.as_bytes(), b"1");
    let m2 = matches.next().unwrap().unwrap();
    assert_eq!(m2.as_bytes(), b"23");
    let m3 = matches.next().unwrap().unwrap();
    assert_eq!(m3.as_bytes(), b"456");
    assert!(matches.next().is_none());
}

#[test]
fn bytes_find_iter_non_utf8() {
    let re = RegexBuilder::new(r".+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let mut matches = re.find_iter(b"\x80\x81\x82");
    let m = matches.next().unwrap().unwrap();
    assert_eq!(m.as_bytes(), b"\x80\x81\x82");
    assert!(matches.next().is_none());
}

#[test]
fn bytes_find_with_str_still_works() {
    let re = RegexBuilder::new(r"\d+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let mat = re.find("abc 123").unwrap().unwrap();
    assert_eq!(mat.as_str(), "123");
}

#[cfg_attr(feature = "track_caller", track_caller)]
fn assert_match_bytes(re: &str, text: &[u8]) {
    let result = match_bytes(re, text);
    assert_eq!(
        result, true,
        "Expected regex '{}' to match bytes {:?}",
        re, text
    );
}

#[cfg_attr(feature = "track_caller", track_caller)]
fn assert_no_match_bytes(re: &str, text: &[u8]) {
    let result = match_bytes(re, text);
    assert_eq!(
        result, false,
        "Expected regex '{}' to not match bytes {:?}",
        re, text
    );
}

fn match_bytes(re: &str, text: &[u8]) -> bool {
    let regex = RegexBuilder::new(re)
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let result = regex.is_match(text);
    assert!(
        result.is_ok(),
        "Expected match to succeed, but was {:?}",
        result
    );
    result.unwrap()
}
