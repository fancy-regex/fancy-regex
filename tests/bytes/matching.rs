use fancy_regex::{BytesMode, RegexBuilder};

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

#[test]
fn bytes_find_fixed_array_input() {
    let re = RegexBuilder::new(r"\d+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let input: &[u8; 7] = b"abc 456";
    let m = re.find(input).unwrap().unwrap();
    assert_eq!(m.as_bytes(), b"456");
    assert_eq!(m.start(), 4);
    assert_eq!(m.end(), 7);
}

#[test]
fn bytes_is_match_fixed_array_input() {
    let re = RegexBuilder::new(r"\d+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let yes: &[u8; 7] = b"abc 123";
    let no: &[u8; 3] = b"abc";
    assert!(re.is_match(yes).unwrap());
    assert!(!re.is_match(no).unwrap());
}

/// Case-insensitive backref in bytes mode.
///
/// This covers the `text_bytes.is_ascii()` fix in the VM: previously the code
/// checked `s.is_ascii()` (the whole input), so a non-ASCII *prefix* before an
/// ASCII capture caused the ASCII fast-path to be skipped even when the
/// captured slice itself was pure ASCII.
#[test]
fn bytes_backrefs_casei() {
    // ASCII: case-insensitive backref match
    assert_match_bytes(r"(abc)(?i:\1)", b"abcABC");
    // ASCII: case-insensitive backref no-match
    assert_no_match_bytes(r"(abc)(?i:\1)", b"abcdef");

    // Non-ASCII prefix before ASCII capture – exercises the is_ascii() fix
    assert_match_bytes(r"(abc)(?i:\1)", "δabcABC".as_bytes());

    // Unicode: case-insensitive backref match (δ ↔ Δ)
    assert_match_bytes(r"(δ)(?i:\1)", "δΔ".as_bytes());
    // Unicode: case-insensitive backref no-match
    assert_no_match_bytes(r"(δ)(?i:\1)", "δσ".as_bytes());

    // Non-UTF-8 bytes: 0xFF is not a valid UTF-8 byte, so (.) captures it as
    // a single raw byte; no ASCII case-insensitive match possible for these bytes.
    assert_no_match_bytes(r"(.)(?i:\1)", b"\xff\xfe\xfd\xfc\xff\xfe\xfd\xfb");
}

#[cfg_attr(feature = "track_caller", track_caller)]
fn assert_match_bytes(re: &str, text: &[u8]) {
    let result = match_bytes(re, text);
    assert!(
        result,
        "Expected regex '{}' to match bytes {:?}",
        re, text
    );
}

#[cfg_attr(feature = "track_caller", track_caller)]
fn assert_no_match_bytes(re: &str, text: &[u8]) {
    let result = match_bytes(re, text);
    assert!(
        !result,
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
