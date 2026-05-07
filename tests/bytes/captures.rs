use fancy_regex::{BytesMode, MatchBytes, RegexBuilder};

#[test]
fn bytes_captures_non_utf8() {
    let re = RegexBuilder::new(r"(\w+)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let caps = re.captures(b"\x80\x81abc").unwrap().unwrap();
    assert_match_bytes(caps.get(0), b"abc", 2, 5);
    assert_match_bytes(caps.get(1), b"abc", 2, 5);
}

#[test]
fn bytes_captures_ascii_dot_captures_raw_bytes() {
    let re = RegexBuilder::new(r"(.+)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let caps = re.captures(b"\x80\x81\x82").unwrap().unwrap();
    assert_match_bytes(caps.get(0), b"\x80\x81\x82", 0, 3);
    assert_match_bytes(caps.get(1), b"\x80\x81\x82", 0, 3);
}

#[test]
fn bytes_captures_iter_attributes() {
    let re = RegexBuilder::new(r"(\d+)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let text = b"a1 b23";
    let iter = re.captures_iter(text);
    assert_eq!(iter.text(), text);
    assert_eq!(re.as_str(), iter.regex().as_str());
}

#[test]
fn bytes_captures_iter_non_utf8() {
    let re = RegexBuilder::new(r"([^\x00]+)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let all_caps: Vec<_> = re
        .captures_iter(b"\x80\x81\x82\x00\x83\x84")
        .map(|c| c.unwrap())
        .collect();
    assert_eq!(all_caps.len(), 2);
    assert_match_bytes(all_caps[0].get(1), b"\x80\x81\x82", 0, 3);
    assert_match_bytes(all_caps[1].get(1), b"\x83\x84", 4, 6);
}

#[test]
fn bytes_captures_input_as_bytes() {
    let re = RegexBuilder::new(r"(\d+)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let caps = re.captures(b"abc 123").unwrap().unwrap();
    // input_as_bytes() returns the entire input, not just the matched portion
    assert_eq!(caps.input_as_bytes(), b"abc 123");
    assert_eq!(caps.get(0).unwrap().as_bytes(), b"123");
}

#[test]
fn bytes_captures_with_str_input() {
    let re = RegexBuilder::new(r"(\d+)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let caps = re.captures("abc 123").unwrap().unwrap();
    assert_eq!(caps.len(), 2);
    assert_eq!(caps.get(0).unwrap().as_str(), "123");
    assert_eq!(caps.get(1).unwrap().as_str(), "123");
}

#[test]
fn bytes_captures_no_match() {
    let re = RegexBuilder::new(r"(\d+)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let result = re.captures(b"abc").unwrap();
    assert!(result.is_none());
}

#[test]
fn bytes_captures_len() {
    let re = RegexBuilder::new(r"(\d+)-(\d+)-(\d+)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let caps = re.captures(b"1-2-3").unwrap().unwrap();
    assert_eq!(caps.len(), 4);
}

#[test]
fn bytes_captures_from_pos_no_match() {
    let re = RegexBuilder::new(r"(\d+)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let result = re.captures_from_pos(b"abc", 0).unwrap();
    assert!(result.is_none());
}

#[test]
fn bytes_captures_fixed_array_input() {
    let re = RegexBuilder::new(r"(\d+)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let input: &[u8; 7] = b"abc 123";
    let caps = re.captures(input).unwrap().unwrap();
    assert_match_bytes(caps.get(0), b"123", 4, 7);
    assert_match_bytes(caps.get(1), b"123", 4, 7);
}

// --- UnicodeBytes mode captures ---

#[test]
fn bytes_unicode_bytes_captures() {
    let re = RegexBuilder::new(r"(\w+)")
        .bytes_mode(BytesMode::UnicodeBytes)
        .build()
        .unwrap();
    let caps = re.captures("café".as_bytes()).unwrap().unwrap();
    assert_eq!(caps.get(0).unwrap().as_bytes(), "café".as_bytes());
    assert_eq!(caps.get(1).unwrap().as_bytes(), "café".as_bytes());
}

#[test]
fn bytes_unicode_bytes_find() {
    let re = RegexBuilder::new(r"\w+")
        .bytes_mode(BytesMode::UnicodeBytes)
        .build()
        .unwrap();
    let m = re.find("café!".as_bytes()).unwrap().unwrap();
    assert_eq!(m.as_bytes(), "café".as_bytes());
    assert_eq!(m.start(), 0);
    assert_eq!(m.end(), 5);
}

#[test]
fn bytes_unicode_bytes_find_iter() {
    let re = RegexBuilder::new(r"\w+")
        .bytes_mode(BytesMode::UnicodeBytes)
        .build()
        .unwrap();
    let matches: Vec<_> = re
        .find_iter("hello world".as_bytes())
        .map(|m| m.unwrap())
        .collect();
    assert_eq!(matches.len(), 2);
    assert_eq!(matches[0].as_bytes(), b"hello");
    assert_eq!(matches[1].as_bytes(), b"world");
}

#[test]
fn bytes_unicode_bytes_captures_iter() {
    let re = RegexBuilder::new(r"(\w+)")
        .bytes_mode(BytesMode::UnicodeBytes)
        .build()
        .unwrap();
    let all_caps: Vec<_> = re
        .captures_iter("foo bar".as_bytes())
        .map(|c| c.unwrap())
        .collect();
    assert_eq!(all_caps.len(), 2);
    assert_eq!(all_caps[0].get(1).unwrap().as_bytes(), b"foo");
    assert_eq!(all_caps[1].get(1).unwrap().as_bytes(), b"bar");
}

#[test]
fn bytes_unicode_bytes_captures_from_pos() {
    let re = RegexBuilder::new(r"(\w+)")
        .bytes_mode(BytesMode::UnicodeBytes)
        .build()
        .unwrap();
    let caps = re
        .captures_from_pos("hello world".as_bytes(), 6)
        .unwrap()
        .unwrap();
    assert_eq!(caps.get(0).unwrap().as_bytes(), b"world");
    assert_eq!(caps.get(1).unwrap().as_bytes(), b"world");
}

#[cfg_attr(feature = "track_caller", track_caller)]
fn assert_match_bytes(m: Option<MatchBytes<'_>>, expected: &[u8], start: usize, end: usize) {
    assert!(m.is_some(), "Expected match, but was None");
    let m = m.unwrap();
    assert_eq!(
        m.as_bytes(),
        expected,
        "Expected bytes {:?} at {}..{}, got {:?} at {}..{}",
        expected,
        start,
        end,
        m.as_bytes(),
        m.start(),
        m.end()
    );
    assert_eq!(m.start(), start);
    assert_eq!(m.end(), end);
}
