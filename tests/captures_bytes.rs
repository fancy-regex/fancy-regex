use fancy_regex::{BytesMode, MatchBytes, RegexBuilder};

mod common;

#[test]
fn bytes_captures_easy() {
    let re = RegexBuilder::new(r"(\d+)-(\d+)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let caps = re.captures(b"abc 123-456 def").unwrap().unwrap();
    assert_eq!(caps.len(), 3);
    assert_match_bytes(caps.get(0), b"123-456", 4, 11);
    assert_match_bytes(caps.get(1), b"123", 4, 7);
    assert_match_bytes(caps.get(2), b"456", 8, 11);
    assert!(caps.get(3).is_none());
}

#[test]
fn bytes_captures_fancy() {
    let re = RegexBuilder::new(r"\s*(\w+)(?=\.)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let caps = re.captures(b"foo bar.").unwrap().unwrap();
    assert_eq!(caps.len(), 2);
    assert_match_bytes(caps.get(0), b" bar", 3, 7);
    assert_match_bytes(caps.get(1), b"bar", 4, 7);
    assert!(caps.get(2).is_none());
}

#[test]
fn bytes_captures_named() {
    let re = RegexBuilder::new(r"(?<first>\d+)-(?<second>\d+)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let caps = re.captures(b"12-34").unwrap().unwrap();
    assert_match_bytes(caps.name("first"), b"12", 0, 2);
    assert_match_bytes(caps.name("second"), b"34", 3, 5);
    assert!(caps.name("nonexistent").is_none());
}

#[test]
fn bytes_captures_unmatched_group() {
    let re = RegexBuilder::new(r"(\w+)(?=\.)|(\w+)(?=!)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let caps = re.captures(b"foo! bar.").unwrap().unwrap();
    assert_eq!(caps.len(), 3);
    assert_match_bytes(caps.get(0), b"foo", 0, 3);
    assert!(caps.get(1).is_none());
    assert_match_bytes(caps.get(2), b"foo", 0, 3);
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
fn bytes_captures_iter_easy() {
    let re = RegexBuilder::new(r"(\d+)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let all_caps: Vec<_> = re
        .captures_iter(b"a1 b23 c456")
        .map(|c| c.unwrap())
        .collect();
    assert_eq!(all_caps.len(), 3);

    assert_match_bytes(all_caps[0].get(0), b"1", 1, 2);
    assert_match_bytes(all_caps[0].get(1), b"1", 1, 2);

    assert_match_bytes(all_caps[1].get(0), b"23", 4, 6);
    assert_match_bytes(all_caps[1].get(1), b"23", 4, 6);

    assert_match_bytes(all_caps[2].get(0), b"456", 8, 11);
    assert_match_bytes(all_caps[2].get(1), b"456", 8, 11);
}

#[test]
fn bytes_captures_iter_fancy() {
    let re = RegexBuilder::new(r"(?P<num>\d)\d")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let all_caps: Vec<_> = re.captures_iter(b"11 21 33").map(|c| c.unwrap()).collect();
    assert_eq!(all_caps.len(), 3);

    assert_match_bytes(all_caps[0].name("num"), b"1", 0, 1);
    assert_match_bytes(all_caps[1].name("num"), b"2", 3, 4);
    assert_match_bytes(all_caps[2].name("num"), b"3", 6, 7);
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
fn bytes_captures_from_pos() {
    let re = RegexBuilder::new(r"(\d)\d")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let caps = re.captures_from_pos(b"11 21 33", 3).unwrap().unwrap();
    assert_eq!(caps.len(), 2);
    assert_match_bytes(caps.get(0), b"21", 3, 5);
    assert_match_bytes(caps.get(1), b"2", 3, 4);
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
fn bytes_captures_from_pos_past_end() {
    let re = RegexBuilder::new(r"(\d+)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let result = re.captures_from_pos(b"abc", 10).unwrap();
    assert!(result.is_none());
}

#[test]
fn bytes_captures_from_pos_fancy() {
    let re = RegexBuilder::new(r"(\d+)\1")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let caps = re.captures_from_pos(b"11 21 33", 3).unwrap().unwrap();
    assert_eq!(caps.len(), 2);
    assert_match_bytes(caps.get(0), b"33", 6, 8);
    assert_match_bytes(caps.get(1), b"3", 6, 7);
}

#[test]
fn bytes_captures_iter_groups() {
    let re = RegexBuilder::new(r"(\d+)-(\d+)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let caps = re.captures(b"123-456").unwrap().unwrap();
    let groups: Vec<_> = caps.iter().collect();
    assert_eq!(groups.len(), 3);
    assert_match_bytes(groups[0], b"123-456", 0, 7);
    assert_match_bytes(groups[1], b"123", 0, 3);
    assert_match_bytes(groups[2], b"456", 4, 7);
}

#[test]
fn bytes_captures_iter_groups_with_none() {
    let re = RegexBuilder::new(r"(\w+)(?=\.)|(\w+)(?=!)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let caps = re.captures(b"foo! bar.").unwrap().unwrap();
    let groups: Vec<_> = caps.iter().collect();
    assert_eq!(groups.len(), 3);
    assert!(groups[0].is_some());
    assert!(groups[1].is_none());
    assert!(groups[2].is_some());
}

#[test]
fn bytes_captures_as_bytes() {
    let re = RegexBuilder::new(r"(\d+)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let caps = re.captures(b"abc 123").unwrap().unwrap();
    assert_eq!(caps.as_bytes(), b"abc 123");
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
fn bytes_captures_backrefs() {
    let re = RegexBuilder::new(r"(\w+)\s+\1")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let caps = re.captures(b"abc abc").unwrap().unwrap();
    assert_eq!(caps.len(), 2);
    assert_match_bytes(caps.get(0), b"abc abc", 0, 7);
    assert_match_bytes(caps.get(1), b"abc", 0, 3);
}

#[test]
fn bytes_captures_subroutine() {
    let re = RegexBuilder::new(r"^(?<pair>..)\g<pair>$")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let caps = re.captures(b"abcd").unwrap().unwrap();
    assert_eq!(caps.len(), 2);
    assert_match_bytes(caps.get(0), b"abcd", 0, 4);
    assert_match_bytes(caps.name("pair"), b"cd", 2, 4);
}

#[test]
fn bytes_captures_lookbehind() {
    let re = RegexBuilder::new(r"(?<=[a-z])(\d+)")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let caps = re.captures(b"abc123").unwrap().unwrap();
    assert_eq!(caps.len(), 2);
    assert_match_bytes(caps.get(0), b"123", 3, 6);
    assert_match_bytes(caps.get(1), b"123", 3, 6);
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
fn bytes_captures_iter_continue_from_previous_match_end() {
    let re = RegexBuilder::new(r"\G(\d)\d")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let all_caps: Vec<_> = re.captures_iter(b"1122 33").map(|c| c.unwrap()).collect();
    assert_eq!(all_caps.len(), 2);
    assert_match_bytes(all_caps[0].get(0), b"11", 0, 2);
    assert_match_bytes(all_caps[0].get(1), b"1", 0, 1);
    assert_match_bytes(all_caps[1].get(0), b"22", 2, 4);
    assert_match_bytes(all_caps[1].get(1), b"2", 2, 3);
}

#[test]
fn bytes_captures_iter_backtrack_limit() {
    let re = RegexBuilder::new(r"(x+x+)+(?>y)")
        .bytes_mode(BytesMode::Ascii)
        .backtrack_limit(1)
        .build()
        .unwrap();
    let result: Vec<_> = re.captures_iter(b"xxxxxxxxxxy").collect();
    assert_eq!(result.len(), 1);
    assert!(result[0].is_err());
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
