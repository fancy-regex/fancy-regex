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

    // Unicode case folding (δ ↔ Δ) is NOT applied in BytesMode::Ascii because
    // unicode=false is implied, so the backref uses ASCII-only case folding.
    assert_no_match_bytes(r"(δ)(?i:\1)", "δΔ".as_bytes());
    // Unicode: case-insensitive backref no-match
    assert_no_match_bytes(r"(δ)(?i:\1)", "δσ".as_bytes());

    // Non-UTF-8 bytes: 0xFF is not a valid UTF-8 byte, so (.) captures it as
    // a single raw byte; no ASCII case-insensitive match possible for these bytes.
    assert_no_match_bytes(r"(.)(?i:\1)", b"\xff\xfe\xfd\xfc\xff\xfe\xfd\xfb");
}

// --- Hard-mode tests (force VM via atomic groups) ---
// These tests use `(?>...)` to force the backtracking VM path,
// verifying that byte-level advancement works correctly with non-UTF-8 high bytes.

/// Atomic group forces hard mode. The dot should advance by 1 byte in Ascii mode,
/// even for bytes >= 0x80 that would be multi-byte UTF-8 lead bytes.
#[test]
fn bytes_hard_mode_dot_advances_one_byte() {
    // (?>.) forces hard mode; the dot should match exactly 1 byte (0xC0),
    // and then the literal `x` should match at position 1.
    // 0xC0 is a 2-byte UTF-8 lead byte; in Unicode mode it would try to
    // advance 2 bytes, but in Ascii mode it should advance only 1.
    assert_match_bytes(r"(?>.)x", b"\xC0x");
    // Same with 0xE0 (3-byte UTF-8 lead byte)
    assert_match_bytes(r"(?>.)x", b"\xE0x");
    // Same with 0xF0 (4-byte UTF-8 lead byte)
    assert_match_bytes(r"(?>.)x", b"\xF0x");
}

/// Verify that the SplitUnanchored preamble (which uses `Any`) advances 1 byte
/// at a time in Ascii mode, allowing matches after high bytes.
#[test]
fn bytes_hard_mode_unanchored_skips_high_bytes() {
    // The pattern is unanchored. The VM needs to skip over 0xFF, 0xFE, 0xFD
    // (each 1 byte) to find the match at "ab".
    // The atomic group forces hard mode.
    assert_match_bytes(r"(?>ab)", b"\xFF\xFE\xFDab");
}

/// `AnyNoNL` in hard mode should advance 1 byte for high bytes.
#[test]
fn bytes_hard_mode_dot_no_newline() {
    // (?s) is NOT set, so `.` means AnyNoNL. Atomic group forces VM.
    // 0x80 is not a newline, so dot should match it (1 byte) and then match `y`.
    assert_match_bytes(r"(?>.)y", b"\x80y");
    // But newline should not be matched by dot (no (?s))
    assert_no_match_bytes(r"^(?>.)y", b"\ny");
}

/// GoBack instruction should go back 1 byte in Ascii mode, even for high bytes.
/// We use a lookbehind with a dot (which is hard due to the lookbehind) to test
/// that GoBack(1) retreats exactly 1 byte, even when the byte at that position
/// is a high byte (>= 0x80) that would normally be a multi-byte UTF-8 lead byte.
#[test]
fn bytes_hard_mode_lookbehind_goback() {
    // `(?<=.)x` is hard (lookbehind). GoBack(1) goes back 1 byte from `x`.
    // In Ascii mode, the dot in the lookbehind then matches the single byte 0xC0.
    // In Unicode mode this would fail because 0xC0 followed by 'x' is invalid UTF-8.
    assert_match_bytes(r"(?<=.)x", b"\xC0x");
    // Verify with a 2-char lookbehind: GoBack(2) should go back 2 bytes.
    assert_match_bytes(r"(?<=..)x", b"\xE0\xF0x");
}

/// Multiple dots in an atomic group should each advance 1 byte.
#[test]
fn bytes_hard_mode_multiple_dots() {
    // (?>...) should consume exactly 3 bytes, then `z` should match.
    assert_match_bytes(r"(?>.{3})z", b"\xC0\xE0\xF0z");
    // Verify the match doesn't happen if z isn't at position 3
    assert_no_match_bytes(r"^(?>.{3})z", b"\xC0\xE0z\xF0");
}

// --- UnicodeBytes hard-mode tests ---
// These verify that UnicodeBytes mode still uses codepoint-level advancement
// in the VM (not byte-level), while accepting non-UTF-8 input.

/// In UnicodeBytes mode, the dot should advance by codepoint length, not 1 byte.
/// A valid multi-byte UTF-8 sequence followed by a literal should work correctly.
#[test]
fn bytes_unicode_bytes_hard_mode_dot_advances_codepoint() {
    // é is 0xC3 0xA9 (2-byte UTF-8). In UnicodeBytes mode with atomic group (hard),
    // the dot should consume both bytes as one codepoint, then match 'x'.
    let re = RegexBuilder::new(r"(?>.)x")
        .bytes_mode(BytesMode::UnicodeBytes)
        .build()
        .unwrap();
    assert!(re.is_match("éx".as_bytes()).unwrap());
    // Should NOT match if x is at byte offset 1 (middle of the codepoint)
    assert!(!re.is_match(b"\xC3x").unwrap());
}

/// In UnicodeBytes mode, the unanchored preamble should skip by codepoint.
#[test]
fn bytes_unicode_bytes_hard_mode_unanchored_skips_codepoints() {
    // The VM skips over "é" (2 bytes) as one codepoint to find the match.
    let re = RegexBuilder::new(r"(?>ab)")
        .bytes_mode(BytesMode::UnicodeBytes)
        .build()
        .unwrap();
    assert!(re.is_match("éab".as_bytes()).unwrap());
}

/// In UnicodeBytes mode, GoBack should retreat by codepoint, not by byte.
#[test]
fn bytes_unicode_bytes_hard_mode_lookbehind_goback() {
    // `(?<=.)x` is hard. GoBack(1) should go back 1 codepoint.
    // "é" is 2 bytes, so GoBack(1) from 'x' at offset 2 should land at offset 0.
    let re = RegexBuilder::new(r"(?<=.)x")
        .bytes_mode(BytesMode::UnicodeBytes)
        .build()
        .unwrap();
    assert!(re.is_match("éx".as_bytes()).unwrap());
}

/// UnicodeBytes mode: dot does NOT match isolated non-UTF-8 bytes (unlike Ascii mode).
#[test]
fn bytes_unicode_bytes_hard_mode_dot_rejects_invalid_utf8() {
    // (?>.)x: the dot in UnicodeBytes mode only matches valid Unicode scalar values.
    // 0xC0 alone is not valid UTF-8, so dot should not match it.
    let re = RegexBuilder::new(r"^(?>.)x")
        .bytes_mode(BytesMode::UnicodeBytes)
        .build()
        .unwrap();
    assert!(!re.is_match(b"\xC0x").unwrap());
}

// --- Seek instruction with high bytes in Ascii mode ---

/// Seek pre-filter with backref pattern should advance byte-by-byte over high bytes.
/// The seek instruction's `advance_one` is used when computing `next_seek_start`.
#[test]
fn bytes_hard_mode_seek_with_high_bytes() {
    // Pattern: (abc)\1 with seek enabled. The seek pre-filter derives a pattern
    // for "abc" and uses it to skip ahead. We place high bytes before the match
    // to verify the seek advances correctly over them.
    let re = RegexBuilder::new(r"(abc)\1")
        .bytes_mode(BytesMode::Ascii)
        .seek(true)
        .build()
        .unwrap();
    let input = b"\xFF\xFE\xFDabcabc";
    let mat = re.find(input).unwrap().unwrap();
    assert_eq!(mat.start(), 3);
    assert_eq!(mat.end(), 9);
    assert_eq!(mat.as_bytes(), b"abcabc");
}

/// Seek with a zero-width match scenario: the seek pre-filter matches at a position
/// but the main pattern fails, requiring backtrack and re-seek past a high byte.
#[test]
fn bytes_hard_mode_seek_advances_past_high_byte_on_backtrack() {
    // Pattern: lookahead for "x" then backref. The seek pattern is "x".
    // Input has \xFF before the real match. If the seek finds "x" at position 1
    // (after \xFF) and the full pattern fails, the next seek start should be
    // position 2 (1 byte past), not a multi-byte advance.
    let re = RegexBuilder::new(r"(?=x)(x)\1")
        .bytes_mode(BytesMode::Ascii)
        .seek(true)
        .build()
        .unwrap();
    let input = b"\xFFxxx";
    let mat = re.find(input).unwrap().unwrap();
    assert_eq!(mat.as_bytes(), b"xx");
}

// --- AbsentRepeater with high bytes in Ascii mode ---

/// The absent repeater `(?~...)` advances one position at a time when scanning.
/// In Ascii mode, this should be 1 byte even for high bytes.
#[test]
fn bytes_hard_mode_absent_repeater_with_high_bytes() {
    // (`+)(?~\1)\1 pattern: capture backticks, then absent-repeat (no backticks),
    // then match backticks again. Place high bytes in the "body" to verify
    // the absent repeater scans byte-by-byte.
    let re = RegexBuilder::new(r"(`+)(?~\1)\1")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let input = b"`\xFF\xFE\xC0`";
    assert!(re.is_match(input).unwrap());
    let mat = re.find(input).unwrap().unwrap();
    assert_eq!(mat.as_bytes(), b"`\xFF\xFE\xC0`");
}

/// Absent repeater with lookahead inner (hard) scanning over high bytes.
#[test]
fn bytes_hard_mode_absent_repeater_lookahead_with_high_bytes() {
    // (?~(?=end))end: match everything until "end" appears.
    // High bytes before "end" should be scanned one byte at a time.
    let re = RegexBuilder::new(r"(?~(?=end))end")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let input = b"\xFF\xC0\xE0end";
    assert!(re.is_match(input).unwrap());
    let mat = re.find(input).unwrap().unwrap();
    assert_eq!(mat.as_bytes(), b"\xFF\xC0\xE0end");
}

// --- Explicit \R tests in Ascii bytes mode ---

/// \R should compile and work in Ascii bytes mode (non-Unicode newline set).
#[test]
fn bytes_hard_mode_general_newline_ascii() {
    // \R is compiled using atomic groups internally, making it hard.
    // In Ascii mode it should match \r\n, \n, \r, \x0B, \x0C but NOT
    // the Unicode newlines U+0085, U+2028, U+2029.
    assert_match_bytes(r"\R", b"\r\n");
    assert_match_bytes(r"\R", b"\n");
    assert_match_bytes(r"\R", b"\r");
    assert_match_bytes(r"\R", b"\x0B");
    assert_match_bytes(r"\R", b"\x0C");

    // Unicode newlines should NOT match in Ascii mode.
    // U+0085 = 0xC2 0x85, U+2028 = 0xE2 0x80 0xA8, U+2029 = 0xE2 0x80 0xA9
    assert_no_match_bytes(r"^\R$", "\u{0085}".as_bytes());
    assert_no_match_bytes(r"^\R$", "\u{2028}".as_bytes());
    assert_no_match_bytes(r"^\R$", "\u{2029}".as_bytes());
}

/// \R should not backtrack from \r\n to \r in Ascii bytes mode.
#[test]
fn bytes_hard_mode_general_newline_atomic() {
    // \R\n should NOT match "\r\n" because \R atomically consumes \r\n,
    // leaving nothing for the \n.
    assert_no_match_bytes(r"\R\n", b"\r\n");
    // But \R\R should match \r\n followed by \n
    assert_match_bytes(r"\R\R", b"\r\n\n");
}

/// \R with high bytes: the unanchored search should skip high bytes to find newlines.
#[test]
fn bytes_hard_mode_general_newline_after_high_bytes() {
    assert_match_bytes(r"\R", b"\xFF\xFE\n");
    assert_match_bytes(r"a\Rb", b"\xFFa\nb");
}

#[cfg_attr(feature = "track_caller", track_caller)]
fn assert_match_bytes(re: &str, text: &[u8]) {
    let result = match_bytes(re, text);
    assert!(result, "Expected regex '{}' to match bytes {:?}", re, text);
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
