#[allow(unused_imports)]
pub use fancy_regex::DebugRegex;
use fancy_regex::{BytesMode, Captures, Regex, RegexBuilder, RegexInput};
use std::ops::Range;

#[allow(dead_code)]
pub fn regex(re: &str) -> Regex {
    let parse_result = Regex::new(re);
    assert!(
        parse_result.is_ok(),
        "Expected regex '{}' to be compiled successfully, got {:?}",
        re,
        parse_result.err()
    );
    parse_result.unwrap()
}

/// Build a regex in ASCII bytes mode.  Useful for bytes-mode tests that need
/// to configure options beyond what the simple dual-mode helpers provide.
#[allow(dead_code)]
pub fn ascii_bytes_regex(re: &str) -> Regex {
    let parse_result = RegexBuilder::new(re).bytes_mode(BytesMode::Ascii).build();
    assert!(
        parse_result.is_ok(),
        "Expected bytes regex '{}' to be compiled successfully, got {:?}",
        re,
        parse_result.err()
    );
    parse_result.unwrap()
}

/// Assert that `re` matches `text` in **both** str mode and ASCII bytes mode.
#[cfg_attr(feature = "track_caller", track_caller)]
#[allow(dead_code)]
pub fn assert_is_match(re: &str, text: &str) {
    let str_result = regex(re).is_match(text);
    assert_eq!(
        str_result.unwrap(),
        true,
        "Expected regex '{}' to match '{}' (str mode)",
        re,
        text
    );
    let bytes_result = ascii_bytes_regex(re).is_match(text.as_bytes());
    assert_eq!(
        bytes_result.unwrap(),
        true,
        "Expected regex '{}' to match {:?} (bytes mode)",
        re,
        text.as_bytes()
    );
}

/// Assert that `re` does **not** match `text` in either str mode or ASCII bytes
/// mode.
#[cfg_attr(feature = "track_caller", track_caller)]
#[allow(dead_code)]
pub fn assert_no_match(re: &str, text: &str) {
    let str_result = regex(re).is_match(text);
    assert_eq!(
        str_result.unwrap(),
        false,
        "Expected regex '{}' to not match '{}' (str mode)",
        re,
        text
    );
    let bytes_result = ascii_bytes_regex(re).is_match(text.as_bytes());
    assert_eq!(
        bytes_result.unwrap(),
        false,
        "Expected regex '{}' to not match {:?} (bytes mode)",
        re,
        text.as_bytes()
    );
}

/// Run `find` against `text` in both str mode and ASCII bytes mode, assert
/// that both agree, and return the common result.
#[cfg_attr(feature = "track_caller", track_caller)]
#[allow(dead_code)]
pub fn assert_find(re: &str, text: &str) -> Option<(usize, usize)> {
    let str_result = regex(re).find(text).unwrap().map(|m| (m.start(), m.end()));
    let bytes_result = ascii_bytes_regex(re)
        .find(text.as_bytes())
        .unwrap()
        .map(|m| (m.start(), m.end()));
    assert_eq!(
        str_result, bytes_result,
        "Expected regex '{}' find results to agree between str and bytes mode for text '{}'",
        re, text
    );
    str_result
}

/// Run `find_input` against `text` in both str mode and ASCII bytes mode, assert
/// that both agree, and return the common result.
#[cfg_attr(feature = "track_caller", track_caller)]
#[allow(dead_code)]
pub fn assert_find_input(
    re: &str,
    text: &str,
    start: usize,
    range: Range<usize>,
) -> Option<(usize, usize)> {
    let str_result = regex(re)
        .find_input(RegexInput::new(text).from_pos(start).range(range.clone()))
        .unwrap()
        .map(|m| (m.start(), m.end()));
    let bytes_result = ascii_bytes_regex(re)
        .find_input(
            RegexInput::new(text.as_bytes())
                .from_pos(start)
                .range(range),
        )
        .unwrap()
        .map(|m| (m.start(), m.end()));
    assert_eq!(
        str_result, bytes_result,
        "Expected regex '{}' find_input results to agree between str and bytes mode for text '{}'",
        re, text
    );
    str_result
}

/// Run `captures_iter` against `text` in both str mode and ASCII bytes mode, assert
/// that both agree on every match's group spans, and return the str-mode results.
#[cfg_attr(feature = "track_caller", track_caller)]
#[allow(dead_code)]
pub fn assert_captures_iter<'t>(re: &str, text: &'t str) -> Vec<Captures<'t, str>> {
    let str_results: Vec<_> = regex(re)
        .captures_iter(text)
        .map(|c| c.expect("captures_iter succeeded (str mode)"))
        .collect();
    let bytes_results: Vec<_> = ascii_bytes_regex(re)
        .captures_iter(text.as_bytes())
        .map(|c| c.expect("captures_iter succeeded (bytes mode)"))
        .collect();
    assert_eq!(
        str_results.len(),
        bytes_results.len(),
        "Expected same number of captures_iter results for regex '{}' on '{}' between str and bytes modes",
        re,
        text
    );
    for (i, (s, b)) in str_results.iter().zip(bytes_results.iter()).enumerate() {
        assert_eq!(
            s.len(),
            b.len(),
            "Expected capture group count to agree for regex '{}' on '{}' at match {}",
            re,
            text,
            i
        );
        for j in 0..s.len() {
            let str_span = s.get(j).map(|m| (m.start(), m.end()));
            let bytes_span = b.get(j).map(|m| (m.start(), m.end()));
            assert_eq!(
                str_span,
                bytes_span,
                "Expected capture group {} to agree between str and bytes modes for regex '{}' on '{}' at match {}",
                j,
                re,
                text,
                i
            );
        }
    }
    str_results
}

/// Run `captures_from_pos` against `text` starting at `pos` in both str mode and ASCII bytes
/// mode, assert that both agree on every group span, and return the str-mode result.
#[cfg_attr(feature = "track_caller", track_caller)]
#[allow(dead_code)]
pub fn assert_captures_from_pos<'t>(
    re: &str,
    text: &'t str,
    pos: usize,
) -> Option<Captures<'t, str>> {
    let str_result = regex(re)
        .captures_from_pos(text, pos)
        .expect("expected captures_from_pos to succeed (str mode)");
    let bytes_result = ascii_bytes_regex(re)
        .captures_from_pos(text.as_bytes(), pos)
        .expect("expected captures_from_pos to succeed (bytes mode)");
    assert_eq!(
        str_result.is_some(),
        bytes_result.is_some(),
        "Expected regex '{}' captures_from_pos({}) to agree between str and bytes modes for '{}'",
        re,
        pos,
        text
    );
    if let (Some(ref s), Some(ref b)) = (&str_result, &bytes_result) {
        assert_eq!(
            s.len(),
            b.len(),
            "Expected capture group count to agree for regex '{}' on '{}' at pos {}",
            re,
            text,
            pos
        );
        for i in 0..s.len() {
            let str_span = s.get(i).map(|m| (m.start(), m.end()));
            let bytes_span = b.get(i).map(|m| (m.start(), m.end()));
            assert_eq!(
                str_span,
                bytes_span,
                "Expected capture group {} to agree between str and bytes modes for regex '{}' on '{}' at pos {}",
                i,
                re,
                text,
                pos
            );
        }
    }
    str_result
}

/// Run `captures_input` against `text` in both str mode and ASCII bytes mode,
/// assert that both agree on every group span, and return the str-mode result.
#[cfg_attr(feature = "track_caller", track_caller)]
#[allow(dead_code)]
pub fn assert_captures_input<'t>(
    re: &str,
    text: &'t str,
    start: usize,
    range: Range<usize>,
) -> Option<Captures<'t, str>> {
    let str_result = regex(re)
        .captures_input(RegexInput::new(text).from_pos(start).range(range.clone()))
        .expect("expected captures_input to succeed (str mode)");
    let bytes_result = ascii_bytes_regex(re)
        .captures_input(
            RegexInput::new(text.as_bytes())
                .from_pos(start)
                .range(range),
        )
        .expect("expected captures_input to succeed (bytes mode)");
    assert_eq!(
        str_result.is_some(),
        bytes_result.is_some(),
        "Expected regex '{}' captures_input to agree between str and bytes modes for '{}'",
        re,
        text
    );
    if let (Some(ref s), Some(ref b)) = (&str_result, &bytes_result) {
        assert_eq!(
            s.len(),
            b.len(),
            "Expected capture group count to agree for regex '{}' on '{}'",
            re,
            text
        );
        for i in 0..s.len() {
            let str_span = s.get(i).map(|m| (m.start(), m.end()));
            let bytes_span = b.get(i).map(|m| (m.start(), m.end()));
            assert_eq!(
                str_span,
                bytes_span,
                "Expected capture group {} to agree between str and bytes modes for regex '{}' on '{}'",
                i,
                re,
                text
            );
        }
    }
    str_result
}

/// Run `captures` against `text` in both str mode and ASCII bytes mode, assert
/// that both engines agree on the spans of every capture group, and return the
/// str-mode `Captures` (or `None` if neither engine matched).
#[cfg_attr(feature = "track_caller", track_caller)]
#[allow(dead_code)]
pub fn assert_captures<'t>(re: &str, text: &'t str) -> Option<Captures<'t, str>> {
    let str_result = regex(re)
        .captures(text)
        .expect("expected captures to succeed (str mode)");
    let bytes_result = ascii_bytes_regex(re)
        .captures(text.as_bytes())
        .expect("expected captures to succeed (bytes mode)");
    assert_eq!(
        str_result.is_some(),
        bytes_result.is_some(),
        "Expected regex '{}' captures to agree between str and bytes mode for '{}'",
        re,
        text
    );
    if let (Some(ref s), Some(ref b)) = (&str_result, &bytes_result) {
        assert_eq!(
            s.len(),
            b.len(),
            "Expected capture group count to agree for regex '{}' on '{}'",
            re,
            text
        );
        for i in 0..s.len() {
            let str_span = s.get(i).map(|m| (m.start(), m.end()));
            let bytes_span = b.get(i).map(|m| (m.start(), m.end()));
            assert_eq!(
                str_span,
                bytes_span,
                "Expected capture group {} to agree between str and bytes mode for regex '{}' on '{}'",
                i,
                re,
                text
            );
        }
    }
    str_result
}
