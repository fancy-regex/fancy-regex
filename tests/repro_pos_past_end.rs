use fancy_regex::RegexBuilder;

/// Calling `captures_from_pos` with a position past the end of the haystack
/// must not panic. Oniguruma returns "no match" for this; syntect relies on
/// it (it slices `&text[..end]` and sometimes passes `begin > end`, which
/// after slicing means `pos > haystack.len()`).
///
/// Repro of the real-world failure: the "Wrap" path (pure regex-automata
/// delegation) panics at `regex-automata/src/util/search.rs:426` with
/// `invalid span pos..text.len()` because start > end in the span.
#[test]
fn captures_from_pos_past_end_wrap() {
    let re = RegexBuilder::new(r"^(<{7})(?:\s+(\S.*?))?$\n?")
        .oniguruma_mode(true)
        .build()
        .unwrap();
    // Haystack length 2, pos 12 — this is the exact shape syntect triggers
    // when parsing LaTeX merge-conflict markers.
    let result = re.captures_from_pos("ab", 12);
    assert!(
        matches!(result, Ok(None)),
        "expected Ok(None) for pos past end, got {result:?}"
    );
}

/// Same contract check for the Fancy (VM) path: trivially-reach-able via
/// a pattern that forces fancy-regex to keep it in the VM rather than
/// delegating (e.g. using a lookbehind).
#[test]
fn captures_from_pos_past_end_fancy() {
    let re = RegexBuilder::new(r"(?<=a)b")
        .oniguruma_mode(true)
        .build()
        .unwrap();
    let result = re.captures_from_pos("ab", 12);
    assert!(
        matches!(result, Ok(None)),
        "expected Ok(None) for pos past end, got {result:?}"
    );
}

/// Boundary: pos == text.len() is a valid anchor position (some patterns
/// like `$` or empty expressions match there), so this must not collapse
/// to the past-end branch.
#[test]
fn captures_from_pos_at_end_still_runs() {
    let re = RegexBuilder::new("$").oniguruma_mode(true).build().unwrap();
    let result = re.captures_from_pos("ab", 2).unwrap();
    assert!(result.is_some(), "pattern `$` must match at end-of-text");
}
