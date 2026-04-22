use fancy_regex::{Error, ParseError, RegexBuilder};

fn compile(pattern: &str, oniguruma: bool) -> Result<fancy_regex::Regex, Error> {
    RegexBuilder::new(pattern)
        .oniguruma_mode(oniguruma)
        .build()
}

#[test]
fn empty_noncapturing_group_with_star_default_mode() {
    let err = compile("(?:)*", false).unwrap_err();
    match err {
        Error::ParseError(_, ParseError::TargetNotRepeatable) => {}
        other => panic!("unexpected error variant: {other:?}"),
    }
}

#[test]
fn empty_noncapturing_group_with_star_oniguruma_mode() {
    let regex = compile("(?:)*", true).expect(
        "Oniguruma accepts `(?:)*` as a zero-width repetition; \
         fancy-regex in oniguruma_mode must match Oniguruma here",
    );
    assert!(regex.is_match("anything").unwrap());
    assert!(regex.is_match("").unwrap());
}

#[test]
fn empty_noncapturing_group_with_plus_oniguruma_mode() {
    let regex = compile("(?:)+", true).expect(
        "Oniguruma accepts `(?:)+`; fancy-regex in oniguruma_mode must match",
    );
    assert!(regex.is_match("anything").unwrap());
    assert!(regex.is_match("").unwrap());
}

#[test]
fn empty_noncapturing_group_with_question_oniguruma_mode() {
    let regex = compile("(?:)?", true).expect("Oniguruma accepts `(?:)?`");
    assert!(regex.is_match("anything").unwrap());
    assert!(regex.is_match("").unwrap());
}

/// Repro of the real-world pattern that syntect produces for markdown
/// fenced-code-block closing, after backreference substitution where
/// one of the backrefs expanded to the empty string.
#[test]
fn markdown_fenced_code_block_closing_after_backref_substitution() {
    let pattern = "^(?x:\n  [ \\t]*\n  (\n    ```\n    (?:(?:`)*|(?:)*)\n  )\n  (\\s*(?m:$)\\n?)\n)";
    let regex = compile(pattern, true).expect(
        "syntect-generated markdown fence pattern must compile in oniguruma_mode",
    );
    assert!(regex.is_match("   ```\n").unwrap());
}
