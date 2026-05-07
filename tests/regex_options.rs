use fancy_regex::{CompileError, Error, ParseError, Regex, RegexBuilder};

fn build_regex(builder: &RegexBuilder) -> Regex {
    let result = builder.build();
    assert!(
        result.is_ok(),
        "Expected regex to build successfully, got {:?}",
        result.err()
    );
    result.unwrap()
}

#[test]
fn check_casing_option() {
    let regex = build_regex(RegexBuilder::new(r"TEST foo").case_insensitive(false));

    assert!(regex.is_match(r"TEST foo").unwrap_or_default());
    assert!(!regex.is_match(r"test foo").unwrap_or_default());
}

#[test]
fn check_override_casing_option() {
    let regex = build_regex(RegexBuilder::new(r"FOO(?i:bar)quux").case_insensitive(false));

    assert!(!regex.is_match("FoObarQuUx").unwrap_or_default());
    assert!(!regex.is_match("fooBARquux").unwrap_or_default());
    assert!(regex.is_match("FOObarquux").unwrap_or_default());
}

#[test]
fn check_casing_insensitive_option() {
    let regex = build_regex(RegexBuilder::new(r"TEST FOO").case_insensitive(true));

    assert!(regex.is_match(r"test foo").unwrap_or_default());
}

#[test]
fn check_multi_line_option() {
    let test_text = r"test
hugo
test";

    let regex = build_regex(RegexBuilder::new(r"^test$").multi_line(true));
    assert!(regex.is_match(test_text).unwrap_or_default());
}

#[test]
fn check_ignore_whitespace_option() {
    let regex = build_regex(RegexBuilder::new(r"test    foo").ignore_whitespace(true));

    let test_text = r"testfoo";
    assert!(regex.is_match(test_text).unwrap_or_default());
}

#[test]
fn check_dot_matches_new_line_option() {
    let regex = build_regex(RegexBuilder::new(r"<div>(.*?)<\/div>").dot_matches_new_line(true));

    let test_text = r"<div>
    hello</div>";

    assert!(regex.is_match(test_text).unwrap_or_default());
}

#[test]
fn check_casing_insensitive_option_hard() {
    let regex = build_regex(RegexBuilder::new(r"[a-z](?<=[^f])").case_insensitive(true));

    assert!(regex.is_match(r"J").unwrap_or_default());
    assert!(!regex.is_match(r"F").unwrap_or_default());
    assert!(regex.is_match(r"j").unwrap_or_default());
}

#[test]
fn check_ignore_whitespace_option_fancy() {
    let regex = build_regex(RegexBuilder::new(r"(?=test    foo)").ignore_whitespace(true));

    let test_text = r"testfoo";

    assert!(regex.is_match(test_text).unwrap_or_default());
}

#[test]
fn check_ignore_whitespace_with_lookahead_matches() {
    let regex = build_regex(RegexBuilder::new(r"(?=test    foo)").ignore_whitespace(true));

    let test_text = r"test    foo";

    assert!(!regex.is_match(test_text).unwrap_or_default());
}

#[test]
fn check_verbose_mode_option() {
    let pattern = "
test  foo #hugo
";
    let regex = build_regex(RegexBuilder::new(pattern).verbose_mode(true));

    let test_text = r"test    foo";

    assert!(!regex.is_match(test_text).unwrap_or_default());
}

#[test]
fn issue_163_fancy_email_test() {
    let pattern =
        r"^(?!\.)(?!.*\.\.)([a-z0-9_'+\-\.]*)[a-z0-9_'+\-]@([a-z0-9][a-z0-9\-]*\.)+[a-z]{2,}$";

    let regex = build_regex(RegexBuilder::new(pattern).case_insensitive(true));

    let test_text = "VALID@domain.com";
    assert!(regex.is_match(test_text).unwrap());
}

#[test]
fn check_oniguruma_mode_changes_wordbounds() {
    fn find_all_matches(regex: &Regex, text: &'static str) -> Vec<&'static str> {
        regex.find_iter(text).map(|m| m.unwrap().as_str()).collect()
    }

    let pattern = r"\<prefix_\w*\>";
    let test_text = "not_prefix_oops prefix_with_suffix <prefix_>";

    // By default the pattern is interpretted as a left word-bound followed by the literal "prefix_"
    // followed by any number of word characters ending on a right word-bound
    let default_mode = Regex::new(pattern).unwrap();
    let default_matches = find_all_matches(&default_mode, test_text);
    assert_eq!(default_matches, ["prefix_with_suffix", "prefix_"]);

    // When Oniguruma-mode is used the pattern is instead a literal "<prefix_" followed by any
    // number of word characters followed by a literal ">"
    let oniguruma_mode = build_regex(RegexBuilder::new(pattern).oniguruma_mode(true));
    let oniguruma_matches = find_all_matches(&oniguruma_mode, test_text);
    assert_eq!(oniguruma_matches, ["<prefix_>"]);
}

#[test]
fn empty_noncapturing_group_with_quantifier_oniguruma_mode() {
    // Oniguruma silently accepts quantifiers on empty non-capturing groups.
    // These patterns should compile and match any input (they are zero-width).
    let patterns = ["(?:)*", "(?:)+", "(?:)?", "(?:){3}", "(?:)*?", "(?:)++"];
    for pat in patterns {
        let regex = build_regex(RegexBuilder::new(pat).oniguruma_mode(true));
        assert!(
            regex.is_match("anything").unwrap(),
            "Expected '{}' to match",
            pat
        );
        assert!(
            regex.is_match("").unwrap(),
            "Expected '{}' to match empty",
            pat
        );
    }
}

#[test]
fn empty_noncapturing_group_with_quantifier_default_mode_errors() {
    // In default mode, quantifiers on empty non-capturing groups should fail.
    let result = RegexBuilder::new("(?:)*").build();
    assert!(result.is_err(), "Expected (?:)* to fail in default mode");
}

#[test]
fn markdown_fenced_code_block_empty_group_oniguruma_mode() {
    // Repro of a real-world pattern that syntect produces for markdown
    // fenced-code-block closing, where a backreference substitution
    // expands to the empty string.
    let pattern =
        "^(?x:\n  [ \\t]*\n  (\n    ```\n    (?:(?:`)*|(?:)*)\n  )\n  (\\s*(?m:$)\\n?)\n)";
    let regex = build_regex(RegexBuilder::new(pattern).oniguruma_mode(true));
    assert!(regex.is_match("   ```\n").unwrap());
}

#[test]
fn check_crlf_option() {
    // With CRLF mode and multi-line enabled, ^ and $ should treat \r\n as a single line ending
    let regex = build_regex(RegexBuilder::new(r"^test$").multi_line(true).crlf(true));
    assert!(regex.is_match("test").unwrap_or_default());
    assert!(regex.is_match("\r\ntest\r\n").unwrap_or_default());
    assert!(regex.is_match("test\r\n").unwrap_or_default());
    assert!(regex.is_match("\r\ntest").unwrap_or_default());
}

#[test]
fn check_crlf_flag_in_pattern() {
    // The (?mR) flag combination in the pattern itself should enable CRLF mode
    let regex = build_regex(&RegexBuilder::new(r"(?mR)^test$"));
    assert!(regex.is_match("test").unwrap_or_default());
    assert!(regex.is_match("\r\ntest\r\n").unwrap_or_default());
    assert!(regex.is_match("test\r\n").unwrap_or_default());
    assert!(regex.is_match("\r\ntest").unwrap_or_default());
}

#[test]
fn can_build_multiple_regexes_with_same_options() {
    let mut builder = RegexBuilder::new(r"^[a-z@.]+$");
    builder.case_insensitive(true);
    let regex1 = build_regex(&builder);

    let test_text = "VALID@domain.com";
    assert!(regex1.is_match(test_text).unwrap());

    let regex2 = build_regex(&builder.pattern(r"AB.$".to_string()));

    let test_text = "abc";
    assert!(regex2.is_match(test_text).unwrap());
}

#[test]
fn changing_builder_options_has_no_effect_on_already_built_regexes() {
    let pattern1 = r"^[a-z@.]+$";
    let pattern2 = r"AB.$";

    let mut builder = RegexBuilder::new(pattern1);
    builder.case_insensitive(true);
    let regex1 = build_regex(&builder);
    let regex2 = build_regex(&builder.case_insensitive(false));
    let regex3 = build_regex(&builder.pattern(pattern2.to_string()));
    let regex4 = build_regex(&builder.case_insensitive(true));

    let test_text1 = "VALID@domain.com";
    let test_text2 = "abc";

    assert!(regex1.is_match(test_text1).unwrap());
    assert!(!regex2.is_match(test_text1).unwrap());
    assert!(!regex3.is_match(test_text2).unwrap());
    assert!(regex4.is_match(test_text2).unwrap());
}

#[test]
fn check_find_not_empty_option() {
    // \d* can match empty, so without find_not_empty it matches at position 0 with zero length.
    // With find_not_empty enabled, that zero-length match is rejected and find returns None.
    let pattern = r"\d*";
    let text = "hello";

    let normal = build_regex(RegexBuilder::new(pattern).find_not_empty(false));
    assert_eq!(
        normal.find(text).unwrap().map(|m| (m.start(), m.end())),
        Some((0, 0))
    );

    let not_empty = build_regex(RegexBuilder::new(pattern).find_not_empty(true));
    assert_eq!(
        not_empty.find(text).unwrap().map(|m| (m.start(), m.end())),
        None
    );
}

#[test]
fn check_find_not_empty_rejects_zero_width_only_patterns() {
    // Patterns that can only ever produce zero-length matches should fail to compile
    // when find_not_empty is set, since they can never return a result.
    let zero_width_patterns = vec![
        r"^", r"$", r"\b", r"(?!bar)", r"(?<=x)", r"(?<!y)", r"(?=.)", r"\G",
    ];

    for pattern in zero_width_patterns {
        let result = RegexBuilder::new(pattern).find_not_empty(true).build();
        assert!(
            matches!(
                result,
                Err(Error::CompileError(ref e)) if matches!(**e, CompileError::PatternCanNeverMatch)
            ),
            "Expected PatternCanNeverMatch for pattern {:?}, got {:?}",
            pattern,
            result,
        );
    }
}

#[test]
fn check_find_not_empty_allows_patterns_that_can_match_non_empty() {
    // Patterns that can produce non-empty matches should compile fine even with find_not_empty
    let patterns = vec![
        r"\d*",
        r"a?",
        r"\d+",
        r"[a-z]+",
        r"(?:foo)?",
        r"\d*(?=x)",
        r"a(?=b)",
    ];

    for pattern in patterns {
        let result = RegexBuilder::new(pattern).find_not_empty(true).build();
        assert!(
            result.is_ok(),
            "Expected pattern {:?} to compile with find_not_empty, got {:?}",
            pattern,
            result.err(),
        );
    }
}

#[test]
fn check_find_not_empty_is_match() {
    // is_match should respect find_not_empty: \d* normally matches empty at position 0
    // in "hello", but with find_not_empty it should return false.
    let pattern = r"\d*";
    let text = "hello";

    let normal = build_regex(RegexBuilder::new(pattern).find_not_empty(false));
    assert!(normal.is_match(text).unwrap());

    let not_empty = build_regex(RegexBuilder::new(pattern).find_not_empty(true));
    assert!(!not_empty.is_match(text).unwrap());
}

#[test]
fn check_find_not_empty_allows_trailing_lookahead_with_content() {
    // a?(?=b) optimizes to (a?)b — group 0 is "a?" which is not always empty, so it should compile
    let result = RegexBuilder::new(r"a(?=b)").find_not_empty(true).build();
    assert!(
        result.is_ok(),
        "Expected a?(?=b) to compile with find_not_empty"
    );

    let regex = result.unwrap();
    assert!(regex.is_match("ab").unwrap());
    assert!(!regex.is_match("ac").unwrap());
}

// --- unicode_mode tests ---
//
// These tests document the behavior of `unicode_mode(false)` when matching
// against `str` input (the default, which corresponds to UTF-8 encoded text).
//
// Summary:
// - Character classes `\w`, `\d`, `\s` are honoured: they become ASCII-only.
// - Negated classes `\W`, `\D`, `\S`, bare `.`, and Unicode properties
//   `\p{...}` fail to compile, because they could match individual bytes that
//   violate UTF-8 boundaries (the underlying engine requires valid UTF-8 when
//   operating on `str`).
// - The inline `(?-u)` flag is rejected when it would change the unicode mode
//   (i.e. when unicode_mode(true) is active, the default), but accepted as a
//   no-op when it matches the current builder setting (i.e. unicode_mode(false)).
// - The unicode flag is also honoured in "hard" (backtracking-VM) patterns
//   such as those containing lookarounds.
//
// If you need to use `unicode_mode(false)` with `.`, `\W`, `\D`, `\S`, or
// Unicode properties, use the bytes API (`BytesMode::Ascii`) instead.

/// `\w` with `unicode_mode(false)` on `str` input matches only ASCII word chars.
#[test]
fn unicode_mode_false_w_is_ascii_only_on_str() {
    // With unicode enabled, \w matches Unicode letters such as 'é'.
    let re_unicode = build_regex(RegexBuilder::new(r"^\w+$").unicode_mode(true));
    assert!(re_unicode.is_match("café").unwrap());

    // With unicode disabled, \w matches only [a-zA-Z0-9_], so 'café' (which
    // contains the non-ASCII 'é') no longer matches when anchored.
    let re_ascii = build_regex(RegexBuilder::new(r"^\w+$").unicode_mode(false));
    assert!(!re_ascii.is_match("café").unwrap());
    // Pure ASCII still matches.
    assert!(re_ascii.is_match("cafe").unwrap());
}

/// `\d` with `unicode_mode(false)` on `str` input matches only ASCII digits.
#[test]
fn unicode_mode_false_d_is_ascii_only_on_str() {
    // Arabic-Indic digit THREE (U+0663) is matched by \d in Unicode mode.
    let arabic_digit = "\u{0663}";

    let re_unicode = build_regex(RegexBuilder::new(r"^\d+$").unicode_mode(true));
    assert!(re_unicode.is_match(arabic_digit).unwrap());

    let re_ascii = build_regex(RegexBuilder::new(r"^\d+$").unicode_mode(false));
    assert!(!re_ascii.is_match(arabic_digit).unwrap());
    // Plain ASCII digit still matches.
    assert!(re_ascii.is_match("3").unwrap());
}

/// `\s` with `unicode_mode(false)` on `str` input matches only ASCII whitespace.
#[test]
fn unicode_mode_false_s_is_ascii_only_on_str() {
    // EM SPACE (U+2003) is a Unicode space character but not ASCII whitespace.
    let em_space = "\u{2003}";

    let re_unicode = build_regex(RegexBuilder::new(r"^\s$").unicode_mode(true));
    assert!(re_unicode.is_match(em_space).unwrap());

    let re_ascii = build_regex(RegexBuilder::new(r"^\s$").unicode_mode(false));
    assert!(!re_ascii.is_match(em_space).unwrap());
    // Plain ASCII space still matches.
    assert!(re_ascii.is_match(" ").unwrap());
}

/// `\W`, `\D`, and `\S` fail to compile with `unicode_mode(false)` on `str`
/// input because their negation could match byte sequences that are not valid
/// UTF-8 codepoint boundaries.
#[test]
fn unicode_mode_false_negated_classes_fail_to_compile_on_str() {
    for pattern in [r"\W", r"\D", r"\S"] {
        let result = RegexBuilder::new(pattern).unicode_mode(false).build();
        assert!(
            result.is_err(),
            "Expected '{}' to fail to compile with unicode_mode(false) on str input",
            pattern
        );
    }
}

/// `.` (bare dot) fails to compile with `unicode_mode(false)` on `str` input
/// for the same reason: it could match arbitrary bytes.
#[test]
fn unicode_mode_false_dot_fails_to_compile_on_str() {
    let result = RegexBuilder::new(r".").unicode_mode(false).build();
    assert!(
        result.is_err(),
        "Expected '.' to fail to compile with unicode_mode(false) on str input"
    );
}

/// `\p{...}` Unicode properties fail to compile with `unicode_mode(false)`.
#[test]
fn unicode_mode_false_unicode_properties_fail_to_compile() {
    let result = RegexBuilder::new(r"\p{L}").unicode_mode(false).build();
    assert!(
        result.is_err(),
        "{}",
        "Expected \\p{{L}} to fail to compile with unicode_mode(false)"
    );
}

/// The inline `(?-u)` flag is rejected when unicode mode is **enabled**
/// (the default), because it would attempt to change the mode. It is accepted
/// as a no-op when unicode mode is **already disabled** via the builder.
/// Conversely, inline `(?u)` is accepted when unicode mode is already enabled,
/// and rejected when it is disabled.
#[test]
fn unicode_mode_inline_flag_only_allowed_when_matching_builder_setting() {
    // (?-u) while unicode is ON (default) → error: would change the mode
    let result = Regex::new(r"(?-u)\w+");
    assert!(
        matches!(
            result,
            Err(Error::ParseError(
                _,
                ParseError::ChangingUnicodeModeUnsupported
            ))
        ),
        "Expected ChangingUnicodeModeUnsupported for (?-u) with unicode enabled, got {:?}",
        result
    );

    // (?-u) while unicode is OFF → accepted (no-op)
    let result = RegexBuilder::new(r"(?-u)\w+").unicode_mode(false).build();
    assert!(
        result.is_ok(),
        "Expected (?-u) to be accepted when unicode_mode(false), got {:?}",
        result
    );

    // (?u) while unicode is ON (default) → accepted (no-op)
    let result = Regex::new(r"(?u)\w+");
    assert!(
        result.is_ok(),
        "Expected (?u) to be accepted when unicode is enabled (default), got {:?}",
        result
    );

    // (?u) while unicode is OFF → error: would change the mode
    let result = RegexBuilder::new(r"(?u)\w+").unicode_mode(false).build();
    assert!(
        matches!(
            result,
            Err(Error::ParseError(
                _,
                ParseError::ChangingUnicodeModeUnsupported
            ))
        ),
        "Expected ChangingUnicodeModeUnsupported for (?u) with unicode disabled, got {:?}",
        result
    );
}

/// `unicode_mode(false)` is also honoured in "hard" (backtracking-VM) patterns
/// such as those containing lookaheads.
#[test]
fn unicode_mode_false_hard_pattern_w_is_ascii_only_on_str() {
    // The lookahead makes this a "hard" pattern processed by the VM.
    let re_unicode = build_regex(RegexBuilder::new(r"^\w+(?=!)").unicode_mode(true));
    let re_ascii = build_regex(RegexBuilder::new(r"^\w+(?=!)").unicode_mode(false));

    // With unicode enabled, \w matches 'é' so the whole pattern matches.
    assert!(re_unicode.is_match("café!").unwrap());
    // With unicode disabled, \w is ASCII-only, so the pattern does not match.
    assert!(!re_ascii.is_match("café!").unwrap());
    // Pure-ASCII input still matches with unicode disabled.
    assert!(re_ascii.is_match("cafe!").unwrap());
}
