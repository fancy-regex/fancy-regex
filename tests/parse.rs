use fancy_regex::Regex;

#[test]
fn word_boundary_brace_parsing_ok() {
    // Valid patterns should succeed
    assert!(Regex::new(r"\b{start}").is_ok());
    assert!(Regex::new(r"\b{end}").is_ok());
    assert!(Regex::new(r"\b{start-half}").is_ok());
    assert!(Regex::new(r"\b{end-half}").is_ok());
}

#[test]
fn word_boundary_brace_parsing_err() {
    let test_cases = [
        // (pattern, expected_message_patten)
        (r"\b{invalid}", "\\b{invalid}"),
        (r"\b{start ", "\\b{...}"),
        (r"\b{end ", "\\b{...}"),
        (r"\b{}", "\\b{}"),
        (r"\b{", "\\b{...}"),
        (r"\b{ }", "\\b{ }"),
        (r"\b{START}", "\\b{START}"),
        (r"\b{END}", "\\b{END}"),
        (r"\B{start}", "\\B{start}"),
        (r"\B{end}", "\\B{end}"),
        (r"\B{start-half}", "\\B{start-half}"),
        (r"\B{end-half}", "\\B{end-half}"),
    ];
    for (pattern, expected_message_pattern) in test_cases {
        assert_parse_error(
            pattern,
            &format!(
                "Parsing error at position 0: Invalid escape: {}",
                expected_message_pattern
            ),
        );
    }
}

#[test]
fn incomplete_escape_sequences_err() {
    // See GH-76
    let test_cases = ["\\u", "\\U", "\\x"];
    for pattern in test_cases {
        assert_parse_error(pattern, "Parsing error at position 2: Invalid hex escape");
    }
}

#[cfg_attr(feature = "track_caller", track_caller)]
fn assert_parse_error(pattern: &str, expected_message: &str) {
    let result = Regex::new(pattern);
    assert!(
        result.is_err(),
        "Expected pattern '{}' to fail parsing",
        pattern
    );
    let error_message = result.unwrap_err().to_string();
    assert_eq!(
        error_message, expected_message,
        "Expected error message to contain '{}', but got: '{}'",
        expected_message, error_message
    );
}
