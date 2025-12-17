use fancy_regex::{Error, RegexBuilder, RuntimeError};

mod common;

#[test]
fn control_character_escapes() {
    assert_match(r"\a", "\x07");
    assert_match(r"\e", "\x1B");
    assert_match(r"\f", "\x0C");
    assert_match(r"\n", "\x0A");
    assert_match(r"\r", "\x0D");
    assert_match(r"\t", "\x09");
    assert_match(r"\v", "\x0B");
}

#[test]
fn character_class_escapes() {
    assert_match(r"[\[]", "[");
    assert_match(r"[\^]", "^");

    // The regex crate would reject the following because it's not necessary to escape them.
    // Other engines allow to escape any non-alphanumeric character.
    assert_match(r"[\<]", "<");
    assert_match(r"[\>]", ">");
    assert_match(r"[\.]", ".");
    assert_match(r"[\ ]", " ");

    // Character class escape
    assert_match(r"[\d]", "1");

    // Control characters
    assert_match(r"[\e]", "\x1B");
    assert_match(r"[\n]", "\x0A");

    // `]` can be unescaped if it's right after `[`
    assert_match(r"[]]", "]");
    // `]` can be unescaped even after `[^`
    assert_match(r"[^]]", "a");
}

#[test]
fn character_class_nested() {
    assert_match(r"[[a][bc]]", "c");
    assert_match(r"[a[^b]]", "c");
}

#[test]
fn character_class_intersection() {
    assert_match(r"[\w&&a-c]", "c");
    assert_no_match(r"[\w&&a-c]", "d");

    assert_match(r"[[0-9]&&[^4]]", "1");
    assert_no_match(r"[[0-9]&&[^4]]", "4");
}

#[test]
fn alternation_with_empty_arm() {
    assert_match(r"^(a|)$", "a");
    assert_match(r"^(a|)$", "");
    assert_match(r"^(|a)$", "a");
    assert_match(r"^(|a)$", "");
    assert_match(r"a|", "a");
    assert_match(r"a|", "");
    assert_match(r"|a", "a");
    assert_match(r"|a", "");
    assert_no_match(r"^(a|)$", "b");
}

#[test]
fn case_insensitive_character_class() {
    assert_match(r"^(?i)[a-z]+$", "aB");
}

#[test]
fn case_insensitive_escape() {
    // `\x61` is lowercase `a`
    assert_match(r"(?i)\x61", "A");

    // `\p{Ll}` is the "Letter, lowercase" category
    assert_match(r"(?i)\p{Ll}", "A");
}

#[test]
fn atomic_group() {
    assert_match(r"^a(?>bc|b)c$", "abcc");
    assert_no_match(r"^a(?>bc|b)c$", "abc");

    // Look-ahead forces use of VM
    assert_match(r"^a(bc(?=d)|b)cd$", "abcd");
    assert_no_match(r"^a(?>bc(?=d)|b)cd$", "abcd");
}

#[test]
fn backtrack_limit() {
    let re = RegexBuilder::new("(?i)(a|b|ab)*(?>c)")
        .backtrack_limit(100_000)
        .build()
        .unwrap();
    let s = "abababababababababababababababababababababababababababab";
    let result = re.is_match(s);
    assert!(result.is_err());
    match result.err() {
        Some(Error::RuntimeError(RuntimeError::BacktrackLimitExceeded)) => {}
        _ => panic!("Expected RuntimeError::BacktrackLimitExceeded"),
    }
}

#[test]
fn end_of_hard_expression_cannot_be_delegated() {
    assert_match(r"(?!x)(?:a|ab)c", "abc");
    // If `(?:a|ab)` is delegated, there's no backtracking and `a` matches and `ab` is never tried.
    assert_match(r"((?!x)(?:a|ab))c", "abc");
}

#[test]
fn issue103() {
    assert_no_match(r"(([ab]+)\1b)", "babab");
}

#[test]
fn backreference_validity_checker() {
    assert_match(r"(a)(?(1))", "a");
    assert_match(r"(?<group1>a)(?('group1'))b", "ab");
    assert_match(r"(a)(b)?(?(2))", "ab");
    assert_no_match(r"(a)(b)?(?(2))", "a");
    assert_match(r"(a)(b?)(?(2))", "a");
}

#[test]
fn conditional_with_backref_validity() {
    assert_match(r"(a)?b(?(1)c|d)", "bd");
    assert_match(r"(a)?b(?(1)c|d)", "abc");
    assert_match(r"(?<group1>a)?b(?(<group1>)c|d)", "abc");
    assert_no_match(r"(a)?b(?(1)c|d)", "bc");
    assert_no_match(r"^(a)?b(?(1)c|d)$", "abd");
}

#[test]
fn conditional_with_consuming_condition() {
    assert_match(r"^(?(ab)c|d)$", "abc");
    assert_no_match(r"^(?(ab)c|d)$", "abd");
    assert_no_match(r"^(?(ab)c|d)$", "ad");

    assert_match(r"^(?(\d)abc|\d!)$", "5abc");
    assert_no_match(r"^(?(\d)abc|\d!)$", "5!");
}

#[test]
fn conditional_with_lookaround_condition() {
    assert_match(r"^(?((?=\d))\w+|!)$", "!");
    assert_match(r"^(?((?=\d))\w+|!)$", "5abc");
    assert_match(r"^(?((?=\d))abc)$", "");
    assert_match(r"^(?((?=\w))abc)$", "abc");
    assert_no_match(r"^(?((?=\d))\wabc|\d!)$", "5!");
}

#[test]
fn backrefs() {
    assert_match(r"(abc)\1", "abcabc");
    assert_match(r"(abc|def)\1", "abcabc");
    assert_no_match(r"(abc|def)\1", "abcdef");
    assert_match(r"(abc|def)\1", "defdef");

    assert_no_match(r"(abc|def)\1", "abcABC");
    assert_match(r"(abc|def)(?i:\1)", "abcABC");
    assert_match(r"(abc|def)(?i:\1)", "abcAbc");
    assert_no_match(r"(abc|def)(?i:\1)", "abcAB");
    assert_no_match(r"(abc|def)(?i:\1)", "abcdef");

    assert_match(r"(δ)(?i:\1)", "δΔ");
    assert_no_match(r"(δ)\1", "δΔ");
    assert_no_match(r"(δδ)\1", "δΔfoo");
    assert_no_match(r"(δδ)\1", "δΔ");

    assert_match(r"(.)(?i:\1)", "\\\\");
    assert_match(r"(.)(?i:\1)", "((");

    assert_match(r"(.)(?i:\1)", "įĮ");
    assert_no_match(r"(.)(?i:\1)", "įi");
    assert_no_match(r"(.)(?i:\1)", "įĖ");

    assert_match(r"(?i)(?<word>\w+)\s+\k<word>", "Greek : δ Δ");
}

#[test]
fn easy_trailing_positive_lookaheads() {
    assert_match(r"(?=c)", "abcabc");
    assert_match(r"abc(?=abc)", "abcabc");
    assert_no_match(r"abc(?=abc)", "abcdef");
    assert_match(r"abc(?=a|b)", "abcabc");
    assert_no_match(r"abc(?=a|f)", "f");
}

#[test]
fn hard_trailing_positive_lookaheads() {
    assert_match(r"(abc|def)(?=\1)", "defdef");
    assert_match(r"(abc|def)(?=a(?!b))", "abca");
    assert_match(r"(abc|def)(?=a(?!b))", "abcaa");
    assert_no_match(r"(abc|def)(?=a(?!b))", "abcabc");
}

#[test]
fn word_boundary_brace_syntax() {
    // \b{start}
    assert_match(r"\b{start}world", "hello world");
    assert_no_match(r"\b{start}world", "helloworld");
    assert_match(r"\b{start}test", "test case");
    assert_no_match(r"\b{start}test", "contest");
    assert_no_match(r"\b{start}-test", "run --test");

    // \b{start-half}
    assert_match(r"\b{start-half}world", "hello world");
    assert_no_match(r"\b{start-half}world", "helloworld");
    assert_match(r"\b{start-half}test", "test case");
    assert_no_match(r"\b{start-half}test", "contest");
    assert_match(r"\b{start-half}-test", "run --test"); // different from \b{start}

    // \b{end}
    assert_match(r"world\b{end}", "hello world");
    assert_no_match(r"world\b{end}", "worldhello");
    assert_match(r"test\b{end}", "run test");
    assert_no_match(r"test\b{end}", "testing");
    assert_no_match(r"-1-\b{end}", "chapter -1-");

    // \b{end-half}
    assert_match(r"world\b{end-half}", "hello world");
    assert_no_match(r"world\b{end-half}", "worldhello");
    assert_match(r"test\b{end-half}", "run test");
    assert_no_match(r"test\b{end-half}", "testing");
    assert_match(r"-1-\b{end-half}", "chapter -1-");

    // repetition
    assert_match(r"\b\bworld", "hello world");
    assert_match(r"\b{2}world", "hello world");
    assert_no_match(r"\b{2}orld", "hello world");
    assert_match(r"world\b{,3}", "hello world");
    assert_match(r"\B{5}est", "run test");
    assert_no_match(r"\B{2}test", "run test");

    // `\b{start}` <=> `\<` and `\b{end}` <=> `\>`
    let test_cases = [
        ("hello world", r"\b{start}world", r"\<world"),
        ("hello world", r"world\b{end}", r"world\>"),
        ("hello world there", r"\b{start}world\b{end}", r"\<world\>"),
    ];

    for (text, b_pattern, angle_pattern) in test_cases {
        let b_result = match_text(b_pattern, text);
        let angle_result = match_text(angle_pattern, text);
        assert_eq!(
            b_result, angle_result,
            "Pattern '{}' and '{}' should match '{}' equivalently",
            b_pattern, angle_pattern, text
        );
    }
}

#[cfg_attr(feature = "track_caller", track_caller)]
fn assert_match(re: &str, text: &str) {
    let result = match_text(re, text);
    assert_eq!(
        result, true,
        "Expected regex '{}' to match text '{}'",
        re, text
    );
}

#[cfg_attr(feature = "track_caller", track_caller)]
fn assert_no_match(re: &str, text: &str) {
    let result = match_text(re, text);
    assert_eq!(
        result, false,
        "Expected regex '{}' to not match text '{}'",
        re, text
    );
}

#[test]
fn unicode_property_remapping_outside_char_class() {
    // Test \p{alnum} - should match alphanumeric characters
    assert_match(r"\p{alnum}", "a");
    assert_match(r"\p{alnum}", "1");
    assert_match(r"\p{alnum}", "š");
    assert_no_match(r"\p{alnum}", " ");
    assert_no_match(r"\p{alnum}", "_");
    assert_no_match(r"\p{alnum}", "-");

    // Test \p{blank} - should match space and tab
    assert_match(r"\p{blank}", " ");
    assert_match(r"\p{blank}", "\t");
    assert_no_match(r"\p{blank}", "a");
    assert_no_match(r"\p{blank}", "\n");
    assert_no_match(r"\p{blank}", "1");
    assert_no_match(r"\p{blank}", "_");
    assert_no_match(r"\p{blank}", "-");

    // Test \p{word} - should match word characters
    assert_match(r"\p{word}", "a");
    assert_match(r"\p{word}", "_");
    assert_match(r"\p{alnum}", "1");
    assert_match(r"\p{alnum}", "š");
    assert_no_match(r"\p{word}", " ");
    assert_no_match(r"\p{word}", "-");

    // Test non-remapped properties still work. L matches letters if unnegated.
    assert_match(r"\p{L}", "a");
    assert_match(r"\p{L}", "š");
    assert_no_match(r"\p{L}", "1");
    assert_no_match(r"\p{L}", "_");
    assert_no_match(r"\p{L}", "-");
    assert_no_match(r"\p{L}", " ");

    assert_no_match(r"\P{L}", "a");
    assert_no_match(r"\P{L}", "š");
    assert_match(r"\P{L}", "1");
    assert_match(r"\P{L}", " ");
    assert_match(r"\P{L}", "-");
}

#[test]
fn unicode_property_remapping_inside_char_class() {
    // Test \p{alnum} - should match alphanumeric characters
    let alnum_pattern = r"[\p{alnum}]";
    assert_match(alnum_pattern, "a");
    assert_match(alnum_pattern, "1");
    assert_match(alnum_pattern, "š");
    assert_no_match(alnum_pattern, " ");
    assert_no_match(alnum_pattern, "]");

    // Test \p{blank} - should match space and tab
    let blank_pattern = r"[\p{blank}]";
    assert_match(blank_pattern, " ");
    assert_match(blank_pattern, "\t");
    assert_no_match(blank_pattern, "a");
    assert_no_match(blank_pattern, "\n");
    assert_no_match(blank_pattern, "]");

    let bang_dash_hash_blank_pattern = r"[!-#\p{blank}]";
    assert_match(bang_dash_hash_blank_pattern, "!");
    assert_match(bang_dash_hash_blank_pattern, " ");
    assert_match(bang_dash_hash_blank_pattern, "\t");
    assert_no_match(bang_dash_hash_blank_pattern, "a");

    // Test \p{word} - should match word characters
    let word_pattern = r"[\p{word}]";
    assert_match(word_pattern, "a");
    assert_match(word_pattern, "_");
    assert_no_match(word_pattern, " ");
    assert_no_match(word_pattern, "-");
    assert_no_match(word_pattern, "]");

    // Test non-remapped properties still work. L matches letters if unnegated.
    let l_pattern = r"[\p{L}]";
    assert_match(l_pattern, "a");
    assert_no_match(l_pattern, "1");
    assert_no_match(l_pattern, " ");

    let not_l_pattern = r"[\P{L}]";
    assert_no_match(not_l_pattern, "a");
    assert_match(not_l_pattern, "1");
    assert_match(not_l_pattern, " ");

    let slash_not_alnum_pattern = r"[/\P{alnum}]";
    assert_no_match(slash_not_alnum_pattern, "a");
    assert_match(slash_not_alnum_pattern, "/");
}

#[test]
fn char_class_negated_property_as_only_item() {
    // Test negated properties as the first (and only) item in a char class

    // \P{alnum} as only content - should match non-alphanumeric characters
    let not_alnum_pattern = r"[\P{alnum}]";
    assert_no_match(not_alnum_pattern, "a");
    assert_no_match(not_alnum_pattern, "Z");
    assert_no_match(not_alnum_pattern, "š");
    assert_no_match(not_alnum_pattern, "0");
    assert_no_match(not_alnum_pattern, "9");
    assert_match(not_alnum_pattern, " ");
    assert_match(not_alnum_pattern, "!");
    assert_match(not_alnum_pattern, "-");
    assert_match(not_alnum_pattern, "_");

    // \P{blank} as only content - should match non-blank characters
    let not_blank_pattern = r"[\P{blank}]";
    assert_match(not_blank_pattern, "a");
    assert_match(not_blank_pattern, "!");
    assert_no_match(not_blank_pattern, " ");
    assert_no_match(not_blank_pattern, "\t");

    // \P{word} as only content - should match non-word characters
    let not_word_pattern = r"[\P{word}]";
    assert_no_match(not_word_pattern, "a");
    assert_no_match(not_word_pattern, "š");
    assert_no_match(not_word_pattern, "_");
    assert_no_match(not_word_pattern, "0");
    assert_match(not_word_pattern, " ");
    assert_match(not_word_pattern, "-");
}

#[test]
fn char_class_negated_property_after_range() {
    // Test negated properties after ranges

    // Range followed by negated property
    let a_z_not_alnum_pattern = r"[a-z\P{alnum}]";
    assert_match(a_z_not_alnum_pattern, "a");
    assert_match(a_z_not_alnum_pattern, "z");
    assert_match(a_z_not_alnum_pattern, "!"); // non-alphanumeric
    assert_no_match(a_z_not_alnum_pattern, "A");
    assert_no_match(a_z_not_alnum_pattern, "0");

    // Another test
    let zero_nine_not_blank_pattern = r"[0-9\P{blank}]";
    assert_match(zero_nine_not_blank_pattern, "5");
    assert_match(zero_nine_not_blank_pattern, "a"); // non-blank
    assert_no_match(zero_nine_not_blank_pattern, " ");
    assert_no_match(zero_nine_not_blank_pattern, "\t");
}

#[test]
fn char_class_negated_property_after_intersection() {
    // Test negated properties after && operator

    // \w intersected with negated blank - should match word chars as usual
    let word_and_not_blank_pattern = r"[\w&&\P{blank}]";
    assert_match(word_and_not_blank_pattern, "a");
    assert_match(word_and_not_blank_pattern, "š");
    assert_match(word_and_not_blank_pattern, "0");
    assert_match(word_and_not_blank_pattern, "_");
    assert_no_match(word_and_not_blank_pattern, " ");
    assert_no_match(word_and_not_blank_pattern, "\t");
    assert_no_match(word_and_not_blank_pattern, "-");
}

#[test]
fn char_class_negated_in_negated_class() {
    // Test negated properties in a negated char class
    // [^\P{alnum}] should match alphanumeric (double negation)

    let not_not_alnum_pattern = r"[^\P{alnum}]";
    assert_match(not_not_alnum_pattern, "a");
    assert_match(not_not_alnum_pattern, "š");
    assert_match(not_not_alnum_pattern, "Z");
    assert_match(not_not_alnum_pattern, "0");
    assert_match(not_not_alnum_pattern, "9");
    assert_no_match(not_not_alnum_pattern, " ");
    assert_no_match(not_not_alnum_pattern, "!");
    assert_no_match(not_not_alnum_pattern, "_");

    // [^\P{word}] should match word characters
    let not_not_word_pattern = r"[^\P{word}]";
    assert_match(not_not_word_pattern, "a");
    assert_match(not_not_word_pattern, "š");
    assert_match(not_not_word_pattern, "_");
    assert_match(not_not_word_pattern, "0");
    assert_no_match(not_not_word_pattern, " ");
    assert_no_match(not_not_word_pattern, "-");
}

#[test]
fn char_class_negated_property_mixed() {
    // Test various combinations

    // Multiple items with negated property in the middle
    let mixed_pattern = r"[a-z0-9\P{blank}_\n]";
    assert_match(mixed_pattern, "a");
    assert_match(mixed_pattern, "5");
    assert_match(mixed_pattern, "_");
    assert_match(mixed_pattern, "!"); // non-blank
    assert_match(mixed_pattern, "A"); // non-blank (even though not in a-z or 0-9)
    assert_match(mixed_pattern, "š"); // non-blank (even though not in a-z or 0-9)
    assert_no_match(mixed_pattern, " "); // blank
    assert_no_match(mixed_pattern, "\t"); // blank

    // Multiple negated properties
    // [\P{alnum}\P{blank}] - matches non-alphanumeric intersected with non-blank
    let not_alnum_and_not_blank_pattern = r"[\P{alnum}\P{blank}]";
    assert_match(not_alnum_and_not_blank_pattern, "!");
    assert_match(not_alnum_and_not_blank_pattern, "-");
    assert_match(not_alnum_and_not_blank_pattern, "_");
    assert_no_match(not_alnum_and_not_blank_pattern, "a");
    assert_no_match(not_alnum_and_not_blank_pattern, "š");
    assert_no_match(not_alnum_and_not_blank_pattern, "0");
    assert_no_match(not_alnum_and_not_blank_pattern, " "); // blank
    assert_no_match(not_alnum_and_not_blank_pattern, "\t"); // blank
}

#[cfg_attr(feature = "track_caller", track_caller)]
fn match_text(re: &str, text: &str) -> bool {
    let regex = common::regex(re);
    let result = regex.is_match(text);
    assert!(
        result.is_ok(),
        "Expected match to succeed, but was {:?}",
        result
    );
    result.unwrap()
}
