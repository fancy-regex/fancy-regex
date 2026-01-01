mod common;

use fancy_regex::Match;
use std::ops::Range;

#[test]
fn match_api() {
    let m = find_match(r"(\w+)", "... test").unwrap();
    assert_eq!(m.range(), (4..8));
    assert_eq!(Range::from(m), (4..8));
    assert_eq!(m.as_str(), "test");
}

#[test]
fn find_wrap() {
    assert_eq!(find(r"(\w+)", "... test"), Some((4, 8)));
    assert_eq!(find(r"(?m)^yes$", "foo\nyes\n"), Some((4, 7)));
}

#[test]
fn find_fancy_case_insensitive() {
    assert_eq!(find(r"(x|xy)\1", "XX"), None);
    assert_eq!(find(r"(x|xy)\1", "xx"), Some((0, 2)));
    assert_eq!(find(r"((?i:x|xy))\1", "XX"), Some((0, 2)));
}

#[test]
fn lookahead_grouping_single_expression() {
    // These would fail if the delegate expression was `^x|a` (if we didn't
    // group as `^(?:x|a)`).
    assert_eq!(find(r"(?=x|a)", "a"), Some((0, 0)));
    assert_eq!(find(r"(?=x|a)", "bbba"), Some((3, 3)));
}

#[test]
fn lookahead_grouping_multiple_expressions() {
    // These would fail if the delegate expression was `^ab|Bc` (if we didn't
    // preserve grouping of `(?:b|B)`).
    assert_eq!(find(r"(?=(?!x)a(?:b|B)c)", "aBc"), Some((0, 0)));
    assert_eq!(find(r"(?=(?!x)a(?:b|B)c)", "Bc"), None);
}

#[test]
fn lookbehind_grouping_single_expression() {
    assert_eq!(find(r"(?<=x|a)", "a"), Some((1, 1)));
    assert_eq!(find(r"(?<=x|a)", "ba"), Some((2, 2)));
    assert_eq!(find(r"(?<=^a)", "a"), Some((1, 1)));
    assert_eq!(find(r"(?<=^a)", "ba"), None);
}

#[test]
fn lookbehind_variable_sized_alt() {
    // These get compiled into multiple alternative lookbehinds
    assert_eq!(find(r"(?<=a|bc)", "xxa"), Some((3, 3)));
    assert_eq!(find(r"(?<=a|bc)", "xxbc"), Some((4, 4)));
    assert_eq!(find(r"(?<=a|bc)", "xx"), None);
    assert_eq!(find(r"(?<=a|bc)", "xxb"), None);
    assert_eq!(find(r"(?<=a|bc)", "xxc"), None);
}

#[test]
fn negative_lookbehind_variable_sized_alt() {
    assert_eq!(find(r"(?<!a|bc)x", "axx"), Some((2, 3)));
    assert_eq!(find(r"(?<!a|bc)x", "bcxx"), Some((3, 4)));
    assert_eq!(find(r"(?<!a|bc)x", "x"), Some((0, 1)));
    assert_eq!(find(r"(?<!a|bc)x", "bx"), Some((1, 2)));
    assert_eq!(find(r"(?<!a|bc)x", "cx"), Some((1, 2)));
    assert_eq!(find(r"(?<!a|bc)x", "ax"), None);
    assert_eq!(find(r"(?<!a|bc)x", "bcx"), None);
}

#[test]
#[cfg(feature = "variable-lookbehinds")]
fn lookbehind_positive_variable_sized_functionality_easy() {
    assert_eq!(find(r"(?<=a(?:b|cd))x", "abx"), Some((2, 3)));
    assert_eq!(find(r"(?<=a(?:b|cd))x", "acdx"), Some((3, 4)));
    assert_eq!(find(r"(?<=a(?:b|cd))x", "ax"), None);
    assert_eq!(find(r"(?<=a(?:b|cd))x", "bcx"), None);

    assert_eq!(find(r"(?<=a+b+)x", "abx"), Some((2, 3)));
    assert_eq!(find(r"(?<=a+b+)x", "aabbx"), Some((4, 5)));
    assert_eq!(find(r"(?<=a+b+)x", "aaabbbx"), Some((6, 7)));
    assert_eq!(find(r"(?<=a+b+)x", "ax"), None);
    assert_eq!(find(r"(?<=a+b+)x", "bx"), None);
    assert_eq!(find(r"(?<=a+b+)x", "abcx"), None);
    assert_eq!(find(r"(?<=a+b+)c", "bc aabbc"), Some((7, 8)));

    assert_eq!(find(r"\b(?<=\|\s{0,9})(?:[gG]pu)\b", "|gpu"), Some((1, 4)));
    assert_eq!(
        find(r"\b(?<=\|\s{0,9})(?:[gG]pu)\b", "|  gpu"),
        Some((3, 6))
    );
}

#[test]
#[cfg(feature = "variable-lookbehinds")]
fn lookbehind_negative_variable_sized_functionality_easy() {
    assert_eq!(find(r"(?<!a(?:b|cd))x", "abx"), None);
    assert_eq!(find(r"(?<!a(?:b|cd))x", "acdx"), None);
    assert_eq!(find(r"(?<!a(?:b|cd))x", "ax"), Some((1, 2)));
    assert_eq!(find(r"(?<!a(?:b|cd))x", "bcx"), Some((2, 3)));

    assert_eq!(find(r"(?<!a+b+)x", "abx"), None);
    assert_eq!(find(r"(?<!a+b+)x", "aabbx"), None);
    assert_eq!(find(r"(?<!a+b+)x", "aaabbbx"), None);
    assert_eq!(find(r"(?<!a+b+)x", "ax"), Some((1, 2)));
    assert_eq!(find(r"(?<!a+b+)x", "bx"), Some((1, 2)));
    assert_eq!(find(r"(?<!a+b+)x", "abcx"), Some((3, 4)));
    assert_eq!(find(r"(?<!a+b+)c", "aabbc bc"), Some((7, 8)));

    assert_eq!(find(r"\b(?<!\|\s{0,9})(?:[gG]pu)\b", "|gpu"), None);
    assert_eq!(find(r"\b(?<!\|\s{0,9})(?:[gG]pu)\b", "|  gpu"), None);
}

#[test]
#[cfg(feature = "variable-lookbehinds")]
fn lookbehind_with_word_boundary_and_variable_length() {
    // Test (?<=\ba+)b - word boundary followed by variable-length a's
    assert_eq!(find(r"(?<=\ba+)b", "aaab"), Some((3, 4)));
    assert_eq!(find(r"(?<=\ba+)b", " aaab"), Some((4, 5)));
    assert_eq!(find(r"(?<=\ba+)b", "ab"), Some((1, 2)));
    assert_eq!(find(r"(?<=\ba+)b", "xaaab"), None); // No word boundary
    assert_eq!(find(r"(?<=\ba+)b", "b"), None); // No 'a's before 'b'

    // Test (?<=\Ba+)b - NOT word boundary followed by variable-length a's
    assert_eq!(find(r"(?<=\Ba+)b", "a_aab"), Some((4, 5)));

    // Test negative lookbehind with word boundary
    assert_eq!(find(r"(?<!\ba+)b", "xaaab"), Some((4, 5))); // 'a's not preceded by word boundary
    assert_eq!(find(r"(?<!\ba+)b", "aaab"), None); // Word boundary before 'a's
    assert_eq!(find(r"(?<!\ba+)b", " aaab"), None); // Word boundary after space
    assert_eq!(find(r"(?<!\ba+)b", "b"), Some((0, 1))); // lookbehind doesn't match

    assert_eq!(
        find(r"(?=fuly)(?<=\b(?:[A-Z][a-z]*|[a-z]+))fuly\b", "Carefuly"),
        Some((4, 8))
    );
}

#[test]
#[cfg(feature = "variable-lookbehinds")]
fn lookbehind_positive_variable_sized_functionality_unicode() {
    assert_eq!(find(r"(?<=\b\w+\b)", "ežeras"), Some((7, 7)));
    assert_eq!(find(r"(?<=\b\w+[ ]\d)", "ežeras 123"), Some((9, 9)));
}

#[test]
fn lookbehind_containing_const_size_backref() {
    assert_eq!(find(r"(..)(?<=\1\1)", "yyxxxx"), Some((4, 6)));
}

#[test]
fn lookahead_looks_left() {
    assert_eq!(find(r"a(?=\b)", "ab"), None);
    assert_eq!(find(r"a(?=\b)", "a."), Some((0, 1)));
    assert_eq!(find(r"a(?=\b|_)", "a."), Some((0, 1)));
    assert_eq!(find(r"a(?=_|\b)", "a."), Some((0, 1)));
}

#[test]
fn negative_lookahead_fail() {
    // This was a tricky one. There's a negative lookahead that contains a
    // "hard" alternative (because of the lookahead). When the VM gets to the
    // point where the body of the negative lookahead matched, it needs to fail
    // the negative lookahead match. That means it needs to pop the stack until
    // before the negative lookahead, and then fail. But how many times it has
    // to pop is not fixed, it depends on the body/VM state.
    assert_eq!(find(r"(?!a(?=b)|x)", "ab"), Some((1, 1)));
    assert_eq!(find(r"(?!`(?:[^`]+(?=`)|x)`)", "`a`"), Some((1, 1)));
}

#[test]
fn backref_for_unmatched_group() {
    assert_eq!(find(r"(a)?\1", "bb"), None);
}

#[test]
fn backref_with_multibyte() {
    assert_eq!(
        find(r"(.+)\1+", "x\u{1F431}\u{1F436}\u{1F431}\u{1F436}"),
        Some((1, 17))
    );
}

#[test]
fn case_insensitive_backref_with_non_ascii() {
    assert_eq!(
        find(r"(?i)(?<word>\w+)\s+\k<word>", "Greek : δ Δ"),
        Some((8, 13))
    );
    assert_eq!(
        find(r"(?i)(?<word>\w+)\s+\k<word>", "foo 🎯 test bar BaR"),
        Some((14, 21))
    );
}

#[test]
fn repeat_non_greedy() {
    // (?=a) to make it fancy and use VM
    assert_eq!(find(r"(a(?=a)){2,}?", "aaa"), Some((0, 2)));
    assert_eq!(find(r"(a(?=a)){2}?a", "aaa"), Some((0, 3)));
}

#[test]
fn empty_repeat_non_greedy() {
    // (?=b) to make it fancy and use VM
    assert_eq!(find(r"(a(?=b)|)+?", "ab"), Some((0, 1)));
    // This tests the "prevent zero-length match on repeat" logic
    assert_eq!(find(r"(a(?=b)|)+?x", "ab"), None);
}

#[test]
fn any_match_unicode_scalar_value() {
    assert_eq!(find(r"(.)\1", "\u{1F60A}\u{1F60A}"), Some((0, 8)));
    assert_eq!(find(r"(?s)(.)\1", "\u{1F60A}\u{1F60A}"), Some((0, 8)));
}

#[test]
fn delegates_match_unicode_scalar_value() {
    assert_eq!(find(r".(?=a)", "\u{1F60A}a"), Some((0, 4)));
    assert_eq!(find(r".(?=\ba+)", "\u{1F60A}a"), Some((0, 4)));
}

#[test]
fn keepout_matches_in_correct_place() {
    assert_eq!(find(r"a\Kb", "aaab"), Some((3, 4)));
    assert_eq!(find(r".+\Kb", "aaab"), Some((3, 4)));
    assert_eq!(find(r"(?:aaa\K)b", "aaab"), Some((3, 4)));
}

#[test]
fn keepout_in_lookarounds_match_in_correct_place() {
    assert_eq!(find(r"(?<=a\Kb)c", "abc"), Some((1, 3)));
    assert_eq!(find(r"(?<!a\Kb)c", "axc"), Some((2, 3)));
    assert_eq!(find(r"a(?=b\Kc)", "abc"), Some((1, 1)));
    assert_eq!(find(r"a(?=b\Kc)..", "abc"), Some((2, 3)));
    assert_eq!(find(r"a(?!b\Kc)", "abx"), Some((0, 1)));
}

#[test]
fn find_no_matches_when_continuing_from_previous_match_end_and_no_match_at_start_of_text() {
    assert_eq!(find(r"\G(\d)\d", " 1122 33"), None);
}

#[test]
fn find_iter() {
    let text = "11 22 33";

    for (i, mat) in common::regex(r"(\d)\d").find_iter(text).enumerate() {
        let mat = mat.unwrap();

        match i {
            0 => assert_eq!((mat.start(), mat.end()), (0, 2)),
            1 => assert_eq!((mat.start(), mat.end()), (3, 5)),
            2 => assert_eq!((mat.start(), mat.end()), (6, 8)),
            i => panic!("Expected 3 captures, got {}", i + 1),
        }
    }
}

#[test]
fn find_iter_overlapping_lookahead() {
    let text = "abcdef";

    for (i, mat) in common::regex(r"[a-z]{2}(?=[a-z])")
        .find_iter(text)
        .enumerate()
    {
        let mat = mat.unwrap();

        match i {
            0 => assert_eq!((mat.start(), mat.end()), (0, 2)),
            1 => assert_eq!((mat.start(), mat.end()), (2, 4)),
            i => panic!("Expected 2 captures, got {}", i + 1),
        }
    }
}

#[test]
fn find_iter_zero_length() {
    let text = "ab1c2";

    for (i, mat) in common::regex(r"\d*(?=[a-z])").find_iter(text).enumerate() {
        let mat = mat.unwrap();

        match i {
            0 => assert_eq!((mat.start(), mat.end()), (0, 0)),
            1 => assert_eq!((mat.start(), mat.end()), (1, 1)),
            2 => assert_eq!((mat.start(), mat.end()), (2, 3)),
            i => panic!("Expected 3 captures, got {}", i + 1),
        }
    }
}

#[test]
fn find_iter_zero_length_longer_codepoint() {
    let text = "é1é";

    for (i, mat) in common::regex(r"\d*(?=é)").find_iter(text).enumerate() {
        let mat = mat.unwrap();

        match i {
            0 => assert_eq!((mat.start(), mat.end()), (0, 0)),
            1 => assert_eq!((mat.start(), mat.end()), (2, 3)),
            i => panic!("Expected 2 captures, got {}", i + 1),
        }
    }
}

#[test]
fn find_iter_continue_from_previous_match_end() {
    let text = "1122 33";

    for (i, mat) in common::regex(r"\G(\d)\d").find_iter(text).enumerate() {
        let mat = mat.unwrap();

        match i {
            0 => assert_eq!((mat.start(), mat.end()), (0, 2)),
            1 => assert_eq!((mat.start(), mat.end()), (2, 4)),
            i => panic!("Expected 2 results, got {}", i + 1),
        }
    }
}

#[test]
fn find_iter_continue_from_previous_match_end_with_zero_width_match() {
    let text = "1122 33";

    for (i, mat) in common::regex(r"\G\d*").find_iter(text).enumerate() {
        let mat = mat.unwrap();

        match i {
            0 => assert_eq!((mat.start(), mat.end()), (0, 4)),
            i => panic!("Expected 1 result, got {}", i + 1),
        }
    }
}

#[test]
fn find_iter_continue_from_previous_match_end_inside_alternation_continue_last() {
    // Test that \G in alternation doesn't cause overzealous early bailout
    // Pattern: abc|\G1
    // The alternation allows matching "abc" at any position,
    // while \G1 only matches "1" at the continuation position
    let text = "1hello1 abc1";

    for (i, mat) in common::regex(r"abc|\G1").find_iter(text).enumerate() {
        let mat = mat.unwrap();

        // Expected matches:
        // 1. "1" at position 0 (via \G1 branch at start)
        // 2. "abc" at position 8 (via abc branch)
        // 3. "1" at position 11 (via \G1 branch continuing from previous match end)
        match i {
            0 => assert_eq!((mat.start(), mat.end()), (0, 1)),
            1 => assert_eq!((mat.start(), mat.end()), (8, 11)),
            2 => assert_eq!((mat.start(), mat.end()), (11, 12)),
            i => panic!("Expected 3 results, got {}", i + 1),
        }
    }
}

#[test]
fn find_iter_continue_from_previous_match_end_inside_alternation_continue_first() {
    // Test with \G branch first in alternation: \G\d+|abc
    // This ensures the optimization doesn't incorrectly bail out
    let text = "123abc456";

    for (i, mat) in common::regex(r"\G\d+|abc").find_iter(text).enumerate() {
        let mat = mat.unwrap();

        // Expected matches:
        // 1. "123" at position 0 (via \G\d+ branch at start)
        // 2. "abc" at position 3 (via abc branch continuing from previous match end)
        // 3. "456" at position 6 (via \G\d+ branch continuing from previous match end)
        match i {
            0 => assert_eq!((mat.start(), mat.end()), (0, 3)),
            1 => assert_eq!((mat.start(), mat.end()), (3, 6)),
            2 => assert_eq!((mat.start(), mat.end()), (6, 9)),
            i => panic!("Expected 3 results, got {}", i + 1),
        }
    }
}

#[test]
fn find_iter_continue_from_previous_match_end_inside_conditional_lookahead() {
    let text = "1a23ba456a";

    for (i, mat) in common::regex(r"(?((?=\G))a|b)").find_iter(text).enumerate() {
        let mat = mat.unwrap();

        // Expected matches:
        // 1. "b" at position 4 (because \G didn't match)
        // 2. "a" at position 5 (because \G matches)
        match i {
            0 => assert_eq!((mat.start(), mat.end()), (4, 5)),
            1 => assert_eq!((mat.start(), mat.end()), (5, 6)),
            i => panic!("Expected 2 results, got {}", i + 1),
        }
    }
}

#[test]
fn find_iter_continue_from_previous_match_end_inside_conditional_lookbehind() {
    let text = "1a23bab456a";

    for (i, mat) in common::regex(r"(?((?!\G))a|b)").find_iter(text).enumerate() {
        let mat = mat.unwrap();

        // Expected matches:
        // 1. "a" at position 1 (because \G didn't match)
        // 2. "a" at position 5 (because \G didn't match)
        // 3. "b" at position 6 (because \G did match)
        // 4. "a" at position 10 (because \G didn't match)
        match i {
            0 => assert_eq!((mat.start(), mat.end()), (1, 2)),
            1 => assert_eq!((mat.start(), mat.end()), (5, 6)),
            2 => assert_eq!((mat.start(), mat.end()), (6, 7)),
            3 => assert_eq!((mat.start(), mat.end()), (10, 11)),
            i => panic!("Expected 4 results, got {}", i + 1),
        }
    }
}

#[test]
fn find_iter_continue_from_previous_match_end_single_match() {
    // Test basic \G behavior with find_iter - should only match at start
    let text = "123 456 789";

    // Should only match at position 0
    for (i, mat) in common::regex(r"\G\d+").find_iter(text).enumerate() {
        let mat = mat.unwrap();

        match i {
            0 => assert_eq!((mat.start(), mat.end()), (0, 3)),
            i => panic!("Expected 1 result, got {}", i + 1),
        }
    }
}

#[test]
fn find_iter_attributes() {
    let text = "ab1c2";
    let regex = common::regex(r"\d*(?=[a-z])");

    let matches = regex.find_iter(text);

    assert_eq!(matches.text(), text);
    assert_eq!(regex.as_str(), matches.regex().as_str());
}

#[test]
fn find_iter_empty_repeat_issue70() {
    fn assert_expected_matches(pattern: &str) {
        let text = "a\naaa\n";
        let regex = common::regex(pattern);

        let matches: Vec<_> = regex.find_iter(text).collect();
        assert_eq!(matches.len(), 4);

        for i in 0..matches.len() {
            let mat = &matches[i].as_ref().unwrap();
            match i {
                0 => assert_eq!((mat.start(), mat.end()), (0, 0)),
                1 => assert_eq!((mat.start(), mat.end()), (2, 2)),
                2 => assert_eq!((mat.start(), mat.end()), (3, 5)),
                3 => assert_eq!((mat.start(), mat.end()), (6, 6)),
                i => panic!("Expected 4 results, got {}", i + 1),
            }
        }
    }

    assert_expected_matches(r"(?m)(?:^|a)+");
    assert_expected_matches(r"(?m)(?:^|a)(?:^|a)*");
    assert_expected_matches(r"(?m)(?>)(?:^|a)+");
    assert_expected_matches(r"(?m)(?>)(?:^|a)(?:^|a)*");
}

#[test]
fn find_iter_empty_repeat_non_greedy_issue70() {
    fn assert_expected_matches(pattern: &str) {
        let text = "a\naaa\n";
        let regex = common::regex(pattern);

        let matches: Vec<_> = regex.find_iter(text).collect();
        assert_eq!(matches.len(), 5);

        for i in 0..matches.len() {
            let mat = &matches[i].as_ref().unwrap();
            match i {
                0 => assert_eq!((mat.start(), mat.end()), (0, 0)),
                1 => assert_eq!((mat.start(), mat.end()), (2, 2)),
                2 => assert_eq!((mat.start(), mat.end()), (3, 4)),
                3 => assert_eq!((mat.start(), mat.end()), (4, 5)),
                4 => assert_eq!((mat.start(), mat.end()), (6, 6)),
                i => panic!("Expected 4 results, got {}", i + 1),
            }
        }
    }

    assert_expected_matches(r"(?m)(?:^|a)+?");
    assert_expected_matches(r"(?m)(?:^|a)(?:^|a)*?");
    assert_expected_matches(r"(?m)(?>)(?:^|a)+?");
    assert_expected_matches(r"(?m)(?>)(?:^|a)(?:^|a)*?");
}

#[test]
fn find_iter_empty_repeat_anchored_non_greedy_issue70() {
    fn assert_expected_matches(pattern: &str) {
        let text = "a\naaa\n";
        let regex = common::regex(pattern);

        let matches: Vec<_> = regex.find_iter(text).collect();
        assert_eq!(matches.len(), 3);

        for i in 0..matches.len() {
            let mat = &matches[i].as_ref().unwrap();
            match i {
                0 => assert_eq!((mat.start(), mat.end()), (0, 1)),
                1 => assert_eq!((mat.start(), mat.end()), (2, 5)),
                2 => assert_eq!((mat.start(), mat.end()), (6, 6)),
                i => panic!("Expected 3 results, got {}", i + 1),
            }
        }
    }

    assert_expected_matches(r"(?m)(?:^|a)+?$");
    assert_expected_matches(r"(?m)(?:^|a)(?:^|a)*?$");
    assert_expected_matches(r"(?m)(?>)(?:^|a)+?$");
    assert_expected_matches(r"(?m)(?>)(?:^|a)(?:^|a)*?$");
}

#[test]
fn find_iter_collect_when_backtrack_limit_hit() {
    use fancy_regex::Error;
    use fancy_regex::RegexBuilder;
    use fancy_regex::RuntimeError;

    let r = RegexBuilder::new("(x+x+)+(?>y)")
        .backtrack_limit(1)
        .build()
        .unwrap();
    let result: Vec<_> = r.find_iter("xxxxxxxxxxy").collect();
    assert_eq!(result.len(), 1);
    assert!(result[0].is_err());
    match &result[0].as_ref().err() {
        Some(Error::RuntimeError(RuntimeError::BacktrackLimitExceeded)) => {}
        _ => panic!("Expected RuntimeError::BacktrackLimitExceeded"),
    }
}

#[test]
fn find_conditional() {
    assert_eq!(find(r"(?(ab)c|d)", "acd"), Some((2, 3)));
    assert_eq!(find(r"(a)?b(?(1)c|d)", "abc"), Some((0, 3)));
    assert_eq!(find(r"(a)?b(?(1)c|d)", "abd"), Some((1, 3)));
}

#[test]
fn find_endtext_before_newlines() {
    assert_eq!(find(r"\Z", "hello\nworld\n\n\n"), Some((11, 11)));
}

#[test]
fn continue_from_previous_match_with_lookahead_before() {
    let text = "123abc";

    // Lookahead doesn't consume, so \G should still be at position 0
    assert_eq!(find(r"(?=\d)\G\d+", text), Some((0, 3)));

    // Lookahead fails, so whole match fails
    assert_eq!(find(r"(?=[a-z])\G\d+", text), None);

    // Negative lookahead passes (not a letter), \G continues
    assert_eq!(find(r"(?![a-z])\G\d+", text), Some((0, 3)));

    // Negative lookahead fails (is a digit), match fails
    assert_eq!(find(r"(?!\d)\G\d+", text), None);
}

#[test]
fn continue_from_previous_match_in_alternation() {
    // Pattern: \G in alternation - tests whether \G behaves correctly in branches
    let text = "123abc";

    // First branch with \G should match
    assert_eq!(find(r"\G\d+|xyz", text), Some((0, 3)));

    // Second branch should also work when first fails
    assert_eq!(find(r"\Gxyz|\d+", text), Some((0, 3)));
    assert_eq!(find(r"\D+|\G123", text), Some((0, 3)));
}

#[test]
fn continue_from_previous_match_fails_when_not_at_continue_position() {
    // \G should fail when we're not at the position where the last match ended
    let text = " 123";

    // Starting match at position 0, but there's a space, so \G at position 0 followed by \d
    // should not match because we'd need to skip the space
    assert_eq!(find(r"\G\d+", text), None);
}

#[test]
fn continue_from_previous_match_at_min_position_zero() {
    let text = "123abc";

    assert_eq!(find(r"^x?\b(\G\d+)", text), Some((0, 3)));
    assert_eq!(find(r"(?:^|\b)x?(\G\d+)", text), Some((0, 3)));
    assert_eq!(find(r"(?:\G\d+|xyz)|abc", text), Some((0, 3)));
    assert_eq!(find(r"(?=\d)(?=1)\G\d+", text), Some((0, 3)));
    // Atomic group doesn't affect \G position checking
    assert_eq!(find(r"(?>\G)\d+", "123abc"), Some((0, 3)));
}

fn find(re: &str, text: &str) -> Option<(usize, usize)> {
    find_match(re, text).map(|m| (m.start(), m.end()))
}

fn find_match<'t>(re: &str, text: &'t str) -> Option<Match<'t>> {
    let regex = common::regex(re);
    let result = regex.find(text);
    assert!(
        result.is_ok(),
        "Expected find to succeed, but was {:?}",
        result
    );
    result.unwrap()
}
