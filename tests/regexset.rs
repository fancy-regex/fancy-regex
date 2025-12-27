mod common;

use fancy_regex::{Error, RegexSet, RegexSetBuilder, RuntimeError};

#[test]
fn test_empty_regexset() {
    let set = RegexSet::new::<&[&str], _>(&[]).unwrap();

    assert_eq!(set.len(), 0);
    assert!(set.is_empty());

    let haystack = "hello world";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();
    assert_eq!(matches.len(), 0);
}

#[test]
fn test_priority_resolution_easy_patterns() {
    // When multiple patterns match at the same position, lowest index wins
    let set = RegexSet::new(&[r"hello", r"h\w+", r"\w+"]).unwrap();

    assert_eq!(set.len(), 3);
    assert!(!set.is_empty());

    let haystack = "hello world";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 2);

    // First match should be pattern 0 at position 0 (lowest index)
    assert_eq!(matches[0].pattern(), 0);
    assert_eq!(matches[0].as_str(), "hello");

    assert_eq!(matches[1].pattern(), 2);
    assert_eq!(matches[1].as_str(), "world");
}

#[test]
fn test_hard_patterns_mixed_with_easy_patterns() {
    let set = RegexSet::new(&[r"(\w+)\s+\1", r"\d+"]).unwrap();

    let haystack = "hello hello 123";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 2);
    assert_eq!(matches[0].pattern(), 0); // backreference pattern
    assert_eq!(matches[0].as_str(), "hello hello");
    assert_eq!(matches[1].pattern(), 1); // \d+
    assert_eq!(matches[1].as_str(), "123");
}

#[test]
fn test_priority_resolution_hard_patterns() {
    // When multiple patterns match at the same position, lowest index wins
    let set = RegexSet::new(&[r"(\w+)\s+\1", r"(?=hello)\w+\s+\w+"]).unwrap();

    assert_eq!(set.len(), 2);
    assert!(!set.is_empty());

    let haystack = "hello hello";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 1);

    // First match should be pattern 0 at position 0 (lowest index)
    assert_eq!(matches[0].pattern(), 0);
    assert_eq!(matches[0].start(), 0);
}

#[test]
fn test_zero_width_matches() {
    // Test that zero-width matches don't cause infinite loops
    let set = RegexSet::new(&[r"\b", r"\w+"]).unwrap();

    let haystack = "hello world";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    // Should find word boundaries and words, but not loop infinitely
    assert_eq!(matches.len(), 6);
}

#[test]
fn test_capture_groups() {
    let set = RegexSet::new(&[r"(\d+)-(\d+)", r"([a-z]+)"]).unwrap();

    let haystack = "abc 123-456";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 2);

    // First match should have captures
    assert_eq!(matches[0].pattern(), 1);
    let caps = matches[0].captures();
    assert_eq!(caps.get(0).unwrap().as_str(), "abc");
    assert_eq!(caps.get(1).unwrap().as_str(), "abc");

    // Second match should have captures
    assert_eq!(matches[1].pattern(), 0);
    let caps = matches[1].captures();
    assert_eq!(caps.get(0).unwrap().as_str(), "123-456");
    assert_eq!(caps.get(1).unwrap().as_str(), "123");
    assert_eq!(caps.get(2).unwrap().as_str(), "456");
}

#[test]
fn test_builder_case_insensitive() {
    let set = RegexSetBuilder::new(&[r"hello", r"world"])
        .case_insensitive(true)
        .build()
        .unwrap();

    let haystack = "HELLO WORLD";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 2);
    assert_eq!(matches[0].as_str(), "HELLO");
    assert_eq!(matches[1].as_str(), "WORLD");
}

#[test]
fn test_builder_multi_line() {
    let set = RegexSetBuilder::new(&[r"^hello", r"world$"])
        .multi_line(true)
        .build()
        .unwrap();

    let haystack = "hello\nworld";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 2);
}

#[test]
fn test_no_matches() {
    let set = RegexSet::new(&[r"\d+", r"[A-Z]+"]).unwrap();

    let haystack = "abc";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 0);
}

#[test]
fn test_matches_range() {
    let set = RegexSet::new(&[r"\d+", r"[a-z]+"]).unwrap();

    let haystack = "abc 123 xyz 456";
    // Search only in the middle part
    let matches: Vec<_> = set
        .matches_range(haystack, 4..11)
        .map(|m| m.unwrap())
        .collect();

    // Should only find "123" and "xyz"
    assert_eq!(matches.len(), 2);
    assert_eq!(matches[0].as_str(), "123");
    assert_eq!(matches[1].as_str(), "xyz");
}

#[test]
fn test_zero_width_matches_utf8_boundary() {
    // Test that zero-width matches with multibyte UTF-8 characters don't cause issues
    let set = RegexSet::new(&[r"\d*(?=é)", r"é"]).unwrap();

    let text = "é1é";
    let matches: Vec<_> = set.matches(text).map(|m| m.unwrap()).collect();

    // Should find zero-width matches and the é characters
    // Verify we got matches at expected positions
    let positions: Vec<_> = matches.iter().map(|m| m.start()).collect();
    assert!(positions.contains(&0), "Should have match at position 0");
    assert!(positions.contains(&2), "Should have match at position 2");
    assert!(positions.contains(&3), "Should have match at position 3");
}

#[test]
fn test_backtrack_limit_error_handling() {
    // Test that when a backtrack limit is hit, the iterator stops properly
    let set = RegexSetBuilder::new(&[r"(x+x+)+(?>y)", r"\d+"])
        .backtrack_limit(1)
        .build()
        .unwrap();

    let text = "xxxxxxxxxxy 123";
    let result: Vec<_> = set.matches(text).collect();

    // Should get an error for the first pattern that exceeds the backtrack limit
    assert_eq!(result.len(), 1);
    assert!(result[0].is_err());
    match &result[0].as_ref().err() {
        Some(Error::RuntimeError(RuntimeError::BacktrackLimitExceeded)) => {}
        _ => panic!("Expected RuntimeError::BacktrackLimitExceeded"),
    }
}

#[test]
fn test_zero_width_match_multibyte_char() {
    // Test zero-width matches with emoji (4-byte UTF-8)
    // This test ensures that after a zero-width match at a multibyte character boundary,
    // the iterator correctly advances to the next UTF-8 codepoint boundary
    let set = RegexSet::new(&[r"(?=🎯)", r"[a-z]+"]).unwrap();

    let text = "foo🎯bar";
    let matches: Vec<_> = set.matches(text).map(|m| m.unwrap()).collect();

    // Should find "foo" (ASCII letters), zero-width before emoji, and "bar" (ASCII letters)
    assert_eq!(matches.len(), 3);

    // Verify the matches
    assert_eq!(matches[0].as_str(), "foo");
    assert_eq!(matches[0].pattern(), 1);

    // Zero-width match before emoji (at byte position 3, which is a 4-byte UTF-8 boundary)
    assert_eq!(matches[1].as_str(), "");
    assert_eq!(matches[1].pattern(), 0);
    assert_eq!(matches[1].start(), 3); // Position after "foo"

    // After advancing past the zero-width match and the 4-byte emoji,
    // we should correctly find "bar" at byte position 7
    assert_eq!(matches[2].as_str(), "bar");
    assert_eq!(matches[2].pattern(), 1);
    assert_eq!(matches[2].start(), 7); // Position after "foo" + emoji (3 + 4 bytes)
}

#[test]
fn test_into_captures() {
    let set = RegexSet::new(&[r"(\d+)-(\d+)"]).unwrap();

    let haystack = "123-456";
    let mut matches = set.matches(haystack);
    let m = matches.next().unwrap().unwrap();

    let caps = m.into_captures();
    assert_eq!(caps.get(0).unwrap().as_str(), "123-456");
    assert_eq!(caps.get(1).unwrap().as_str(), "123");
    assert_eq!(caps.get(2).unwrap().as_str(), "456");
}

#[test]
fn test_match_range() {
    let set = RegexSet::new(&[r"\d+"]).unwrap();

    let haystack = "abc 123 xyz";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 1);
    assert_eq!(matches[0].range(), 4..7);
}
