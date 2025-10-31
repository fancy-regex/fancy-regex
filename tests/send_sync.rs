use fancy_regex::{Matches, Regex};

// Helper functions to ensure types implement Send and Sync
fn assert_send<T: Send>() {}
fn assert_sync<T: Sync>() {}

#[test]
#[cfg(feature = "std")]
fn test_regex_is_send() {
    assert_send::<Regex>();
}

#[test]
#[cfg(feature = "std")]
fn test_regex_is_sync() {
    assert_sync::<Regex>();
}

#[test]
#[cfg(feature = "std")]
fn test_matches_is_send() {
    assert_send::<Matches>();
}

#[test]
#[cfg(all(feature = "variable-lookbehinds", feature = "std"))]
fn test_variable_lookbehind_regex_is_send_sync() {
    // Create a regex with variable-length lookbehind to ensure
    // ReverseBackwardsDelegate is Send/Sync when std is available
    let re = Regex::new(r"(?<=ab+)x").unwrap();
    assert_send::<Regex>();
    assert_sync::<Regex>();

    // Verify it actually works
    assert!(re.is_match("abx").unwrap());
    assert!(re.is_match("abbx").unwrap());
    assert!(!re.is_match("ax").unwrap());
}
