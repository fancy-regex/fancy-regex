use fancy_regex::{Matches, Regex};

// Helper functions to ensure types implement Send and Sync
fn assert_send<T: Send>() {}
fn assert_sync<T: Sync>() {}

#[test]
fn test_regex_is_send() {
    assert_send::<Regex>();
}

#[test]
fn test_regex_is_sync() {
    assert_sync::<Regex>();
}

#[test]
fn test_matches_is_send() {
    assert_send::<Matches>();
}

#[cfg(feature = "std")]
#[test]
fn test_threading() {
    use std::thread;

    // Create a regex with variable-length lookbehind
    let re = Regex::new(r"(?<=ab+)x").unwrap();

    // Clone it to move into thread
    let re_clone = re.clone();

    // Spawn a thread - this requires Send
    let handle = thread::spawn(move || {
        // Use the regex in the thread
        re_clone.is_match("abbbx").unwrap()
    });

    // Use the original regex in the main thread - this requires Sync
    let result1 = re.is_match("abbx").unwrap();
    let result2 = handle.join().unwrap();

    assert!(result1);
    assert!(result2);
}
