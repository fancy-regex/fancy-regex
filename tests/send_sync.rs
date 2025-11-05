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
fn test_threading_with_arc() {
    use std::sync::Arc;
    use std::thread;

    // Create a single regex with variable-length lookbehind wrapped in Arc
    let re = Arc::new(Regex::new(r"(?<=ab+)x").unwrap());

    // Create multiple threads that share the same regex instance
    let mut handles = vec![];

    // Test data: different haystacks to match against
    let test_cases = vec![
        ("abbx", true),
        ("abbbx", true),
        ("ax", false),
        ("x", false),
        ("abbbbbbx", true),
    ];

    for (haystack, expected) in test_cases {
        let re_clone = Arc::clone(&re);
        let handle = thread::spawn(move || {
            // Use the same regex instance in each thread
            let result = re_clone.is_match(haystack).unwrap();
            (haystack, result)
        });
        handles.push((handle, expected));
    }

    // Collect results from all threads
    for (handle, expected) in handles {
        let (haystack, result) = handle.join().unwrap();
        assert_eq!(
            result, expected,
            "Failed for haystack '{}': expected {}, got {}",
            haystack, expected, result
        );
    }
}
