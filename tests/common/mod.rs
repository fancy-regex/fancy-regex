use fancy_regex::Regex;

#[allow(dead_code)]
pub fn regex(re: &str) -> Regex {
    let parse_result = Regex::new(re);
    assert!(
        parse_result.is_ok(),
        "Expected regex '{}' to be compiled successfully, got {:?}",
        re,
        parse_result.err()
    );
    parse_result.unwrap()
}

use std::fmt;
#[allow(dead_code)]
pub struct DebugRegex<'a>(pub &'a Regex);
impl fmt::Display for DebugRegex<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.debug_print(f)
    }
}
