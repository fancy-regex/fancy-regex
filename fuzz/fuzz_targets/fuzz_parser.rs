#![no_main]

#[macro_use]
extern crate libfuzzer_sys;
extern crate fancy_regex;

use fancy_regex::Expr;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let _ = Expr::parse(s);
    }
});
