#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &str| {
    let _ = fancy_regex::Expr::parse_tree(data);
});
