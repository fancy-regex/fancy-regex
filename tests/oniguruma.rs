//! Run tests from Oniguruma's test suite, see `oniguruma/README.md`

extern crate fancy_regex;
extern crate regex;

use std::collections::HashSet;
use std::panic;

use regex::Regex;

use fancy_regex::Regex as FancyRegex;

#[derive(Debug, Eq, Hash, PartialEq)]
struct Test {
    source: String,
    pattern: String,
    text: String,
    assertion: Assertion,
}

#[derive(Debug, Eq, Hash, PartialEq)]
enum Assertion {
    Match {
        group: usize,
        start: usize,
        end: usize,
    },
    NoMatch,
}

/// Extract tests from the C source file (or the ignore file).
fn parse_tests(test_source: &str) -> Vec<Test> {
    let mut tests = Vec::new();

    let c_string = r#""((?:\\\\|\\"|[^"])*)""#;
    let re = Regex::new(&format!(
        r"(?m)^\s*(x2|x3|n)\({},\s*{},?([^\)]+)\);",
        c_string, c_string
    ))
    .unwrap();
    for caps in re.captures_iter(test_source) {
        let source = caps.get(0).unwrap().as_str().trim().to_string();
        let kind = caps.get(1).unwrap().as_str();
        let pattern = unescape(caps.get(2).unwrap().as_str());
        let text = unescape(caps.get(3).unwrap().as_str());
        let args: Vec<usize> = caps
            .get(4)
            .unwrap()
            .as_str()
            .split(",")
            .map(|s| s.trim().parse().unwrap())
            .collect();

        let assertion = match kind {
            "x2" => Assertion::Match {
                start: args[0],
                end: args[1],
                group: 0,
            },
            "x3" => Assertion::Match {
                start: args[0],
                end: args[1],
                group: args[2],
            },
            "n" => Assertion::NoMatch,
            _ => {
                panic!("Unexpected test type {}", kind);
            }
        };

        let test = Test {
            source,
            pattern,
            text,
            assertion,
        };

        tests.push(test);
    }
    tests
}

/// Unescape a string as it appears in C source. This is probably not a perfect implementation, but
/// it's good enough for these tests.
fn unescape(escaped: &str) -> String {
    let mut s: Vec<u8> = Vec::new();
    let mut chars = escaped.chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                let next = chars.next().expect("Expected character after backslash");
                match next {
                    '\\' => {
                        s.push(b'\\');
                    }
                    '"' => {
                        s.push(b'"');
                    }
                    '?' => {
                        // '?' has to be escaped in C to avoid trigraphs
                        s.push(b'?');
                    }
                    'n' => {
                        s.push(b'\n');
                    }
                    'r' => {
                        s.push(b'\r');
                    }
                    '0' => {
                        // octal escape, e.g. \001
                        let mut octal = String::new();
                        octal.push(chars.next().expect("Expected character after \\0"));
                        octal.push(chars.next().expect("Expected second character after \\0"));
                        let num =
                            u8::from_str_radix(&octal, 8).expect("Error parsing octal number");
                        s.push(num);
                    }
                    'x' => {
                        // hex escape, e.g. \x1f
                        let mut hex = String::new();
                        hex.push(chars.next().expect("Expected character after \\x"));
                        hex.push(chars.next().expect("Expected second character after \\x"));
                        let num = u8::from_str_radix(&hex, 16).expect("Error parsing hex number");
                        s.push(num);
                    }
                    _ => {
                        unimplemented!("Unknown escaped character {} in {}", next, escaped);
                    }
                }
            }
            _ => {
                s.append(&mut c.to_string().into_bytes());
            }
        }
    }
    // Some strings in the test are invalid UTF-8. We handle them via ignores.
    String::from_utf8_lossy(&s).to_string()
}

fn run_test(test: &Test) -> Option<String> {
    let Test {
        source,
        pattern,
        text,
        assertion,
    } = test;

    let compile_result = FancyRegex::new(&pattern);
    if compile_result.is_err() {
        let mut error = format!("{:?}", compile_result.unwrap_err());
        // The regex crate's error can be multiple lines, format nicely
        error = error.replace("\n", "\n  // ");
        return Some(format!("  // Compile failed: {}\n  {}\n", error, source));
    }

    match *assertion {
        Assertion::Match { group, start, end } => {
            let result = panic::catch_unwind(|| {
                // compile regex again instead of using above, otherwise:
                // "may not be safely transferrable across a catch_unwind boundary"
                let regex = FancyRegex::new(&pattern).unwrap();
                regex.captures(&text).unwrap()
            });

            if let Ok(captures_result) = result {
                if let Some(captures) = captures_result {
                    let m = captures.get(group).expect("Expected group to exist");
                    if m.start() != start || m.end() != end {
                        Some(format!(
                            "  // Match found at start {} and end {} (expected {} and {})\n  {}\n",
                            m.start(),
                            m.end(),
                            start,
                            end,
                            source
                        ))
                    } else {
                        None
                    }
                } else {
                    Some(format!("  // No match found\n  {}\n", source))
                }
            } else {
                Some(format!("  // Panic while matching\n  {}\n", source))
            }
        }
        Assertion::NoMatch => {
            let regex = FancyRegex::new(&pattern).unwrap();
            let result = regex.find(&text).unwrap();
            if result.is_some() {
                Some(format!(" // Match found\n  {}\n", source))
            } else {
                // We expected it not to match and it didn't -> good
                None
            }
        }
    }
}

#[test]
fn oniguruma() {
    let tests = parse_tests(include_str!("oniguruma/test_utf8.c"));
    let ignore: HashSet<_> = parse_tests(include_str!("oniguruma/test_utf8_ignore.c"))
        .into_iter()
        .collect();

    let mut ignored = 0;
    let mut success = 0;

    for test in tests {
        if ignore.contains(&test) {
            ignored += 1;
            continue;
        }

        let result = run_test(&test);
        if let Some(failure) = result {
            // This is a weird way to do the assertions, but the nice thing about it is that we can
            // run the tests without an "ignore" file and instead of failing, print the contents for
            // the ignore file. To do that, disable the assert and enable the print:

            // println!("{}", failure);
            assert!(false, "{}", failure);
        } else {
            // println!("Success: {}", test.source);
            success += 1;
        }
    }

    println!(
        "{} successful Oniguruma tests, {} ignored",
        success, ignored
    );
}
