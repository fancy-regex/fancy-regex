use fancy_regex::RegexBuilder;

#[test]
fn check_casing_option() {
    let builder = RegexBuilder::new(r"TEST foo")
        .case_insensitive(false)
        .build();

    match builder {
        Ok(regex) => {
            assert!(regex.is_match(r"TEST foo").unwrap_or_default());
            assert!(!regex.is_match(r"test foo").unwrap_or_default());
        }
        _ => panic!("builder should be able to compile with casing options"),
    }
}

#[test]
fn check_override_casing_option() {
    let builder = RegexBuilder::new(r"FOO(?i:bar)quux")
        .case_insensitive(false)
        .build();

    match builder {
        Ok(regex) => {
            assert!(!regex.is_match("FoObarQuUx").unwrap_or_default());
            assert!(!regex.is_match("fooBARquux").unwrap_or_default());
            assert!(regex.is_match("FOObarquux").unwrap_or_default());
        }
        _ => panic!("builder should be able to compile with casing options"),
    }
}

#[test]
fn check_casing_insensitive_option() {
    let builder = RegexBuilder::new(r"TEST FOO")
        .case_insensitive(true)
        .build();

    match builder {
        Ok(regex) => assert!(regex.is_match(r"test foo").unwrap_or_default()),
        _ => panic!("builder should be able to compile with casing options"),
    }
}

#[test]
fn check_multi_line_option() {
    let builder = RegexBuilder::new(r"^test$").multi_line(true).build();

    let test_text = r"test
hugo
test";

    match builder {
        Ok(regex) => assert!(regex.is_match(test_text).unwrap_or_default()),
        _ => panic!("builder should be able to compile with multiline option"),
    }
}

#[test]
fn check_ignore_whitespace_option() {
    let builder = RegexBuilder::new(r"test    foo")
        .ignore_whitespace(true)
        .build();

    let test_text = r"testfoo";
    match builder {
        Ok(regex) => assert!(regex.is_match(test_text).unwrap_or_default()),
        _ => panic!("builder should be able to compile with ignore whitespace option"),
    }
}

#[test]
fn check_dot_matches_new_line_option() {
    let builder = RegexBuilder::new(r"<div>(.*?)<\/div>")
        .dot_matches_new_line(true)
        .build();

    let test_text = r"<div>
    hello</div>";

    match builder {
        Ok(regex) => assert!(regex.is_match(test_text).unwrap_or_default()),
        _ => panic!("builder should be able to compile with dot matches new line option"),
    }
}

#[test]
fn check_casing_insensitive_option_hard() {
    let builder = RegexBuilder::new(r"[a-z](?<=[^f])")
        .case_insensitive(true)
        .build();

    match builder {
        Ok(regex) => {
            assert!(regex.is_match(r"J").unwrap_or_default());
            assert!(!regex.is_match(r"F").unwrap_or_default());
            assert!(regex.is_match(r"j").unwrap_or_default());
        }
        _ => panic!("builder should be able to compile with casing options"),
    }
}

#[test]
fn check_ignore_whitespace_option_fancy() {
    let builder = RegexBuilder::new(r"(?=test    foo)")
        .ignore_whitespace(true)
        .build();

    let test_text = r"testfoo";

    let reggie = builder.unwrap();
    reggie.is_match(test_text);
    let x = reggie.is_match(test_text).unwrap_or_default();

    println!("done {}", x)

    /*

    match builder {
        Ok(regex) => assert!(regex.is_match(test_text).unwrap_or_default()),
        _ => panic!("builder should be able to compile with ignore whitespace option"),
    }
    */
}

#[test]
fn issue_163_fancy_email_test() {
    let regex = fancy_regex::RegexBuilder::new(
        r"^(?!\.)(?!.*\.\.)([a-z0-9_'+\-\.]*)[a-z0-9_'+\-]@([a-z0-9][a-z0-9\-]*\.)+[a-z]{2,}$",
    )
    .case_insensitive(true)
    .build()
    .unwrap();

    let test_email = "VALID@domain.com";
    let is_valid = regex.is_match(test_email).unwrap();
    assert!(is_valid);
}
