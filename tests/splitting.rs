use fancy_regex::Regex;

fn split_to_vec<'a>(re_str: &'a str, target: &'a str) -> Vec<&'a str> {
    let re = Regex::new(re_str).unwrap();
    re.split(target).map(|x| x.unwrap()).collect()
}

#[test]
fn split_left() {
    let result: Vec<&str> = split_to_vec("1", "123");
    assert_eq!(result, vec!["", "23"]);
}

#[test]
fn split_center() {
    let result: Vec<&str> = split_to_vec("2", "123");
    assert_eq!(result, vec!["1", "3"]);
}

#[test]
fn split_right() {
    let result: Vec<&str> = split_to_vec("3", "123");
    assert_eq!(result, vec!["12", ""]);
}

#[test]
fn split_no_matches() {
    let result: Vec<&str> = split_to_vec("4", "123");
    assert_eq!(result, vec!["123"]);
}

#[test]
fn split_empty() {
    let result: Vec<&str> = split_to_vec("1", "");
    assert_eq!(result, vec![""]);
}

#[test]
fn split_by_empty() {
    let result: Vec<&str> = split_to_vec("", "123");
    assert_eq!(result, vec!["", "1", "2", "3", ""]);
}

#[test]
fn split_by_own() {
    let result: Vec<&str> = split_to_vec("123", "123");
    assert_eq!(result, vec!["", ""]);
}

#[test]
fn split_consecutive_matches() {
    let result: Vec<&str> = split_to_vec("1", "111");
    assert_eq!(result, vec!["", "", "", ""]);
}

#[test]
fn split_by_substring() {
    let result: Vec<&str> = split_to_vec("123", "123456");
    assert_eq!(result, vec!["", "456"]);

    let result: Vec<&str> = split_to_vec("234|678", "123456789");
    assert_eq!(result, vec!["1", "5", "9"]);
}

#[test]
fn split_multiple_different_characters() {
    let result: Vec<&str> = split_to_vec("[1-3]", "123456");
    assert_eq!(result, vec!["", "", "", "456"]);
}

#[test]
fn split_mixed_characters() {
    let result: Vec<&str> = split_to_vec("[236]", "123456");
    assert_eq!(result, vec!["1", "", "45", ""]);
}

#[test]
fn split_with_backreferences() {
    let result: Vec<&str> = split_to_vec(r"(1|2)\1", "12112122");
    assert_eq!(result, vec!["12", "21", ""]);
}

#[test]
fn split_with_look_around() {
    let result: Vec<&str> = split_to_vec(r"(?<=1)2", "12112122");
    assert_eq!(result, vec!["1", "11", "1", "2"]);

    let result: Vec<&str> = split_to_vec(r"1(?=2)", "12112122");
    assert_eq!(result, vec!["", "21", "2", "22"]);

    let result: Vec<&str> = split_to_vec(r"(?<=2)1(?=2)", "12112122");
    assert_eq!(result, vec!["12112", "22"]);
}
