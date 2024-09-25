use fancy_regex::Regex;

#[test]
fn split_left_center_right() {
    let re = Regex::new("1").unwrap();
    let target = "123";
    let fields: Vec<&str> = re.split(target).map(|x| x.unwrap()).collect();
    assert_eq!(fields, vec!["", "23"]);

    let re = Regex::new("2").unwrap();
    let fields: Vec<&str> = re.split(target).map(|x| x.unwrap()).collect();
    assert_eq!(fields, vec!["1", "3"]);

    let re = Regex::new("3").unwrap();
    let fields: Vec<&str> = re.split(target).map(|x| x.unwrap()).collect();
    assert_eq!(fields, vec!["12", ""]);
}

#[test]
fn split_no_matches() {
    let re = Regex::new("4").unwrap();
    let target = "123";
    let fields: Vec<&str> = re.split(target).map(|x| x.unwrap()).collect();
    assert_eq!(fields, vec!["123"]);
}

#[test]
fn split_empty() {
    let re = Regex::new("1").unwrap();
    let target = "";
    let fields: Vec<&str> = re.split(target).map(|x| x.unwrap()).collect();
    assert_eq!(fields, vec![""]);
}

#[test]
fn split_by_own() {
    let re = Regex::new("123").unwrap();
    let target = "123";
    let fields: Vec<&str> = re.split(target).map(|x| x.unwrap()).collect();
    assert_eq!(fields, vec!["", ""]);
}

#[test]
fn split_consecutive_matches() {
    let re = Regex::new("1").unwrap();
    let target = "111";
    let fields: Vec<&str> = re.split(target).map(|x| x.unwrap()).collect();
    assert_eq!(fields, vec!["", "", "", ""]);
}

#[test]
fn split_by_substring() {
    let re = Regex::new("123").unwrap();
    let target = "123456";
    let fields: Vec<&str> = re.split(target).map(|x| x.unwrap()).collect();
    assert_eq!(fields, vec!["", "456"]);

    let re = Regex::new("234|678").unwrap();
    let target = "123456789";
    let fields: Vec<&str> = re.split(target).map(|x| x.unwrap()).collect();
    assert_eq!(fields, vec!["1", "5", "9"]);
}

#[test]
fn split_multiple_different_characters() {
    let re = Regex::new("[1-3]").unwrap();
    let target = "123456";
    let fields: Vec<&str> = re.split(target).map(|x| x.unwrap()).collect();
    assert_eq!(fields, vec!["", "", "", "456"]);
}

#[test]
fn split_mixed_characters() {
    let re = Regex::new("[236]").unwrap();
    let target = "123456";
    let fields: Vec<&str> = re.split(target).map(|x| x.unwrap()).collect();
    assert_eq!(fields, vec!["1", "", "45", ""]);
}

#[test]
fn split_with_backreferences() {
    let re = Regex::new(r"(1|2)\1").unwrap();
    let target = "12112122";
    let fields: Vec<&str> = re.split(target).map(|x| x.unwrap()).collect();
    assert_eq!(fields, vec!["12", "21", ""]);
}

#[test]
fn split_with_look_around() {
    let re = Regex::new(r"(?<=1)2").unwrap();
    let target = "12112122";
    let fields: Vec<&str> = re.split(target).map(|x| x.unwrap()).collect();
    assert_eq!(fields, vec!["1", "11", "1", "2"]);

    let re = Regex::new(r"1(?=2)").unwrap();
    let target = "12112122";
    let fields: Vec<&str> = re.split(target).map(|x| x.unwrap()).collect();
    assert_eq!(fields, vec!["", "21", "2", "22"]);

    let re = Regex::new(r"(?<=2)1(?=2)").unwrap();
    let target = "12112122";
    let fields: Vec<&str> = re.split(target).map(|x| x.unwrap()).collect();
    assert_eq!(fields, vec!["12112", "22"]);
}
