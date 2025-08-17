use fancy_regex::{Regex, RegexBuilder};
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

// Expose console.log for debugging
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

// Macro for console.log
macro_rules! console_log {
    ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}

#[derive(Serialize, Deserialize)]
pub struct Match {
    pub start: usize,
    pub end: usize,
    pub text: String,
}

#[derive(Serialize, Deserialize)]
pub struct CaptureGroup {
    pub index: usize,
    pub name: Option<String>,
    pub start: Option<usize>,
    pub end: Option<usize>,
    pub text: Option<String>,
}

#[derive(Serialize, Deserialize)]
pub struct MatchResult {
    pub full_match: Option<Match>,
    pub captures: Vec<CaptureGroup>,
}

#[derive(Serialize, Deserialize)]
pub struct RegexFlags {
    pub case_insensitive: bool,
    pub multi_line: bool,
    pub dot_matches_new_line: bool,
    pub ignore_whitespace: bool,
    pub unicode: bool,
}

impl Default for RegexFlags {
    fn default() -> Self {
        Self {
            case_insensitive: false,
            multi_line: false,
            dot_matches_new_line: false,
            ignore_whitespace: false,
            unicode: true,
        }
    }
}

#[wasm_bindgen]
pub fn create_regex(pattern: &str, flags: JsValue) -> Result<JsValue, JsValue> {
    let flags: RegexFlags = if flags.is_undefined() {
        RegexFlags::default()
    } else {
        serde_wasm_bindgen::from_value(flags).map_err(|e| {
            JsValue::from_str(&format!("Invalid flags: {}", e))
        })?
    };

    let mut builder = RegexBuilder::new(pattern);
    
    if flags.case_insensitive {
        builder.case_insensitive(true);
    }
    if flags.multi_line {
        builder.multi_line(true);
    }
    if flags.dot_matches_new_line {
        builder.dot_matches_new_line(true);
    }
    if flags.ignore_whitespace {
        builder.ignore_whitespace(true);
    }
    
    match builder.build() {
        Ok(_regex) => {
            // Store the pattern for later use since we can't serialize the regex directly
            let regex_info = serde_json::json!({
                "pattern": pattern,
                "flags": flags
            });
            Ok(JsValue::from_str(&regex_info.to_string()))
        }
        Err(e) => Err(JsValue::from_str(&format!("Regex compilation error: {}", e))),
    }
}

#[wasm_bindgen]
pub fn find_matches(pattern: &str, text: &str, flags: JsValue) -> Result<JsValue, JsValue> {
    let flags: RegexFlags = if flags.is_undefined() {
        RegexFlags::default()
    } else {
        serde_wasm_bindgen::from_value(flags).map_err(|e| {
            JsValue::from_str(&format!("Invalid flags: {}", e))
        })?
    };

    let regex = build_regex(pattern, &flags)?;

    let mut matches = Vec::new();
    for mat in regex.find_iter(text) {
        match mat {
            Ok(m) => {
                matches.push(Match {
                    start: m.start(),
                    end: m.end(),
                    text: m.as_str().to_string(),
                });
            }
            Err(e) => return Err(JsValue::from_str(&format!("Match error: {}", e))),
        }
    }

    serde_wasm_bindgen::to_value(&matches).map_err(|e| {
        JsValue::from_str(&format!("Serialization error: {}", e))
    })
}

#[wasm_bindgen]
pub fn find_captures(pattern: &str, text: &str, flags: JsValue) -> Result<JsValue, JsValue> {
    let flags: RegexFlags = if flags.is_undefined() {
        RegexFlags::default()
    } else {
        serde_wasm_bindgen::from_value(flags).map_err(|e| {
            JsValue::from_str(&format!("Invalid flags: {}", e))
        })?
    };

    let regex = build_regex(pattern, &flags)?;

    let mut all_captures = Vec::new();

    for caps_result in regex.captures_iter(text) {
        match caps_result {
            Ok(caps) => {
                let full_match = caps.get(0).map(|m| Match {
                    start: m.start(),
                    end: m.end(),
                    text: m.as_str().to_string(),
                });

                let mut captures = Vec::new();
                for i in 0..caps.len() {
                    let capture = if let Some(m) = caps.get(i) {
                        CaptureGroup {
                            index: i,
                            name: None, // Named groups aren't directly accessible by index
                            start: Some(m.start()),
                            end: Some(m.end()),
                            text: Some(m.as_str().to_string()),
                        }
                    } else {
                        CaptureGroup {
                            index: i,
                            name: None,
                            start: None,
                            end: None,
                            text: None,
                        }
                    };
                    captures.push(capture);
                }

                all_captures.push(MatchResult {
                    full_match,
                    captures,
                });
            }
            Err(e) => return Err(JsValue::from_str(&format!("Capture error: {}", e))),
        }
    }

    serde_wasm_bindgen::to_value(&all_captures).map_err(|e| {
        JsValue::from_str(&format!("Serialization error: {}", e))
    })
}

#[wasm_bindgen]
pub fn parse_regex(pattern: &str, flags: JsValue) -> Result<String, JsValue> {
    let flags: RegexFlags = if flags.is_undefined() {
        RegexFlags::default()
    } else {
        serde_wasm_bindgen::from_value(flags).map_err(|e| {
            JsValue::from_str(&format!("Invalid flags: {}", e))
        })?
    };

    // Build the regex with flags to get proper parse tree representation
    let regex = build_regex(pattern, &flags)?;
    
    match fancy_regex::Expr::parse_tree(pattern) {
        Ok(tree) => Ok(format!("{:#?}", tree)),
        Err(e) => Err(JsValue::from_str(&format!("Parse error: {}", e))),
    }
}

#[wasm_bindgen]
pub fn analyze_regex(pattern: &str, flags: JsValue) -> Result<String, JsValue> {
    let flags: RegexFlags = if flags.is_undefined() {
        RegexFlags::default()
    } else {
        serde_wasm_bindgen::from_value(flags).map_err(|e| {
            JsValue::from_str(&format!("Invalid flags: {}", e))
        })?
    };

    // Build the regex with flags to ensure analysis takes flags into account
    let _regex = build_regex(pattern, &flags)?;
    
    use fancy_regex::internal::{analyze, optimize};
    
    match fancy_regex::Expr::parse_tree(pattern) {
        Ok(mut tree) => {
            optimize(&mut tree);
            match analyze(&tree, 1) {
                Ok(info) => Ok(format!("{:#?}", info)),
                Err(e) => Err(JsValue::from_str(&format!("Analysis error: {}", e))),
            }
        }
        Err(e) => Err(JsValue::from_str(&format!("Parse error: {}", e))),
    }
}

#[wasm_bindgen]
pub fn is_match(pattern: &str, text: &str, flags: JsValue) -> Result<bool, JsValue> {
    let flags: RegexFlags = if flags.is_undefined() {
        RegexFlags::default()
    } else {
        serde_wasm_bindgen::from_value(flags).map_err(|e| {
            JsValue::from_str(&format!("Invalid flags: {}", e))
        })?
    };

    let regex = build_regex(pattern, &flags)?;

    match regex.is_match(text) {
        Ok(result) => Ok(result),
        Err(e) => Err(JsValue::from_str(&format!("Match error: {}", e))),
    }
}

fn build_regex(pattern: &str, flags: &RegexFlags) -> Result<Regex, JsValue> {
    let mut builder = RegexBuilder::new(pattern);
    
    if flags.case_insensitive {
        builder.case_insensitive(true);
    }
    if flags.multi_line {
        builder.multi_line(true);
    }
    if flags.dot_matches_new_line {
        builder.dot_matches_new_line(true);
    }
    if flags.ignore_whitespace {
        builder.ignore_whitespace(true);
    }
    
    builder.build().map_err(|e| {
        JsValue::from_str(&format!("Regex compilation error: {}", e))
    })
}

// Initialize the module
#[wasm_bindgen(start)]
pub fn main() {
    console_log!("fancy-regex WASM playground initialized");
}