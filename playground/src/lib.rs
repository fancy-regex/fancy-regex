use fancy_regex::internal::{FLAG_CASEI, FLAG_DOTNL, FLAG_IGNORE_SPACE, FLAG_MULTI, FLAG_ONIGURUMA_MODE, FLAG_UNICODE};
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

#[derive(Serialize, Deserialize, Debug)]
pub struct RegexFlags {
    pub case_insensitive: bool,
    pub multi_line: bool,
    pub dot_matches_new_line: bool,
    pub ignore_whitespace: bool,
    pub unicode: bool,
    pub oniguruma_mode: bool,
}

impl Default for RegexFlags {
    fn default() -> Self {
        Self {
            case_insensitive: false,
            multi_line: false,
            dot_matches_new_line: false,
            ignore_whitespace: false,
            unicode: true,
            oniguruma_mode: true,
        }
    }
}

// Helper function to deserialize flags or use default
fn get_flags(flags: JsValue) -> Result<RegexFlags, String> {
    if flags.is_undefined() {
        Ok(RegexFlags::default())
    } else {
        serde_wasm_bindgen::from_value(flags).map_err(|e| format!("Invalid flags: {}", e))
    }
}

// Helper function to build regex with flags
fn build_regex(pattern: &str, flags: &RegexFlags) -> Result<Regex, String> {
    let mut builder = RegexBuilder::new(pattern);

    builder.case_insensitive(flags.case_insensitive);
    builder.multi_line(flags.multi_line);
    builder.dot_matches_new_line(flags.dot_matches_new_line);
    builder.ignore_whitespace(flags.ignore_whitespace);
    builder.unicode_mode(flags.unicode);
    builder.oniguruma_mode(flags.oniguruma_mode);

    builder
        .build()
        .map_err(|e| format!("Regex compilation error: {}", e))
}

// Helper function to compute regex flags for parse_tree_with_flags
fn compute_regex_flags(flags: &RegexFlags) -> u32 {
    let mut result = 0;
    if flags.case_insensitive {
        result |= FLAG_CASEI;
    }
    if flags.multi_line {
        result |= FLAG_MULTI;
    }
    if flags.dot_matches_new_line {
        result |= FLAG_DOTNL;
    }
    if flags.ignore_whitespace {
        result |= FLAG_IGNORE_SPACE;
    }
    if flags.unicode {
        result |= FLAG_UNICODE;
    }
    if flags.oniguruma_mode {
        result |= FLAG_ONIGURUMA_MODE;
    }
    result
}

#[wasm_bindgen]
pub fn find_captures(pattern: &str, text: &str, flags: JsValue) -> Result<JsValue, String> {
    let flags = get_flags(flags)?;
    let regex = build_regex(pattern, &flags)?;

    // Get capture group names once for this regex
    let capture_names: Vec<Option<&str>> = regex.capture_names().collect();

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
                    let name = capture_names
                        .get(i)
                        .and_then(|&name_opt| name_opt.map(|s| s.to_string()));
                    let capture = if let Some(m) = caps.get(i) {
                        CaptureGroup {
                            index: i,
                            name,
                            start: Some(m.start()),
                            end: Some(m.end()),
                            text: Some(m.as_str().to_string()),
                        }
                    } else {
                        CaptureGroup {
                            index: i,
                            name,
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
            Err(e) => return Err(format!("Capture error: {}", e)),
        }
    }

    serde_wasm_bindgen::to_value(&all_captures).map_err(|e| format!("Serialization error: {}", e))
}

#[wasm_bindgen]
pub fn parse_regex(pattern: &str, flags: JsValue) -> Result<String, String> {
    let flags = get_flags(flags)?;
    let regex_flags = compute_regex_flags(&flags);

    match fancy_regex::Expr::parse_tree_with_flags(pattern, regex_flags) {
        Ok(tree) => Ok(format!("{:#?}", tree)),
        Err(e) => Err(format!("Parse error: {}", e)),
    }
}

#[wasm_bindgen]
pub fn analyze_regex(pattern: &str, flags: JsValue) -> Result<String, String> {
    let flags = get_flags(flags)?;
    let regex_flags = compute_regex_flags(&flags);

    use fancy_regex::internal::{analyze, optimize};

    match fancy_regex::Expr::parse_tree_with_flags(pattern, regex_flags) {
        Ok(mut tree) => {
            let requires_capture_group_fixup = optimize(&mut tree);
            match analyze(&tree, requires_capture_group_fixup) {
                Ok(info) => Ok(format!("{:#?}", info)),
                Err(e) => Err(format!("Analysis error: {}", e)),
            }
        }
        Err(e) => Err(format!("Parse error: {}", e)),
    }
}

#[wasm_bindgen]
pub fn is_match(pattern: &str, text: &str, flags: JsValue) -> Result<bool, String> {
    let flags = get_flags(flags)?;
    let regex = build_regex(pattern, &flags)?;

    match regex.is_match(text) {
        Ok(result) => Ok(result),
        Err(e) => Err(format!("Match error: {}", e)),
    }
}

// Initialize the module
#[wasm_bindgen(start)]
pub fn main() {
    console_log!("fancy-regex WASM playground initialized");
}
