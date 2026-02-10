use fancy_regex::internal::{
    FLAG_CASEI, FLAG_DOTNL, FLAG_IGNORE_SPACE, FLAG_MULTI, FLAG_ONIGURUMA_MODE, FLAG_UNICODE,
};
use fancy_regex::{Expr, LookAround, Regex, RegexBuilder};
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

// Structured analysis tree node for UI rendering
#[derive(Serialize, Deserialize)]
pub struct AnalysisTreeNode {
    pub kind: String,
    pub summary: String,
    pub hard: bool,
    pub min_size: usize,
    pub const_size: bool,
    pub children: Vec<AnalysisTreeNode>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub group: Option<GroupInfo>,
}

#[derive(Serialize, Deserialize)]
pub struct GroupInfo {
    pub index: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
}

// Helper function to escape literal strings for display
fn escape_literal(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '"' => result.push_str("\\\""),
            c if c.is_control() => {
                result.push_str(&format!("\\u{{{:x}}}", c as u32));
            }
            c => result.push(c),
        }
    }
    result
}

/// Build a reverse lookup map from group index to name from the named_groups map.
/// This converts from (name → index) to (index → name) format.
fn build_group_names_lookup(
    named_groups: &std::collections::HashMap<String, usize>,
) -> std::collections::HashMap<usize, String> {
    let mut group_names = std::collections::HashMap::new();
    for (name, &index) in named_groups {
        group_names.insert(index, name.clone());
    }
    group_names
}

// Convert Info to AnalysisTreeNode for UI rendering
// Takes a pre-built reverse lookup map (group index -> name) to avoid rebuilding it on every call
fn info_to_tree_node<'a>(
    info: &fancy_regex::internal::Info<'a>,
    group_names: &std::collections::HashMap<usize, String>,
) -> AnalysisTreeNode {
    let (kind, summary, group_info) = match info.expr {
        Expr::Empty => ("Empty".to_string(), "".to_string(), None),
        Expr::Any { newline } => {
            if *newline {
                ("Any".to_string(), "".to_string(), None)
            } else {
                ("Any".to_string(), "(no newline)".to_string(), None)
            }
        }
        Expr::Assertion(_) => ("Assertion".to_string(), "".to_string(), None),
        Expr::GeneralNewline { .. } => ("GeneralNewline".to_string(), "\\R".to_string(), None),
        Expr::Literal { val, .. } => {
            let escaped = escape_literal(&val);
            ("Literal".to_string(), format!("\"{}\"", escaped), None)
        }
        Expr::Concat(v) => ("Concat".to_string(), format!("({})", v.len()), None),
        Expr::Alt(v) => ("Alt".to_string(), format!("({})", v.len()), None),
        Expr::Group(_) => {
            let group_index = info.start_group();
            let group_name = group_names.get(&group_index).cloned();
            let summary = if let Some(ref name) = group_name {
                format!("{} ({})", group_index, name)
            } else {
                format!("{}", group_index)
            };
            (
                "Group".to_string(),
                summary,
                Some(GroupInfo {
                    index: group_index,
                    name: group_name,
                }),
            )
        }
        Expr::LookAround(_, la) => {
            let (kind_str, polarity) = match la {
                LookAround::LookAhead => ("LookAhead", "positive"),
                LookAround::LookAheadNeg => ("LookAhead", "negative"),
                LookAround::LookBehind => ("LookBehind", "positive"),
                LookAround::LookBehindNeg => ("LookBehind", "negative"),
            };
            (kind_str.to_string(), format!("({})", polarity), None)
        }
        Expr::Repeat { lo, hi, .. } => {
            let hi_str = if *hi == usize::MAX {
                "∞".to_string()
            } else {
                hi.to_string()
            };
            (
                "Repeat".to_string(),
                format!("{{{}..{}}}", lo, hi_str),
                None,
            )
        }
        Expr::Delegate { inner, .. } => {
            let escaped = escape_literal(inner);
            ("Delegate".to_string(), format!("\"{}\"", escaped), None)
        }
        Expr::Backref { group, .. } => {
            let summary = if let Some(name) = group_names.get(&group) {
                format!("({})", name)
            } else {
                format!("{}", group)
            };
            ("Backref".to_string(), summary, None)
        }
        Expr::BackrefWithRelativeRecursionLevel {
            group,
            relative_level,
            ..
        } => {
            let summary = if let Some(name) = group_names.get(&group) {
                format!("({}) level={}", name, relative_level)
            } else {
                format!("{} level={}", group, relative_level)
            };
            ("Backref".to_string(), summary, None)
        }
        Expr::AtomicGroup(_) => ("AtomicGroup".to_string(), "".to_string(), None),
        Expr::KeepOut => ("KeepOut".to_string(), "".to_string(), None),
        Expr::ContinueFromPreviousMatchEnd => (
            "ContinueFromPreviousMatchEnd".to_string(),
            "".to_string(),
            None,
        ),
        Expr::BackrefExistsCondition(group) => (
            "BackrefExistsCondition".to_string(),
            format!("{}", group),
            None,
        ),
        Expr::Conditional { .. } => ("Conditional".to_string(), "".to_string(), None),
        Expr::SubroutineCall(group) => ("SubroutineCall".to_string(), format!("{}", group), None),
        Expr::UnresolvedNamedSubroutineCall { name, .. } => (
            "UnresolvedNamedSubroutineCall".to_string(),
            format!("({})", name),
            None,
        ),
        Expr::BacktrackingControlVerb(_) => {
            ("BacktrackingControlVerb".to_string(), "".to_string(), None)
        }
        Expr::Absent(_) => ("Absent".to_string(), "".to_string(), None),
    };

    let children = info
        .children
        .iter()
        .map(|child| info_to_tree_node(child, group_names))
        .collect();

    AnalysisTreeNode {
        kind,
        summary,
        hard: info.hard,
        min_size: info.min_size,
        const_size: info.const_size,
        children,
        group: group_info,
    }
}

// New WASM export for structured analysis
#[wasm_bindgen]
pub fn analyze_regex_tree(pattern: &str, flags: JsValue) -> Result<JsValue, String> {
    let flags = get_flags(flags)?;
    let regex_flags = compute_regex_flags(&flags);

    use fancy_regex::internal::{analyze, optimize};

    match fancy_regex::Expr::parse_tree_with_flags(pattern, regex_flags) {
        Ok(mut tree) => {
            let named_groups = tree.named_groups.clone();
            // Build reverse lookup map from group index to name once
            let group_names = build_group_names_lookup(&named_groups);
            let requires_capture_group_fixup = optimize(&mut tree);
            match analyze(&tree, requires_capture_group_fixup) {
                Ok(info) => {
                    let tree_node = info_to_tree_node(&info, &group_names);
                    serde_wasm_bindgen::to_value(&tree_node)
                        .map_err(|e| format!("Serialization error: {}", e))
                }
                Err(e) => Err(format!("Analysis error: {}", e)),
            }
        }
        Err(e) => Err(format!("Parse error: {}", e)),
    }
}

// Initialize the module
#[wasm_bindgen(start)]
pub fn main() {
    console_log!("fancy-regex WASM playground initialized");
}

#[cfg(test)]
mod tests {
    use super::*;
    use fancy_regex::Expr;

    #[test]
    fn test_escape_literal_basic() {
        assert_eq!(escape_literal("abc"), "abc");
        assert_eq!(escape_literal("hello world"), "hello world");
    }

    #[test]
    fn test_escape_literal_special_chars() {
        assert_eq!(escape_literal("a\nb"), "a\\nb");
        assert_eq!(escape_literal("a\rb"), "a\\rb");
        assert_eq!(escape_literal("a\tb"), "a\\tb");
        assert_eq!(escape_literal("a\\b"), "a\\b");
        assert_eq!(escape_literal("a\"b"), "a\\\"b");
    }

    #[test]
    fn test_escape_literal_control_chars() {
        let result = escape_literal("\x01\x02");
        assert!(result.contains("\\u{1}"));
        assert!(result.contains("\\u{2}"));
    }

    #[test]
    fn test_info_to_tree_node_literal() {
        // Create a simple literal expression
        let _expr = Expr::Literal {
            val: "test\n".to_string(),
            casei: false,
        };

        // Create mock Info - we need to manually construct this
        // For this test, we'll use analyze to get real Info
        let tree = fancy_regex::Expr::parse_tree("test\\n").unwrap();
        let info = fancy_regex::internal::analyze(&tree, false).unwrap();
        let group_names = std::collections::HashMap::new();

        let node = info_to_tree_node(&info, &group_names);

        assert_eq!(node.kind, "Concat");
        assert_eq!(node.children.len(), 5); // "test\n" as separate literals
        assert_eq!(node.children[0].kind, "Literal");
        assert_eq!(node.children[0].summary, "\"t\"");
    }

    #[test]
    fn test_info_to_tree_node_delegate() {
        // Test that Delegate shows the pattern
        let tree = fancy_regex::Expr::parse_tree(r"\w").unwrap();
        let info = fancy_regex::internal::analyze(&tree, false).unwrap();
        let group_names = std::collections::HashMap::new();

        let node = info_to_tree_node(&info, &group_names);

        // The root should be a Delegate node
        assert_eq!(node.kind, "Delegate");
        assert!(
            node.summary.contains("\\w"),
            "Delegate summary should contain the pattern"
        );
    }

    #[test]
    fn test_info_to_tree_node_group_named() {
        // Test named capture group
        let tree = fancy_regex::Expr::parse_tree(r"(?<word>\w+)").unwrap();
        let info = fancy_regex::internal::analyze(&tree, false).unwrap();
        // Build reverse lookup map from named_groups
        let group_names = build_group_names_lookup(&tree.named_groups);

        let node = info_to_tree_node(&info, &group_names);

        // Find the Group node (should be a child of root)
        assert_eq!(node.kind, "Group");
        assert!(
            node.summary.contains("word"),
            "Group summary should contain the name"
        );
        assert_eq!(node.group.as_ref().unwrap().name, Some("word".to_string()));
    }

    #[test]
    fn test_info_to_tree_node_group_unnamed() {
        // Test unnamed capture group
        let tree = fancy_regex::Expr::parse_tree(r"(\w+)").unwrap();
        let info = fancy_regex::internal::analyze(&tree, false).unwrap();
        let group_names = std::collections::HashMap::new();

        let node = info_to_tree_node(&info, &group_names);

        assert_eq!(node.kind, "Group");
        assert_eq!(node.group.as_ref().unwrap().index, 1);
        assert_eq!(node.group.as_ref().unwrap().name, None);
    }

    #[test]
    fn test_info_to_tree_node_backref_named() {
        // Test named backref
        let tree = fancy_regex::Expr::parse_tree(r"(?<word>\w+)\s+\k<word>").unwrap();
        let info = fancy_regex::internal::analyze(&tree, false).unwrap();
        // Build reverse lookup map from named_groups
        let group_names = build_group_names_lookup(&tree.named_groups);

        let node = info_to_tree_node(&info, &group_names);

        // Navigate to find Backref node (it's in Concat children)
        assert_eq!(node.kind, "Concat");
        let backref_node = node
            .children
            .iter()
            .find(|n| n.kind == "Backref")
            .expect("Should find Backref node");

        assert!(
            backref_node.summary.contains("word"),
            "Backref summary should contain the name"
        );
    }

    #[test]
    fn test_info_to_tree_node_backref_numeric() {
        // Test numeric backref
        let tree = fancy_regex::Expr::parse_tree(r"(\w+)\s+\1").unwrap();
        let info = fancy_regex::internal::analyze(&tree, false).unwrap();
        let group_names = std::collections::HashMap::new();

        let node = info_to_tree_node(&info, &group_names);

        // Navigate to find Backref node
        let backref_node = node
            .children
            .iter()
            .find(|n| n.kind == "Backref")
            .expect("Should find Backref node");

        assert_eq!(backref_node.summary, "1");
    }

    #[test]
    fn test_info_to_tree_node_repeat() {
        // Test repeat ranges
        let test_cases = vec![
            (r"a*", "{0..∞}"),
            (r"a+", "{1..∞}"),
            (r"a?", "{0..1}"),
            (r"a{2,5}", "{2..5}"),
            (r"a{3}", "{3..3}"),
        ];

        for (pattern, expected_summary) in test_cases {
            let tree = fancy_regex::Expr::parse_tree(pattern).unwrap();
            let info = fancy_regex::internal::analyze(&tree, false).unwrap();
            let group_names = std::collections::HashMap::new();

            let node = info_to_tree_node(&info, &group_names);

            assert_eq!(node.kind, "Repeat", "Pattern: {}", pattern);
            assert_eq!(node.summary, expected_summary, "Pattern: {}", pattern);
        }
    }

    #[test]
    fn test_info_to_tree_node_concat_alt() {
        // Test Concat and Alt count
        let tree = fancy_regex::Expr::parse_tree(r"abc").unwrap();
        let info = fancy_regex::internal::analyze(&tree, false).unwrap();
        let group_names = std::collections::HashMap::new();

        let node = info_to_tree_node(&info, &group_names);

        assert_eq!(node.kind, "Concat");
        assert_eq!(node.summary, "(3)");

        // Test Alt
        let tree = fancy_regex::Expr::parse_tree(r"a|b|c").unwrap();
        let info = fancy_regex::internal::analyze(&tree, false).unwrap();
        let group_names = std::collections::HashMap::new();

        let node = info_to_tree_node(&info, &group_names);

        assert_eq!(node.kind, "Alt");
        assert_eq!(node.summary, "(3)");
    }

    #[test]
    fn test_info_to_tree_node_lookaround() {
        let test_cases = vec![
            (r"(?=a)", "LookAhead", "(positive)"),
            (r"(?!a)", "LookAhead", "(negative)"),
            (r"(?<=a)", "LookBehind", "(positive)"),
            (r"(?<!a)", "LookBehind", "(negative)"),
        ];

        for (pattern, expected_kind, expected_summary) in test_cases {
            let tree = fancy_regex::Expr::parse_tree(pattern).unwrap();
            let info = fancy_regex::internal::analyze(&tree, false).unwrap();
            let group_names = std::collections::HashMap::new();

            let node = info_to_tree_node(&info, &group_names);

            assert_eq!(node.kind, expected_kind, "Pattern: {}", pattern);
            assert_eq!(node.summary, expected_summary, "Pattern: {}", pattern);
        }
    }

    #[test]
    fn test_info_to_tree_node_hard_easy() {
        // Test that hard flag is correctly set
        // Backref should be hard
        let tree = fancy_regex::Expr::parse_tree(r"(\w+)\1").unwrap();
        let info = fancy_regex::internal::analyze(&tree, false).unwrap();
        let group_names = std::collections::HashMap::new();

        let node = info_to_tree_node(&info, &group_names);

        assert!(node.hard, "Pattern with backref should be hard");

        // Simple literal should be easy
        let tree = fancy_regex::Expr::parse_tree(r"abc").unwrap();
        let info = fancy_regex::internal::analyze(&tree, false).unwrap();
        let group_names = std::collections::HashMap::new();

        let node = info_to_tree_node(&info, &group_names);

        assert!(!node.hard, "Simple literal pattern should be easy");
    }
}
