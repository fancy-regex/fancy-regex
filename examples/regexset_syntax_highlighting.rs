// Example demonstrating RegexSet usage for syntax highlighting

use fancy_regex::{RegexSetBuilder, Result};

fn main() -> Result<()> {
    // Create a regex set for basic syntax highlighting
    let set = RegexSetBuilder::new(&[
        r"//.*$",                    // 0: Single-line comments
        r#""(?:[^"\\]|\\.)*""#,      // 1: String literals
        r"\b(fn|let|mut|if|else)\b", // 2: Keywords
        r"\b[0-9]+\b",               // 3: Numbers
        r"[a-zA-Z_][a-zA-Z0-9_]*",   // 4: Identifiers
    ])
    .multi_line(true)
    .build()?;

    let code = r#"let x = 42; // a comment
let s = "hello world";"#;

    println!("Tokenizing code:\n{}\n", code);
    println!("Tokens found:");

    for result in set.matches(code) {
        let m = result?;
        let token_type = match m.pattern() {
            0 => "comment",
            1 => "string",
            2 => "keyword",
            3 => "number",
            4 => "identifier",
            _ => "unknown",
        };

        println!(
            "  [{}] {} at {}..{}: '{}'",
            m.pattern(),
            token_type,
            m.start(),
            m.end(),
            m.as_str()
        );
    }

    Ok(())
}
