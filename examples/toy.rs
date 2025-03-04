// Copyright 2016 The Fancy Regex Authors.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

//! A simple test app for exercising and debugging the regex engine.

use fancy_regex::internal::{analyze, compile, run_trace, Insn, Prog};
use fancy_regex::*;
use std::env;
use std::io;
use std::str::FromStr;

fn main() {
    let mut args = env::args().skip(1);
    if let Some(cmd) = args.next() {
        if cmd == "parse" {
            let re = args.next().expect("expected regexp argument");
            let e = Expr::parse_tree(&re);
            println!("{:#?}", e);
        } else if cmd == "analyze" {
            let re = args.next().expect("expected regexp argument");
            let tree = Expr::parse_tree(&re).unwrap();
            let a = analyze(&tree);
            println!("{:#?}", a);
        } else if cmd == "compile" {
            let re = args.next().expect("expected regexp argument");
            show_compiled_program(&re, &mut io::stdout()).expect("error compiling program");
        } else if cmd == "run" {
            let re = args.next().expect("expected regexp argument");
            let r = Regex::new(&re).unwrap();
            let text = args.next().expect("expected text argument");
            let mut pos = 0;
            if let Some(pos_str) = args.next() {
                pos = usize::from_str(&pos_str).unwrap();
            }
            if let Some(caps) = r.captures_from_pos(&text, pos).unwrap() {
                print!("captures:");
                for i in 0..caps.len() {
                    print!(" {}:", i);
                    if let Some(m) = caps.get(i) {
                        print!("[{}..{}] \"{}\"", m.start(), m.end(), m.as_str());
                    } else {
                        print!("_");
                    }
                }
                println!("");
                for cap in caps.iter() {
                    println!("iterate {:?}", cap);
                }
            } else {
                println!("no match");
            }
        } else if cmd == "trace" {
            let re = args.next().expect("expected regexp argument");
            let prog = prog(&re);
            let text = args.next().expect("expected text argument");
            run_trace(&prog, &text, 0).unwrap();
        } else if cmd == "trace-inner" {
            let re = args.next().expect("expected regexp argument");
            let tree = Expr::parse_tree(&re).unwrap();
            let text = args.next().expect("expected text argument");
            let a = analyze(&tree).unwrap();
            let p = compile(&a).unwrap();
            run_trace(&p, &text, 0).unwrap();
        } else if cmd == "graph" {
            let re = args.next().expect("expected regexp argument");
            graph(&re, &mut io::stdout()).expect("error making graph");
        } else {
            println!("commands: parse|analyze|compile|graph <expr>, run|trace|trace-inner <expr> <input>");
        }
    }
}

fn graph(re: &str, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
    let prog = prog(re);
    write!(writer, "digraph G {{\n")?;
    for (i, insn) in prog.body.iter().enumerate() {
        let label = format!("{:?}", insn)
            .replace(r#"\"#, r#"\\"#)
            .replace(r#"""#, r#"\""#);
        write!(writer, r#"{:3} [label="{}: {}"];{}"#, i, i, label, "\n")?;
        match *insn {
            Insn::Split(a, b) => {
                write!(writer, "{:3} -> {};\n", i, a)?;
                write!(writer, "{:3} -> {};\n", i, b)?;
            }
            Insn::Jmp(target) => {
                write!(writer, "{:3} -> {};\n", i, target)?;
            }
            Insn::End => {}
            _ => {
                write!(writer, "{:3} -> {};\n", i, i + 1)?;
            }
        }
    }
    write!(writer, "}}\n")?;
    Ok(())
}

fn show_compiled_program(re: &str, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
    let r = Regex::new(&re).unwrap();
    r.debug_print(writer)?;
    Ok(())
}

fn prog(re: &str) -> Prog {
    let tree = Expr::parse_tree(re).expect("Expected parsing regex to work");
    let result = analyze(&tree).expect("Expected analyze to succeed");
    compile(&result).expect("Expected compile to succeed")
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_graph() {
        assert_graph(
            "a+bc?",
            "\
digraph G {
  0 [label=\"0: Delegate { pattern: \\\"a+bc?\\\", start_group: 0, end_group: 0 }\"];
  0 -> 1;
  1 [label=\"1: End\"];
}
",
        );

        assert_graph(
            "a+(?<b>b*)(?=c)\\k<b>",
            "\
digraph G {
  0 [label=\"0: Delegate { pattern: \\\"a+bc?\\\", start_group: 0, end_group: 0 }\"];
  0 -> 1;
  1 [label=\"1: End\"];
}
",
        );
    }

    fn assert_graph(re: &str, expected: &str) {
        use crate::graph;
        let mut buf = Vec::new();
        graph(re, &mut buf).expect("error making graph");
        let output = String::from_utf8(buf).expect("error converting graph to string");
        assert_eq!(&output, &expected);
    }

    #[test]
    fn test_compilation_debug_output() {
        let expected = "  ".to_owned()
            + "\
  0: Split(3, 1)
  1: Any
  2: Jmp(0)
  3: Save(0)
  4: Lit(\"a\")
  5: Split(4, 6)
  6: Save(2)
  7: Split(8, 10)
  8: Lit(\"b\")
  9: Jmp(7)
 10: Save(3)
 11: Save(4)
 12: Lit(\"c\")
 13: Restore(4)
 14: Backref(2)
 15: Save(1)
 16: End
";

        assert_compiled_prog("a+(?<b>b*)(?=c)\\k<b>", &expected);
    }

    fn assert_compiled_prog(re: &str, expected: &str) {
        use crate::show_compiled_program;
        let mut buf = Vec::new();
        show_compiled_program(re, &mut buf).expect("error compiling program");
        let output = String::from_utf8(buf).expect("error converting program to string");
        assert_eq!(&output, &expected);
    }
}
