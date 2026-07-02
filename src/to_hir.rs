// Copyright 2026 The Fancy Regex Authors.
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

//! Translation of easy `Expr` subtrees directly into `regex_syntax::hir::Hir`.
//!
//! Delegating a subtree to regex-automata used to mean re-serializing it with
//! [`Expr::to_str`] and letting the engine parse the string all over again —
//! a full AST parse plus Hir translation per delegated engine. Building the
//! `Hir` directly from the `Expr` tree skips that second parse.
//!
//! The translation is defined to produce **exactly** the `Hir` that
//! `regex_syntax` would produce for the `to_str` output (there is an oracle
//! test asserting this equality). Node kinds outside the easy subset that
//! `to_str` handles — and any fragment whose semantics depend on parsing (a
//! `Delegate`'s inner string, a case-insensitive literal) that fails its
//! (fragment-sized) parse — return `None`, and the caller falls back to the
//! string path, preserving both behavior and error reporting.

use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;
use core::convert::TryFrom;

use regex_syntax::hir::{Capture, Dot, Hir, Look, Repetition};

use crate::{push_quoted, Assertion, Expr};

/// Context threaded through a translation: the syntax options the string path
/// would hand to the engine's parser, plus the capture-group counter (groups
/// are numbered in the order their `(` appears, starting at 1, exactly like
/// the parser numbers the `to_str` output).
pub(crate) struct HirCtx {
    unicode: bool,
    utf8: bool,
    next_group: u32,
}

impl HirCtx {
    pub(crate) fn new(unicode: bool, utf8: bool) -> Self {
        HirCtx {
            unicode,
            utf8,
            next_group: 1,
        }
    }
}

/// Translate an easy `Expr` subtree to an `Hir`, or `None` when it isn't
/// covered. `None` is always safe: the caller falls back to the string path.
pub(crate) fn expr_to_hir(expr: &Expr, ctx: &mut HirCtx) -> Option<Hir> {
    Some(match *expr {
        Expr::Empty | Expr::DefineGroup { .. } => Hir::empty(),
        Expr::Any { newline, crlf } => {
            let dot = match (newline, crlf, ctx.unicode, ctx.utf8) {
                (true, _, true, _) => Dot::AnyChar,
                (true, _, false, false) => Dot::AnyByte,
                (false, false, true, _) => Dot::AnyCharExceptLF,
                (false, false, false, false) => Dot::AnyByteExceptLF,
                (false, true, true, _) => Dot::AnyCharExceptCRLF,
                (false, true, false, false) => Dot::AnyByteExceptCRLF,
                // A byte dot under a UTF-8-only haystack is rejected by the
                // parser; let the string path produce that error.
                (_, _, false, true) => return None,
            };
            Hir::dot(dot)
        }
        Expr::Literal { ref val, casei } => {
            if !casei {
                Hir::literal(val.as_bytes())
            } else {
                // Case-insensitive literals expand to case-folded classes with
                // semantics owned by regex-syntax (Unicode simple folding vs
                // ASCII folding, error cases in non-Unicode modes). Parse just
                // this literal instead of replicating those rules.
                let mut cooked = String::with_capacity(val.len() + 5);
                cooked.push_str("(?i:");
                push_quoted(&mut cooked, val);
                cooked.push(')');
                parse_fragment(&cooked, ctx)?
            }
        }
        Expr::Assertion(assertion) => Hir::look(match assertion {
            Assertion::StartText => Look::Start,
            Assertion::EndText => Look::End,
            Assertion::StartLine { crlf: false }
            | Assertion::StartLineOniguruma { crlf: false } => Look::StartLF,
            Assertion::EndLine { crlf: false } => Look::EndLF,
            Assertion::StartLine { crlf: true } | Assertion::StartLineOniguruma { crlf: true } => {
                Look::StartCRLF
            }
            Assertion::EndLine { crlf: true } => Look::EndCRLF,
            // Word boundaries and \Z are hard and never reach a delegated
            // subtree; be conservative if they somehow do.
            _ => return None,
        }),
        Expr::Concat(ref children) => {
            let subs = children
                .iter()
                .map(|child| expr_to_hir(child, ctx))
                .collect::<Option<Vec<_>>>()?;
            Hir::concat(subs)
        }
        Expr::Alt(ref children) => {
            let subs = children
                .iter()
                .map(|child| expr_to_hir(child, ctx))
                .collect::<Option<Vec<_>>>()?;
            Hir::alternation(subs)
        }
        Expr::Group(ref child) => {
            let index = ctx.next_group;
            ctx.next_group += 1;
            let sub = expr_to_hir(child, ctx)?;
            Hir::capture(Capture {
                index,
                name: None,
                sub: Box::new(sub),
            })
        }
        Expr::Repeat {
            ref child,
            lo,
            hi,
            greedy,
        } => {
            let min = u32::try_from(lo).ok()?;
            let max = if hi == usize::MAX {
                None
            } else {
                Some(u32::try_from(hi).ok()?)
            };
            let sub = expr_to_hir(child, ctx)?;
            Hir::repetition(Repetition {
                min,
                max,
                greedy,
                sub: Box::new(sub),
            })
        }
        Expr::Delegate { ref inner, casei } => {
            // Delegate nodes carry raw pattern fragments (character classes
            // and the like); their meaning is defined by the parser.
            if casei {
                let mut cooked = String::with_capacity(inner.len() + 5);
                cooked.push_str("(?i:");
                cooked.push_str(inner);
                cooked.push(')');
                parse_fragment(&cooked, ctx)?
            } else {
                parse_fragment(inner, ctx)?
            }
        }
        _ => return None,
    })
}

fn parse_fragment(fragment: &str, ctx: &HirCtx) -> Option<Hir> {
    regex_syntax::ParserBuilder::new()
        .utf8(ctx.utf8)
        .unicode(ctx.unicode)
        .build()
        .parse(fragment)
        .ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analyze::{analyze, AnalyzeContext};
    use crate::{BytesMode, RegexOptions};
    use alloc::string::ToString;

    /// Patterns covering the easy subset: literals (plain, quoted, casei,
    /// non-ASCII), dots in all newline/CRLF modes, anchors in all line modes,
    /// concat/alt/group nesting, all quantifier shapes, classes and other
    /// delegated fragments, and mixed inline flags.
    const PATTERNS: &[&str] = &[
        "a",
        "abc",
        "a.c",
        ".",
        "(?s).",
        "(?s:.)x",
        "^abc$",
        "(?m)^abc$",
        "(?m:^)foo",
        "(?Rm)^x$",
        "(?i)abc",
        "(?i)aBc(?-i)d",
        "(?i)δΔ",
        "αβγ",
        "a+b*c?",
        "a{2,5}",
        "a{3}",
        "a{2,}?",
        "a+?b??",
        "(a)(b(c))",
        "(?:ab|cd)e",
        "a|b|",
        "(a|)",
        "()",
        "[a-z]+",
        r"\d\w\s",
        "[^a-c]{2,3}",
        r"\p{L}+",
        "(?i)[a-k]x",
        r"\x61\n\t",
        "(a+)(?:b|c)*",
        r"\.\*\+#",
        r"(?i)ſ",
        // Hard patterns must be skipped by the harness (info.hard).
        r"\bword\b",
        r"(a)\1",
    ];

    /// The translator must produce exactly the Hir the parser produces for
    /// the `to_str` serialization — or `None` where the parser errors, so the
    /// string path can report the error.
    fn check_oracle(bytes_mode: BytesMode) {
        for pattern in PATTERNS {
            let options = RegexOptions {
                bytes_mode,
                ..RegexOptions::default()
            };
            let Ok(tree) = crate::Expr::parse_tree_with_flags(pattern, options.compute_flags())
            else {
                continue;
            };
            let Ok(info) = analyze(&tree, AnalyzeContext::default()) else {
                continue;
            };
            if info.hard {
                continue;
            }
            let mut cooked = String::new();
            tree.expr.to_str(&mut cooked, 0);
            let unicode =
                options.syntaxc.get_unicode() && !matches!(options.bytes_mode, BytesMode::Ascii);
            let utf8 = matches!(options.bytes_mode, BytesMode::Unicode);
            let expected = regex_syntax::ParserBuilder::new()
                .utf8(utf8)
                .unicode(unicode)
                .build()
                .parse(&cooked);
            let got = expr_to_hir(&tree.expr, &mut HirCtx::new(unicode, utf8));
            match expected {
                Ok(hir) => assert_eq!(
                    Some(hir),
                    got,
                    "Hir mismatch for {:?} (cooked {:?}) in {:?} mode",
                    pattern,
                    cooked,
                    bytes_mode
                ),
                Err(_) => assert_eq!(
                    None, got,
                    "expected fallback for {:?} (cooked {:?}) in {:?} mode: the parser \
                     rejects it, so the translator must not accept it",
                    pattern, cooked, bytes_mode
                ),
            }
        }
    }

    #[test]
    fn oracle_unicode_mode() {
        check_oracle(BytesMode::Unicode);
    }

    #[test]
    fn oracle_unicode_bytes_mode() {
        check_oracle(BytesMode::UnicodeBytes);
    }

    #[test]
    fn oracle_ascii_mode() {
        check_oracle(BytesMode::Ascii);
    }

    #[test]
    fn group_numbering_matches_textual_order() {
        let tree = crate::Expr::parse_tree("(a)((b)(c))").unwrap();
        let hir = expr_to_hir(&tree.expr, &mut HirCtx::new(true, true)).unwrap();
        let cooked = {
            let mut s = String::new();
            tree.expr.to_str(&mut s, 0);
            s
        };
        let expected = regex_syntax::ParserBuilder::new().build().parse(&cooked);
        assert_eq!(expected.unwrap().to_string(), hir.to_string());
    }
}
