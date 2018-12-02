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

//! Analysis of regex expressions.

use bit_set::BitSet;
use std::cmp::min;
use std::usize;

use Error;
use Expr;
use Result;

#[derive(Debug)]
pub struct Info<'a> {
    pub start_group: usize,
    pub end_group: usize,
    pub min_size: usize,
    pub const_size: bool,
    pub hard: bool,

    /// Whether the expression's matching could be dependent on what the
    /// previous character was. E.g. `^` matches if there's no previous
    /// character; `(?m:^)` matches if the previous character was a newline.
    pub looks_left: bool,

    pub expr: &'a Expr,
    pub children: Vec<Info<'a>>,
}

impl<'a> Info<'a> {
    pub fn is_literal(&self) -> bool {
        match *self.expr {
            Expr::Literal { casei, .. } => !casei,
            Expr::Concat(_) => self.children.iter().all(|child| child.is_literal()),
            _ => false,
        }
    }

    pub fn push_literal(&self, buf: &mut String) {
        match *self.expr {
            // could be more paranoid about checking casei
            Expr::Literal { ref val, .. } => buf.push_str(val),
            Expr::Concat(_) => {
                for child in &self.children {
                    child.push_literal(buf);
                }
            }
            _ => panic!("push_literal called on non-literal"),
        }
    }
}

struct Analyzer<'a> {
    backrefs: &'a BitSet,
    group_ix: usize,
}

impl<'a> Analyzer<'a> {
    fn visit(&mut self, expr: &'a Expr) -> Result<Info<'a>> {
        let start_group = self.group_ix;
        let mut children = Vec::new();
        let mut min_size = 0;
        let mut const_size = false;
        let mut hard = false;
        let mut looks_left = false;
        match *expr {
            Expr::Empty | Expr::EndText | Expr::EndLine => {
                const_size = true;
            }
            Expr::Any { .. } => {
                min_size = 1;
                const_size = true;
            }
            Expr::Literal { ref val, casei } => {
                // right now each character in a literal gets its own node, that might change
                min_size = 1;
                const_size = literal_const_size(val, casei);
            }
            Expr::StartText | Expr::StartLine => {
                const_size = true;
                looks_left = true;
            }
            Expr::Concat(ref v) => {
                const_size = true;
                for child in v {
                    let child_info = self.visit(child)?;
                    looks_left |= child_info.looks_left && min_size == 0;
                    min_size += child_info.min_size;
                    const_size &= child_info.const_size;
                    hard |= child_info.hard;
                    children.push(child_info);
                }
            }
            Expr::Alt(ref v) => {
                let child_info = self.visit(&v[0])?;
                min_size = child_info.min_size;
                const_size = child_info.const_size;
                hard = child_info.hard;
                looks_left = child_info.looks_left;
                children.push(child_info);
                for child in &v[1..] {
                    let child_info = self.visit(child)?;
                    const_size &= child_info.const_size && min_size == child_info.min_size;
                    min_size = min(min_size, child_info.min_size);
                    hard |= child_info.hard;
                    looks_left |= child_info.looks_left;
                    children.push(child_info);
                }
            }
            Expr::Group(ref child) => {
                let group = self.group_ix;
                self.group_ix += 1;
                let child_info = self.visit(child)?;
                min_size = child_info.min_size;
                const_size = child_info.const_size;
                looks_left = child_info.looks_left;
                hard = child_info.hard | self.backrefs.contains(group);
                children.push(child_info);
            }
            Expr::LookAround(ref child, _) => {
                let child_info = self.visit(child)?;
                // min_size = 0
                const_size = true;
                hard = true;
                looks_left = child_info.looks_left;
                children.push(child_info);
            }
            Expr::Repeat {
                ref child, lo, hi, ..
            } => {
                let child_info = self.visit(child)?;
                min_size = child_info.min_size * lo;
                const_size = child_info.const_size && lo == hi;
                hard = child_info.hard;
                looks_left = child_info.looks_left;
                children.push(child_info);
            }
            Expr::Delegate { size, .. } => {
                // currently only used for empty and single-char matches
                min_size = size;
                const_size = true;
                looks_left = size == 0; // TODO: conservative for \z
            }
            Expr::Backref(group) => {
                if group >= self.group_ix {
                    return Err(Error::InvalidBackref);
                }
                hard = true;
            }
            Expr::AtomicGroup(ref child) => {
                let child_info = self.visit(child)?;
                min_size = child_info.min_size;
                const_size = child_info.const_size;
                looks_left = child_info.looks_left;
                hard = true; // TODO: possibly could weaken
                children.push(child_info);
            }
        };

        Ok(Info {
            expr,
            children,
            start_group,
            end_group: self.group_ix,
            min_size,
            const_size,
            hard,
            looks_left,
        })
    }
}

fn literal_const_size(_: &str, _: bool) -> bool {
    // Right now, regex doesn't do sophisticated case folding,
    // test below will fail when that changes, then we need to
    // do something fancier here.
    true
}

pub fn analyze<'a>(expr: &'a Expr, backrefs: &'a BitSet) -> Result<Info<'a>> {
    let mut analyzer = Analyzer {
        backrefs: backrefs,
        group_ix: 0,
    };

    analyzer.visit(expr)
}

#[cfg(test)]
mod tests {
    use super::analyze;
    use super::literal_const_size;
    use regex;
    use Expr;

    #[test]
    fn case_folding_safe() {
        let re = regex::Regex::new("(?i:ß)").unwrap();
        if re.is_match("SS") {
            assert!(!literal_const_size("ß", true));
        }

        // Another tricky example, Armenian ECH YIWN
        let re = regex::Regex::new("(?i:\\x{0587})").unwrap();
        if re.is_match("\u{0565}\u{0582}") {
            assert!(!literal_const_size("\u{0587}", true));
        }
    }

    #[test]
    fn invalid_backref_1() {
        let (e, backrefs) = Expr::parse(".\\0").unwrap();
        assert!(analyze(&e, &backrefs).is_err());
    }

    #[test]
    fn invalid_backref_2() {
        let (e, backrefs) = Expr::parse("(.\\1)").unwrap();
        assert!(analyze(&e, &backrefs).is_err());
    }

    #[test]
    fn invalid_backref_3() {
        let (e, backrefs) = Expr::parse("\\1(.)").unwrap();
        assert!(analyze(&e, &backrefs).is_err());
    }

    #[test]
    fn is_literal() {
        let (e, backrefs) = Expr::parse("abc").unwrap();
        let info = analyze(&e, &backrefs).unwrap();
        assert_eq!(info.is_literal(), true);
    }

    #[test]
    fn is_literal_with_repeat() {
        let (e, backrefs) = Expr::parse("abc*").unwrap();
        let info = analyze(&e, &backrefs).unwrap();
        assert_eq!(info.is_literal(), false);
    }
}
