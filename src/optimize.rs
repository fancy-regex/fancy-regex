// Copyright 2025 The Fancy Regex Authors.
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

//! Optimization of regex expressions.

use crate::parse::ExprTree;
use crate::Expr;
use crate::LookAround;

/// Rewrite the expression tree to help the VM compile an efficient program.
/// Expects the tree to have been wrapped ready for the VM, such that the first capture group will be 0.
pub fn optimize(tree: ExprTree) -> (ExprTree, bool) {
    // self recursion prevents us from moving the trailing lookahead out of group 0
    if !tree.self_recursive {
        let mut optimized_tree = tree.clone();
        let requires_capture_group_fixup = optimize_trailing_lookahead(&mut optimized_tree);
        (optimized_tree, requires_capture_group_fixup)
    } else {
        (tree, false)
    }
}

fn optimize_trailing_lookahead(tree: &mut ExprTree) -> bool {
    // returns a boolean to say whether the optimization was applied.
    // - if it was applied, capture group 0 is no longer implicit, but explicit
    //   if/when the whole expression gets delegated to regex-automata
    // converts i.e. original pattern `a(?=b)` when wrapped in the capture group 0
    // as `(?s:.)*?(a(?=b))`
    // to `(?s:.)*?(a)b`

    // here we traverse the tree to find capture group 0
    // we start with the root concat, and get the last child
    if let Expr::Concat(ref mut root_concat_children) = tree.expr {
        if let Some(last_child_of_root_concat) = &mut root_concat_children.last_mut() {
            // we get the last child if it is a capture group
            if let Expr::Group(ref mut inner) = last_child_of_root_concat {
                // if the inner expression to the capture group is a concatenation
                if let Expr::Concat(ref mut children) = **inner {
                    // whose last child is a positive lookahead
                    if let Some(Expr::LookAround(_, LookAround::LookAhead)) = &children.last_mut() {
                        // then pop the lookahead and attach the inner expression to the root concat
                        // after capture group 0 ends
                        let inner_box = children.pop().expect("lookaround should be popped");
                        if let Expr::LookAround(inner, LookAround::LookAhead) = inner_box {
                            root_concat_children.push(*inner);
                            return true;
                        }
                    }
                }
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::optimize;
    use crate::parse::make_literal;
    use crate::Expr;

    #[test]
    fn trailing_positive_lookahead_optimized() {
        let tree = Expr::parse_tree("a(?=b)").unwrap();
        let (optimized_tree, requires_capture_group_fixup) = optimize(tree);
        assert_eq!(requires_capture_group_fixup, true);
        let mut s = String::new();
        optimized_tree.expr.to_str(&mut s, 0);
        assert_eq!(s, "(?s:.)*?(a)b");
    }

    #[test]
    fn trailing_positive_lookahead_with_alternative_optimized() {
        let tree = Expr::parse_tree("a(?=b|c)").unwrap();
        let (optimized_tree, requires_capture_group_fixup) = optimize(tree);
        assert_eq!(requires_capture_group_fixup, true);
        let mut s = String::new();
        optimized_tree.expr.to_str(&mut s, 0);
        assert_eq!(s, "(?s:.)*?(a)(?:b|c)");
    }

    #[test]
    fn trailing_positive_lookahead_moved_even_if_not_easy() {
        let tree = Expr::parse_tree(r"(a)\1(?=c)").unwrap();
        let (optimized_tree, requires_capture_group_fixup) = optimize(tree);
        assert_eq!(requires_capture_group_fixup, true);
        assert_eq!(
            optimized_tree.expr,
            Expr::Concat(vec![
                Expr::Repeat {
                    child: Box::new(Expr::Any { newline: true }),
                    lo: 0,
                    hi: usize::MAX,
                    greedy: false
                },
                Expr::Group(Box::new(Expr::Concat(vec![
                    Expr::Group(Box::new(make_literal("a"))),
                    Expr::Backref {
                        group: 1,
                        casei: false
                    }
                ]))),
                make_literal("c"),
            ])
        );
    }

    #[test]
    fn trailing_positive_lookahead_left_alone_when_self_recursive() {
        let tree = Expr::parse_tree(r"ab?\g<0>?(?=a|$)").unwrap();
        let (optimized_tree, requires_capture_group_fixup) = optimize(tree.clone());
        assert_eq!(requires_capture_group_fixup, false);
        assert_eq!(&optimized_tree.expr, &tree.expr);
    }

    #[test]
    fn trailing_negative_lookahead_left_alone() {
        let tree = Expr::parse_tree(r"a(?!b)").unwrap();
        let (optimized_tree, requires_capture_group_fixup) = optimize(tree.clone());
        assert_eq!(requires_capture_group_fixup, false);
        assert_eq!(&optimized_tree.expr, &tree.expr);
    }

    #[test]
    fn trailing_positive_lookbehind_left_alone() {
        let tree = Expr::parse_tree(r"(?<=b)").unwrap();
        let (optimized_tree, requires_capture_group_fixup) = optimize(tree.clone());
        assert_eq!(requires_capture_group_fixup, false);
        assert_eq!(&optimized_tree.expr, &tree.expr);
    }
}
