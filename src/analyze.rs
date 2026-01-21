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

use alloc::boxed::Box;
use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;
use core::cmp::min;

use bit_set::BitSet;

use crate::alloc::string::ToString;
use crate::parse::ExprTree;
use crate::vm::CaptureGroupRange;
use crate::{CompileError, Error, Expr, Result};

#[cfg(not(feature = "std"))]
use alloc::collections::BTreeMap as Map;
#[cfg(feature = "std")]
use std::collections::HashMap as Map;

#[derive(Debug)]
pub struct Info<'a> {
    pub(crate) capture_groups: CaptureGroupRange,
    pub(crate) min_size: usize,
    pub(crate) const_size: bool,
    /// Tracks the minimum number of characters that would be consumed in the innermost capture group
    /// before this expression is matched.
    pub(crate) min_pos_in_group: usize,
    pub(crate) hard: bool,
    pub(crate) expr: &'a Expr,
    pub(crate) children: Vec<Info<'a>>,
}

impl<'a> Info<'a> {
    /// Returns the start (first) group number for this expression.
    pub(crate) fn start_group(&self) -> usize {
        self.capture_groups.start()
    }

    /// Returns the end (last) group number for this expression.
    pub(crate) fn end_group(&self) -> usize {
        self.capture_groups.end()
    }

    pub(crate) fn is_literal(&self) -> bool {
        match *self.expr {
            Expr::Literal { casei, .. } => !casei,
            Expr::Concat(_) => self.children.iter().all(|child| child.is_literal()),
            _ => false,
        }
    }

    pub(crate) fn push_literal(&self, buf: &mut String) {
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

struct SizeInfo {
    min_size: usize,
    const_size: bool,
}

/// Represents a subroutine call and its minimum position within a group
#[derive(Debug, Clone)]
struct SubroutineCallInfo {
    /// The group being called
    target_group: usize,
    /// The minimum number of characters consumed in the haystack by the capture group in which this call occurs
    min_pos: usize,
}

struct Analyzer<'a> {
    backrefs: &'a BitSet,
    group_ix: usize,
    /// Stores the analysis info for each group by group number
    // NOTE: uses a Map instead of a Vec because sometimes we start from capture group 1, other times 0
    group_info: Map<usize, SizeInfo>,
    /// Tracks subroutine calls: maps from a group to the subroutines it calls
    subroutine_calls: Map<usize, Vec<SubroutineCallInfo>>,
    /// The current group being analyzed (for tracking which group contains subroutine calls)
    current_group: usize,
    /// Whether we're currently inside a zero-repetition (unreachable code)
    inside_zero_rep: bool,
    /// Groups that are directly executed from root (not inside {0})
    root_groups: BitSet,
    /// Pre-populated map of capture group index to the inner expression of that group
    pre_populated_groups: Map<usize, &'a Expr>,
    /// Track groups currently being analyzed to prevent infinite recursion
    analyzing_groups: BitSet,
}

impl<'a> Analyzer<'a> {
    fn visit(&mut self, expr: &'a Expr, min_pos_in_group: usize) -> Result<Info<'a>> {
        let start_group = self.group_ix;
        let mut children = Vec::new();
        let mut min_size = 0;
        let mut const_size = false;
        let mut hard = false;
        match *expr {
            Expr::Assertion(assertion) if assertion.is_hard() => {
                const_size = true;
                hard = true;
            }
            Expr::Empty | Expr::Assertion(_) => {
                const_size = true;
            }
            Expr::Any { .. } => {
                min_size = 1;
                const_size = true;
            }
            Expr::GeneralNewline { .. } => {
                // \R matches either \r\n (2 chars) or single newline chars (1 char)
                // So it has min_size = 1, but is not const_size
                min_size = 1;
                const_size = false;
                hard = true; // requires backtracking to handle \r\n without backtracking to \r
            }
            Expr::Literal { ref val, casei } => {
                // right now each character in a literal gets its own node, that might change
                min_size = 1;
                const_size = literal_const_size(val, casei);
            }
            Expr::Concat(ref v) => {
                const_size = true;
                let mut pos_in_group = min_pos_in_group;
                for child in v {
                    let child_info = self.visit(child, pos_in_group)?;
                    min_size += child_info.min_size;
                    const_size &= child_info.const_size;
                    hard |= child_info.hard;
                    pos_in_group += child_info.min_size;
                    children.push(child_info);
                }
            }
            Expr::Alt(ref v) => {
                let child_info = self.visit(&v[0], min_pos_in_group)?;
                min_size = child_info.min_size;
                const_size = child_info.const_size;
                hard = child_info.hard;
                children.push(child_info);
                for child in &v[1..] {
                    let child_info = self.visit(child, min_pos_in_group)?;
                    const_size &= child_info.const_size && min_size == child_info.min_size;
                    min_size = min(min_size, child_info.min_size);
                    hard |= child_info.hard;
                    children.push(child_info);
                }
            }
            Expr::Group(ref child) => {
                let group = self.group_ix;
                self.group_ix += 1;
                self.analyzing_groups.insert(group);

                // Track if this group is executed from root (not inside {0})
                if self.current_group == 0 && !self.inside_zero_rep {
                    self.root_groups.insert(group);
                }

                let prev_group = self.current_group;
                self.current_group = group;
                let child_info = self.visit(child, 0)?;
                self.current_group = prev_group;
                self.analyzing_groups.remove(group);
                min_size = child_info.min_size;
                const_size = child_info.const_size;
                // Store the group info for use by backrefs
                self.group_info.insert(
                    group,
                    SizeInfo {
                        min_size,
                        const_size,
                    },
                );
                // If there's a backref to this group, we potentially have to backtrack within the
                // group. E.g. with `(x|xy)\1` and input `xyxy`, `x` matches but then the backref
                // doesn't, so we have to backtrack and try `xy`.
                hard = child_info.hard | self.backrefs.contains(group);
                children.push(child_info);
            }
            Expr::LookAround(ref child, _) => {
                // NOTE: min_pos_in_group might seem weird for lookbehinds
                let child_info = self.visit(child, min_pos_in_group)?;
                // min_size = 0
                const_size = true;
                hard = true;
                children.push(child_info);
            }
            Expr::Repeat {
                ref child, lo, hi, ..
            } => {
                // If lo and hi are both 0, we're in a zero-repetition (unreachable)
                let prev_zero_rep = self.inside_zero_rep;
                if lo == 0 && hi == 0 {
                    self.inside_zero_rep = true;
                }
                let child_info = self.visit(child, min_pos_in_group)?;
                self.inside_zero_rep = prev_zero_rep;
                min_size = child_info.min_size * lo;
                const_size = child_info.const_size && lo == hi;
                hard = child_info.hard;
                children.push(child_info);
            }
            Expr::Delegate { .. } => {
                // Delegate expressions always match exactly 1 character.
                // This constraint ensures consistency in the AST representation.
                min_size = 1;
                const_size = true;
            }
            Expr::Backref { group, .. } => {
                if group == 0 {
                    return Err(Error::CompileError(Box::new(CompileError::InvalidBackref(
                        group,
                    ))));
                }
                // Look up the referenced group's size information
                if let Some(&SizeInfo {
                    min_size: group_min_size,
                    const_size: group_const_size,
                }) = self.group_info.get(&group)
                {
                    min_size = group_min_size;
                    const_size = group_const_size;
                }
                hard = true;
            }
            Expr::AtomicGroup(ref child) => {
                let child_info = self.visit(child, min_pos_in_group)?;
                min_size = child_info.min_size;
                const_size = child_info.const_size;
                hard = true; // TODO: possibly could weaken
                children.push(child_info);
            }
            Expr::KeepOut => {
                hard = true;
                const_size = true;
            }
            Expr::ContinueFromPreviousMatchEnd => {
                hard = true;
                const_size = true;
            }
            Expr::BackrefExistsCondition(_) => {
                hard = true;
                const_size = true;
            }
            Expr::BacktrackingControlVerb(_) => {
                hard = true;
                const_size = true;
            }
            Expr::Conditional {
                ref condition,
                ref true_branch,
                ref false_branch,
            } => {
                hard = true;

                let child_info_condition = self.visit(condition, min_pos_in_group)?;
                let child_info_truth = self.visit(
                    true_branch,
                    min_pos_in_group + child_info_condition.min_size,
                )?;
                let child_info_false = self.visit(false_branch, min_pos_in_group)?;

                min_size = child_info_condition.min_size
                    + min(child_info_truth.min_size, child_info_false.min_size);
                const_size = child_info_condition.const_size
                    && child_info_truth.const_size
                    && child_info_false.const_size
                    // if the condition's size plus the truth branch's size is equal to the false branch's size then it's const size
                    && child_info_condition.min_size + child_info_truth.min_size == child_info_false.min_size;

                children.push(child_info_condition);
                children.push(child_info_truth);
                children.push(child_info_false);
            }
            Expr::SubroutineCall(target_group) => {
                // Track this subroutine call
                // Only skip tracking if we're in unreachable code at the root level
                // Calls inside groups should always be tracked, even if the group is inside {0} at root,
                // because the group can be called as a subroutine from elsewhere
                if !self.inside_zero_rep || self.current_group != 0 {
                    self.subroutine_calls
                        .entry(self.current_group)
                        .or_default()
                        .push(SubroutineCallInfo {
                            target_group,
                            min_pos: min_pos_in_group,
                        });
                }

                // Look up the target group's min_size if available (similar to backrefs)
                // This is important for accurate left recursion detection
                if let Some(&SizeInfo {
                    min_size: group_min_size,
                    const_size: group_const_size,
                }) = self.group_info.get(&target_group)
                {
                    min_size = group_min_size;
                    const_size = group_const_size;
                } else if self.analyzing_groups.contains(target_group) {
                    // Currently analyzing this group - circular reference
                    // Use conservative defaults to avoid infinite recursion
                    min_size = 0;
                    const_size = false;
                } else if let Some(&group_expr) = self.pre_populated_groups.get(&target_group) {
                    // If the group hasn't been seen yet (forward reference),
                    // directly analyze it now
                    self.analyzing_groups.insert(target_group);

                    // Save and update current_group to properly track subroutine calls within the group
                    let prev_group = self.current_group;
                    self.current_group = target_group;
                    let group_info = self.visit(group_expr, 0)?;
                    self.current_group = prev_group;

                    self.analyzing_groups.remove(target_group);

                    min_size = group_info.min_size;
                    const_size = group_info.const_size;
                    // Store the analysis result for future lookups
                    self.group_info.insert(
                        target_group,
                        SizeInfo {
                            min_size,
                            const_size,
                        },
                    );
                } else {
                    // Group doesn't exist - this shouldn't happen as the parser would have caught it
                    min_size = 0;
                    const_size = false;
                }
                hard = true;
            }
            Expr::UnresolvedNamedSubroutineCall { ref name, ix } => {
                return Err(Error::CompileError(Box::new(
                    CompileError::SubroutineCallTargetNotFound(name.to_string(), ix),
                )));
            }
            Expr::BackrefWithRelativeRecursionLevel { .. } => {
                return Err(Error::CompileError(Box::new(
                    CompileError::FeatureNotYetSupported("Backref at recursion level".to_string()),
                )));
            }
            Expr::Absent(ref absent) => {
                use crate::Absent::*;
                match absent {
                    Repeater(ref child) => {
                        let child_info = self.visit(child, min_pos_in_group)?;
                        min_size = 0;
                        const_size = false;
                        hard = true;
                        children.push(child_info);
                    }
                    Expression {
                        ref absent,
                        ref exp,
                    } => {
                        let absent_info = self.visit(absent, min_pos_in_group)?;
                        let exp_info = self.visit(exp, min_pos_in_group)?;
                        min_size = exp_info.min_size;
                        const_size = false;
                        hard = true;
                        children.push(absent_info);
                        children.push(exp_info);
                    }
                    Stopper(ref child) => {
                        let child_info = self.visit(child, min_pos_in_group)?;
                        // Absent stopper doesn't consume any characters itself
                        min_size = 0;
                        const_size = true;
                        hard = true;
                        children.push(child_info);
                    }
                    Clear => {
                        // Range clear doesn't consume any characters
                        min_size = 0;
                        const_size = true;
                        hard = true;
                    }
                }
            }
        };

        Ok(Info {
            expr,
            children,
            capture_groups: CaptureGroupRange(start_group, self.group_ix),
            min_size,
            const_size,
            hard,
            min_pos_in_group,
        })
    }

    /// Check for left-recursive subroutine calls using depth-first search
    fn check_left_recursion(&self, named_groups: &Map<String, usize>) -> Result<()> {
        // Build reverse mapping from group number to group name (if any)
        // so we can give friendly error messages when left recursion is detected
        let mut group_names: Map<usize, String> = Map::new();
        for (name, &group_num) in named_groups.iter() {
            group_names.insert(group_num, name.clone());
        }

        // Compute which groups are reachable from the root (group 0)
        let reachable_groups = self.compute_reachable_groups();

        // Check each reachable group for left recursion
        for &start_group in self.subroutine_calls.keys() {
            if !reachable_groups.contains(start_group) {
                // Skip unreachable groups
                continue;
            }

            let mut visited = BitSet::new();
            let mut recursion_stack = BitSet::new();
            if self.dfs_check_left_recursion(start_group, &mut visited, &mut recursion_stack)? {
                // Found left recursion
                let group_desc = if let Some(name) = group_names.get(&start_group) {
                    format!("group '{}' ({})", name, start_group)
                } else {
                    format!("group {}", start_group)
                };
                return Err(Error::CompileError(Box::new(
                    CompileError::LeftRecursiveSubroutineCall(group_desc),
                )));
            }
        }
        Ok(())
    }

    /// A group is reachable if it's executed from root (not inside {0}) or called from a reachable group
    fn compute_reachable_groups(&self) -> BitSet {
        let mut reachable = BitSet::new();
        let mut to_visit = Vec::new();

        // Start from root (group 0)
        // Group 0 is always reachable
        reachable.insert(0);
        to_visit.push(0);

        // Also mark groups that are directly executed from root (not inside {0})
        for group in self.root_groups.iter() {
            if !reachable.contains(group) {
                reachable.insert(group);
                to_visit.push(group);
            }
        }

        // Propagate reachability through subroutine calls
        while let Some(group) = to_visit.pop() {
            if let Some(calls) = self.subroutine_calls.get(&group) {
                for call_info in calls {
                    if !reachable.contains(call_info.target_group) {
                        reachable.insert(call_info.target_group);
                        to_visit.push(call_info.target_group);
                    }
                }
            }
        }

        reachable
    }

    /// Depth-first search to detect left recursion
    /// Returns true if left recursion is detected
    fn dfs_check_left_recursion(
        &self,
        group: usize,
        visited: &mut BitSet,
        recursion_stack: &mut BitSet,
    ) -> Result<bool> {
        if recursion_stack.contains(group) {
            // We found a cycle. Since we only follow calls at position 0 (see below),
            // reaching a group already in the recursion stack means we have a left-recursive cycle.
            return Ok(true);
        }

        if visited.contains(group) {
            return Ok(false);
        }

        visited.insert(group);
        recursion_stack.insert(group);

        // Check all subroutine calls from this group
        if let Some(calls) = self.subroutine_calls.get(&group) {
            for call_info in calls {
                // Only consider calls at position 0 (potential left recursion)
                if call_info.min_pos == 0
                    && self.dfs_check_left_recursion(
                        call_info.target_group,
                        visited,
                        recursion_stack,
                    )?
                {
                    return Ok(true);
                }
            }
        }

        recursion_stack.remove(group);
        Ok(false)
    }
}

fn literal_const_size(_: &str, _: bool) -> bool {
    // Right now, regex doesn't do sophisticated case folding,
    // test below will fail when that changes, then we need to
    // do something fancier here.
    true
}

/// Recursively collect all Group expressions and their capture group indices
fn collect_groups<'a>(expr: &'a Expr, group_ix: &mut usize, groups: &mut Map<usize, &'a Expr>) {
    match expr {
        Expr::Group(inner) => {
            let current_group = *group_ix;
            *group_ix += 1;
            groups.insert(current_group, inner.as_ref());
            // Continue recursing to find nested groups
            collect_groups(inner.as_ref(), group_ix, groups);
        }
        _ => {
            // Recurse into all children
            for child in expr.children_iter() {
                collect_groups(child, group_ix, groups);
            }
        }
    }
}

/// Analyze the parsed expression to determine whether it requires fancy features.
pub fn analyze<'a>(tree: &'a ExprTree, explicit_capture_group_0: bool) -> Result<Info<'a>> {
    let start_group = if explicit_capture_group_0 { 0 } else { 1 };

    // pre-populate groups if subroutines are present to handle forward references
    let pre_populated_groups = if tree.contains_subroutines {
        let mut groups = Map::new();
        let mut group_ix = start_group;
        collect_groups(&tree.expr, &mut group_ix, &mut groups);
        groups
    } else {
        Map::new()
    };

    let mut analyzer = Analyzer {
        backrefs: &tree.backrefs,
        group_ix: start_group,
        group_info: Map::new(),
        subroutine_calls: Map::new(),
        current_group: 0, // Always start at group 0 (the implicit whole-pattern group)
        inside_zero_rep: false,
        root_groups: BitSet::new(),
        pre_populated_groups,
        analyzing_groups: BitSet::new(),
    };

    let analyzed = analyzer.visit(&tree.expr, 0)?;
    if analyzer.backrefs.contains(0) {
        return Err(Error::CompileError(Box::new(CompileError::InvalidBackref(
            0,
        ))));
    }
    if let Some(highest_backref) = analyzer.backrefs.into_iter().last() {
        if highest_backref > analyzer.group_ix - start_group
            // if we have an explicit capture group 0, and the highest backref is the number of capture groups
            // then that backref refers to an invalid group
            // i.e. `(a\1)b`   has no capture group 1
            //      `(a(b))\2` has no capture group 2
            || highest_backref == analyzer.group_ix && start_group == 0
        {
            return Err(Error::CompileError(Box::new(CompileError::InvalidBackref(
                highest_backref,
            ))));
        }
    }

    // Check for left-recursive subroutine calls (only if subroutines are present)
    if tree.contains_subroutines {
        analyzer.check_left_recursion(&tree.named_groups)?;
    }

    Ok(analyzed)
}

/// Determine if the expression will always only ever match at position 0.
/// Note that false negatives are possible - it can return false even if it could be anchored.
/// This should therefore only be treated as an optimization.
pub fn can_compile_as_anchored(root_expr: &Expr) -> bool {
    use crate::Assertion;

    match root_expr {
        Expr::Concat(children) => match children[0] {
            Expr::Assertion(assertion) => assertion == Assertion::StartText,
            _ => false,
        },
        Expr::Assertion(assertion) => *assertion == Assertion::StartText,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::analyze;
    // use super::literal_const_size;
    use crate::{can_compile_as_anchored, CompileError, Error, Expr};

    // #[test]
    // fn case_folding_safe() {
    //     let re = regex::Regex::new("(?i:ß)").unwrap();
    //     if re.is_match("SS") {
    //         assert!(!literal_const_size("ß", true));
    //     }

    //     // Another tricky example, Armenian ECH YIWN
    //     let re = regex::Regex::new("(?i:\\x{0587})").unwrap();
    //     if re.is_match("\u{0565}\u{0582}") {
    //         assert!(!literal_const_size("\u{0587}", true));
    //     }
    // }

    #[test]
    fn invalid_backref_zero() {
        let tree = Expr::parse_tree(r".\0").unwrap();
        let result = analyze(&tree, false);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(0))
        ));

        let result = analyze(&tree, true);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(0))
        ));

        let tree = Expr::parse_tree(r"(.)\0").unwrap();
        let result = analyze(&tree, false);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(0))
        ));

        let result = analyze(&tree, true);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(0))
        ));

        let tree = Expr::parse_tree(r"(.)\0\1").unwrap();
        let result = analyze(&tree, false);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(0))
        ));
    }

    #[test]
    fn invalid_backref_no_captures() {
        let tree = Expr::parse_tree(r"aa\1").unwrap();
        let result = analyze(&tree, false);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(1))
        ));

        let tree = Expr::parse_tree(r"aaaa\2").unwrap();
        let result = analyze(&tree, false);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(2))
        ));
    }

    #[test]
    fn invalid_backref_with_captures() {
        let tree = Expr::parse_tree(r"a(a)\2").unwrap();
        let result = analyze(&tree, false);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(2))
        ));

        let tree = Expr::parse_tree(r"a(a)\2\1").unwrap();
        let result = analyze(&tree, false);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(2))
        ));
    }

    #[test]
    fn invalid_backref_with_captures_explict_capture_group_zero() {
        let tree = Expr::parse_tree(r"(a(b)\2)c").unwrap();
        let result = analyze(&tree, true);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(2))
        ));

        let tree = Expr::parse_tree(r"(a(b)\1\2)c").unwrap();
        let result = analyze(&tree, true);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(2))
        ));

        let tree = Expr::parse_tree(r"(a\1)b").unwrap();
        let result = analyze(&tree, true);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(1))
        ));

        let tree = Expr::parse_tree(r"(a(b))\2").unwrap();
        let result = analyze(&tree, true);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(2))
        ));
    }

    #[test]
    fn unresolved_subroutine_call_error_takes_precedence_over_invalid_backref() {
        // Regression test for issue where encountering an unresolved subroutine call would
        // cause the analyzer to stop visiting groups, leading to an incomplete group count.
        // This would then cause a misleading "Invalid back reference" error instead of
        // the correct error.
        let tree = Expr::parse_tree(r"(?<a>a)(?<b>b)\g<no_exist>(?<c>c)\k<a>\k<c>").unwrap();
        let result = analyze(&tree, false);

        // Should get the unresolved subroutine call error, not an invalid backref error
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err))
                if matches!(**box_err, CompileError::SubroutineCallTargetNotFound(ref s, _) if s == "no_exist")
        ));
    }

    #[test]
    fn allow_analysis_of_self_backref() {
        // even if it will never match, see issue 103
        assert!(!analyze(&Expr::parse_tree(r"(.\1)").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"((.\1))").unwrap(), true).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(([ab]+)\1b)").unwrap(), false).is_err());
        // in the following scenario it can match
        assert!(!analyze(&Expr::parse_tree(r"(([ab]+?)(?(1)\1| )c)+").unwrap(), false).is_err());
    }

    #[test]
    fn allow_backref_even_when_capture_group_occurs_after_backref() {
        assert!(!analyze(&Expr::parse_tree(r"\1(.)").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(\1(.))").unwrap(), true).is_err());
    }

    #[test]
    fn valid_backref_occurs_after_capture_group() {
        assert!(!analyze(&Expr::parse_tree(r"(.)\1").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"((.)\1)").unwrap(), true).is_err());

        assert!(!analyze(&Expr::parse_tree(r"((.)\2\2)\1").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(.)\1(.)\2").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(.)foo(.)\2").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(.)(foo)(.)\3\2\1").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(.)(foo)(.)\3\1").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(.)(foo)(.)\2\1").unwrap(), false).is_err());
    }

    #[test]
    fn feature_not_yet_supported() {
        let tree = &Expr::parse_tree(r"(a)\k<1-0>").unwrap();
        let result = analyze(tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::FeatureNotYetSupported(_))
        ));
    }

    #[test]
    fn subroutine_call_undefined() {
        let tree = &Expr::parse_tree(r"\g<wrong_name>(?<different_name>a)").unwrap();
        let result = analyze(tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::SubroutineCallTargetNotFound(_, _))
        ));
    }

    #[test]
    fn is_literal() {
        let tree = Expr::parse_tree("abc").unwrap();
        let info = analyze(&tree, false).unwrap();
        assert_eq!(info.is_literal(), true);
    }

    #[test]
    fn is_literal_with_repeat() {
        let tree = Expr::parse_tree("abc*").unwrap();
        let info = analyze(&tree, false).unwrap();
        assert_eq!(info.is_literal(), false);
    }

    #[test]
    fn anchored_for_starttext_assertions() {
        let tree = Expr::parse_tree(r"^(\w+)\1").unwrap();
        assert_eq!(can_compile_as_anchored(&tree.expr), true);

        let tree = Expr::parse_tree(r"^").unwrap();
        assert_eq!(can_compile_as_anchored(&tree.expr), true);
    }

    #[test]
    fn backref_inherits_group_size_info() {
        // Test that backrefs properly inherit min_size and const_size from referenced groups
        let tree = Expr::parse_tree(r"(abc)\1").unwrap();
        let info = analyze(&tree, false).unwrap();
        // The concatenation should have min_size = 3 + 3 = 6 (group + backref)
        assert_eq!(info.min_size, 6);
        assert!(info.const_size);

        // Test with a variable-length group
        let tree = Expr::parse_tree(r"(a+)\1").unwrap();
        let info = analyze(&tree, false).unwrap();
        // The group has min_size = 1, but const_size = false due to the +
        // So the total should be min_size = 2, const_size = false
        assert_eq!(info.min_size, 2);
        assert!(!info.const_size);

        // Test with optional group
        let tree = Expr::parse_tree(r"(a?)\1").unwrap();
        let info = analyze(&tree, false).unwrap();
        // Both group and backref can be empty, so min_size = 0
        assert_eq!(info.min_size, 0);
        assert!(!info.const_size);
    }

    #[test]
    fn backref_forward_reference() {
        // Test forward references (backref before group definition)
        // These should use conservative defaults but still work
        let tree = Expr::parse_tree(r"\1(abc)").unwrap();
        let info = analyze(&tree, false).unwrap();
        // Forward ref gets min_size=0, group gets min_size=3, total=3
        assert_eq!(info.min_size, 3);
        // Forward ref sets const_size=false, so overall is false
        assert!(!info.const_size);
    }

    #[test]
    fn backref_in_lookbehind() {
        assert!(!analyze(&Expr::parse_tree(r"(hello)(?<=\b\1)").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(..)(?<=\1\1)").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(abc)(?<=\1)def").unwrap(), false).is_err());
    }

    #[test]
    fn not_anchored_for_startline_assertions() {
        let tree = Expr::parse_tree(r"(?m)^(\w+)\1").unwrap();
        assert_eq!(can_compile_as_anchored(&tree.expr), false);
    }

    #[test]
    fn min_pos_in_group_calculated_correctly_with_no_groups() {
        let tree = Expr::parse_tree(r"\G").unwrap();
        let info = analyze(&tree, false).unwrap();
        assert_eq!(info.min_size, 0);
        assert_eq!(info.min_pos_in_group, 0);
        assert!(info.const_size);

        let tree = Expr::parse_tree(r"\G(?=abc)\w+").unwrap();
        let info = analyze(&tree, false).unwrap();
        // the lookahead itself has min size 0
        assert_eq!(info.children[1].min_size, 0);
        assert!(info.children[1].const_size);
        // the children of the lookahead have min_size 3 from the literal
        assert_eq!(info.children[1].children[0].min_size, 3);
        assert!(info.children[1].children[0].const_size);
        // after lookahead, the position is reset
        assert_eq!(info.children[2].min_pos_in_group, 0);
        assert_eq!(info.children[2].min_size, 1);
        assert_eq!(info.min_pos_in_group, 0);
        assert!(!info.const_size);

        let tree = Expr::parse_tree(r"(?:ab*|cd){2}(?=bar)\w").unwrap();
        let info = analyze(&tree, false).unwrap();
        // the whole expression has min size 3 (a times 2 plus \w)
        assert_eq!(info.min_size, 3);
        // the min pos of the lookahead is 2
        assert_eq!(info.children[1].min_pos_in_group, 2);
        // after lookahead, the position is reset
        assert_eq!(info.children[2].min_pos_in_group, 2);
        assert_eq!(info.children[2].min_size, 1);
        assert!(!info.const_size);
    }

    #[test]
    fn backtracking_control_verb_is_hard_and_const_size() {
        let tree = Expr::parse_tree(r"(*FAIL)").unwrap();
        let info = analyze(&tree, false).unwrap();
        assert_eq!(info.min_size, 0);
        assert_eq!(info.min_pos_in_group, 0);
        assert!(info.const_size);
    }

    #[test]
    fn min_pos_in_group_calculated_correctly_with_capture_groups() {
        use matches::assert_matches;

        let tree = Expr::parse_tree(r"a(bc)d(e(f)g)").unwrap();
        let info = analyze(&tree, false).unwrap();
        assert_eq!(info.min_pos_in_group, 0);
        // before the capture begins, the min pos in group 0 is 1
        assert_eq!(info.children[1].min_pos_in_group, 1);
        // inside capture group 1, the min pos of the Concat inside the group is 0
        assert_matches!(info.children[1].children[0].expr, Expr::Concat(_));
        assert_eq!(info.children[1].children[0].min_pos_in_group, 0);
        assert!(info.children[1].children[0].const_size);
        // inside capture group 1, the min pos of the c inside the group is 1
        assert_matches!(info.children[1].children[0].children[1].expr, Expr::Literal { val, casei: false } if val == "c");
        assert_eq!(info.children[1].children[0].children[1].min_pos_in_group, 1);

        // prove we are looking at the position of the d after capture group 1
        assert_matches!(info.children[2].expr, Expr::Literal { val, casei: false } if val == "d");
        assert_eq!(info.children[2].min_pos_in_group, 3);
        assert_eq!(info.children[2].start_group(), 2);
        assert_eq!(info.children[2].min_size, 1);

        // prove we are looking at the position of the e in capture group 2
        assert_matches!(info.children[3].children[0].children[0].expr, Expr::Literal { val, casei: false } if val == "e");
        assert_eq!(info.children[3].children[0].children[0].min_pos_in_group, 0);
    }

    #[test]
    fn absent_repeater_is_hard_and_not_const_size() {
        let tree = Expr::parse_tree(r"(?~abc)").unwrap();
        let info = analyze(&tree, false).unwrap();
        assert_eq!(info.min_size, 0);
        assert!(!info.const_size);
        assert!(info.hard);
    }

    #[test]
    fn absent_expression_is_hard_and_not_const_size() {
        let tree = Expr::parse_tree(r"(?~|abc|\d+)").unwrap();
        let info = analyze(&tree, false).unwrap();
        // min_size comes from exp part (\d+ has min_size 1)
        assert_eq!(info.min_size, 1);
        assert!(!info.const_size);
        assert!(info.hard);
    }

    #[test]
    fn range_clear_is_hard_and_const_size() {
        let tree = Expr::parse_tree(r"(?~|)").unwrap();
        let info = analyze(&tree, false).unwrap();
        assert_eq!(info.min_size, 0);
        assert!(info.const_size);
        assert!(info.hard);
    }

    #[test]
    fn left_recursive_subroutine_direct() {
        // Direct left recursion: group 1 calls itself at position 0
        let tree = Expr::parse_tree(r"(\g<1>a)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));

        let tree = Expr::parse_tree(r"abc(\g<1>a)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));
    }

    #[test]
    fn not_left_recursive_subroutine_after_group() {
        let tree = Expr::parse_tree(r"(a)\g<1>").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_ok());

        let tree = Expr::parse_tree(r"(?<test>a)\g<test>").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_ok());
    }

    #[test]
    fn left_recursive_subroutine_at_start() {
        // Left recursion at start of group: (\g<1>a)
        let tree = Expr::parse_tree(r"(\g<1>a)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));

        let tree = Expr::parse_tree(r"(?<test>\g<test>a)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));
    }

    #[test]
    fn left_recursive_subroutine_indirect() {
        // Indirect left recursion: non-nested subroutine calls to each other
        let tree = Expr::parse_tree(r"(\g<2>)(\g<1>)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));

        let tree = Expr::parse_tree(r"(\g<2>)(\g<1>a)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));
    }

    #[test]
    fn left_recursive_subroutine_with_alternation() {
        // Left recursion through alternation, depending which branch is taken it could be left-recursive
        let tree = Expr::parse_tree(r"(a|\g<1>)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));
    }

    #[test]
    fn not_left_recursive_after_char() {
        // Not left recursive because subroutine call is after a character was consumed
        let tree = Expr::parse_tree(r"(a\g<1>)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_ok());
    }

    #[test]
    fn not_left_recursive_zero_repetition() {
        // Not left recursive because subroutine call is unreachable
        let tree = Expr::parse_tree(r"(a?\g<1>){0}").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_ok());
    }

    #[test]
    fn left_recursive_with_both_positions() {
        // Left recursive because \g<1> appears at position 0 in the group even though also at end at position 1
        let tree = Expr::parse_tree(r"(\g<1>a\g<1>)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));
    }

    #[test]
    fn left_recursive_with_lookahead() {
        let tree = Expr::parse_tree(r"((?=a)\g<1>)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));
    }

    #[test]
    fn self_recursive_group_zero() {
        // Self-recursive on group 0 after a character
        let tree = Expr::parse_tree(r"a\g<0>").unwrap();
        let result = analyze(&tree, false);
        // Group 0 calls itself at position 1 (after 'a'), so this is NOT left recursive
        assert!(result.is_ok());
    }

    #[test]
    fn not_left_recursive_forward_call() {
        // Forward subroutine call - not left recursive: \g<1>(a)
        let tree = Expr::parse_tree(r"\g<1>(a)").unwrap();
        let result = analyze(&tree, false);
        // The call happens before the group is defined, but it's at position 0 of group 0 (implicit)
        // which calls group 1. Group 1 doesn't call anything, so no cycle.
        assert!(result.is_ok());
    }

    #[test]
    fn left_recursive_group_zero_explicit() {
        // Self-recursive on explicit group 0: (a\g<0>)
        let tree = Expr::parse_tree(r"(a\g<0>)").unwrap();
        let result = analyze(&tree, true);
        assert!(result.is_ok());
    }

    #[test]
    fn left_recursive_group_zero_at_start() {
        // Self-recursive on explicit group 0 at start: (\g<0>a)
        let tree = Expr::parse_tree(r"(\g<0>a)").unwrap();
        let result = analyze(&tree, true);
        // With explicit group 0, \g<0> at position 0 is left-recursive
        assert!(result.is_err());
    }

    #[test]
    fn three_way_indirect_recursion() {
        // Three-way indirect recursion
        let tree = Expr::parse_tree(r"(\g<2>)(\g<3>)(a\g<1>)").unwrap();
        let result = analyze(&tree, false);
        // Group 1 -> Group 2 (at pos 0)
        // Group 2 -> Group 3 (at pos 0)
        // Group 3 -> Group 1 (at pos 1, after 'a')
        // This forms a cycle, but the call from group 3 to group 1 is at position 1
        // So it's not left-recursive
        assert!(result.is_ok());
    }

    #[test]
    fn three_way_left_recursive() {
        // Three-way left recursion
        let tree = Expr::parse_tree(r"(\g<2>)(\g<3>)(\g<1>)").unwrap();
        let result = analyze(&tree, false);
        // Group 1 -> Group 2 (at pos 0)
        // Group 2 -> Group 3 (at pos 0)
        // Group 3 -> Group 1 (at pos 0)
        // This forms a left-recursive cycle
        assert!(result.is_err());

        let tree = Expr::parse_tree(r"(\g<2>a)(\g<3>b)(\g<1>c)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
    }

    #[test]
    fn left_recursive_with_call_to_defined_group() {
        // Even though the call from Group 1 to Group 2 is inside {0} at root level,
        // Group 1's pattern can still be executed when called from Group 2
        // Group 1 contains a?\g<2> - calls group 2 (when executed)
        // Group 2 contains \g<1> - calls group 1 at position 0
        // This creates a cycle: Group 2 -> Group 1 (at pos 0) -> Group 2 (at pos 0)
        let tree = Expr::parse_tree(r"(a?\g<2>){0}(\g<1>)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err(), "Should be left-recursive");
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));
    }

    #[test]
    fn no_left_recursion_complex_pattern() {
        // Group n (1): |\g<m>\g<n> - calls m then itself, but m has min_size > 0
        // Group m (2): a(b)\g<m> - calls itself after 'ab'
        let tree = Expr::parse_tree(r"(?<n>|\g<m>\g<n>)\z|\zEND (?<m>a(b)\g<m>)").unwrap();
        let result = analyze(&tree, false);

        assert!(
            result.is_ok(),
            "Pattern should not be left-recursive because group m has min_size > 0"
        );
    }
}
