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

//! A regex parser yielding an AST.

use crate::RegexOptions;
use alloc::boxed::Box;
use alloc::string::{String, ToString};
use alloc::sync::Arc;
use alloc::vec::Vec;
use alloc::{format, vec};

use bit_set::BitSet;
use regex_syntax::escape_into;

use crate::parse_flags::*;
use crate::{codepoint_len, CompileError, Error, Expr, ParseError, Result, MAX_RECURSION};
use crate::{Absent, Assertion, BacktrackingControlVerb, LookAround::*};

#[cfg(not(feature = "std"))]
pub(crate) type NamedGroups = alloc::collections::BTreeMap<String, usize>;
#[cfg(feature = "std")]
pub(crate) type NamedGroups = std::collections::HashMap<String, usize>;

#[derive(Debug, Clone)]
pub struct ExprTree {
    pub expr: Expr,
    pub backrefs: BitSet,
    pub named_groups: NamedGroups,
    #[allow(dead_code)]
    pub(crate) contains_subroutines: bool,
    pub(crate) self_recursive: bool,
}

#[derive(Debug)]
pub(crate) struct Parser<'a> {
    re: &'a str, // source
    backrefs: BitSet,
    flags: u32,
    named_groups: NamedGroups,
    numeric_backrefs: bool,
    curr_group: usize, // need to keep track of which group number we're parsing
    contains_subroutines: bool,
    has_unresolved_subroutines: bool,
    self_recursive: bool,
}

struct NamedBackrefOrSubroutine<'a> {
    ix: usize,
    group_ix: Option<usize>,
    group_name: Option<&'a str>,
    recursion_level: Option<isize>,
}

impl<'a> Parser<'a> {
    pub(crate) fn parse_with_flags(re: &str, flags: u32) -> Result<ExprTree> {
        let mut p = Parser::new(re, flags);
        let (ix, mut expr) = p.parse_re(0, 0)?;
        if ix < re.len() {
            return Err(Error::ParseError(
                ix,
                ParseError::GeneralParseError("end of string not reached".to_string()),
            ));
        }

        if p.has_unresolved_subroutines {
            p.has_unresolved_subroutines = false;
            p.resolve_named_subroutine_calls(&mut expr);
        }

        Ok(ExprTree {
            expr,
            backrefs: p.backrefs,
            named_groups: p.named_groups,
            contains_subroutines: p.contains_subroutines,
            self_recursive: p.self_recursive,
        })
    }

    pub(crate) fn parse(re: &str) -> Result<ExprTree> {
        Self::parse_with_flags(re, RegexOptions::default().compute_flags())
    }

    fn new(re: &str, flags: u32) -> Parser<'_> {
        let flags = flags | FLAG_UNICODE;

        Parser {
            re,
            backrefs: Default::default(),
            named_groups: Default::default(),
            numeric_backrefs: false,
            flags,
            curr_group: 0,
            contains_subroutines: false,
            has_unresolved_subroutines: false,
            self_recursive: false,
        }
    }

    fn parse_re(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        let (ix, child) = self.parse_branch(ix, depth)?;
        let mut ix = self.optional_whitespace(ix)?;
        if self.re[ix..].starts_with('|') {
            let mut children = vec![child];
            while self.re[ix..].starts_with('|') {
                ix += 1;
                let (next, child) = self.parse_branch(ix, depth)?;
                children.push(child);
                ix = self.optional_whitespace(next)?;
            }
            return Ok((ix, Expr::Alt(children)));
        }
        // can't have numeric backrefs and named backrefs
        if self.numeric_backrefs && !self.named_groups.is_empty() {
            return Err(Error::CompileError(Box::new(
                CompileError::NamedBackrefOnly,
            )));
        }
        Ok((ix, child))
    }

    fn parse_branch(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        let mut children = Vec::new();
        let mut ix = ix;
        while ix < self.re.len() {
            let (next, child) = self.parse_piece(ix, depth)?;
            if next == ix {
                break;
            }
            if child != Expr::Empty {
                children.push(child);
            }
            ix = next;
        }
        match children.len() {
            0 => Ok((ix, Expr::Empty)),
            1 => Ok((ix, children.pop().unwrap())),
            _ => Ok((ix, Expr::Concat(children))),
        }
    }

    fn parse_piece(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        let (ix, child) = self.parse_atom(ix, depth)?;
        let mut ix = self.optional_whitespace(ix)?;
        if ix < self.re.len() {
            // fail when child is empty?
            let (lo, hi) = match self.re.as_bytes()[ix] {
                b'?' => (0, 1),
                b'*' => (0, usize::MAX),
                b'+' => (1, usize::MAX),
                b'{' => {
                    match self.parse_repeat(ix) {
                        Ok((next, lo, hi)) => {
                            ix = next - 1;
                            (lo, hi)
                        }
                        Err(_) => {
                            // Invalid repeat syntax, which results in `{` being treated as a literal
                            return Ok((ix, child));
                        }
                    }
                }
                _ => return Ok((ix, child)),
            };
            if !self.is_repeatable(&child) {
                return Err(Error::ParseError(ix, ParseError::TargetNotRepeatable));
            }
            ix += 1;
            ix = self.optional_whitespace(ix)?;
            let mut greedy = true;
            if ix < self.re.len() && self.re.as_bytes()[ix] == b'?' {
                greedy = false;
                ix += 1;
            }
            greedy ^= self.flag(FLAG_SWAP_GREED);
            let mut node = Expr::Repeat {
                child: Box::new(child),
                lo,
                hi,
                greedy,
            };
            if ix < self.re.len() && self.re.as_bytes()[ix] == b'+' {
                ix += 1;
                node = Expr::AtomicGroup(Box::new(node));
            }
            return Ok((ix, node));
        }
        Ok((ix, child))
    }

    fn is_repeatable(&self, child: &Expr) -> bool {
        match child {
            Expr::LookAround(_, _) => false,
            Expr::Empty => false,
            // In Oniguruma mode, repetition after assertions is not allowed
            Expr::Assertion(_) => !self.flag(FLAG_ONIGURUMA_MODE),
            Expr::KeepOut => false,
            Expr::ContinueFromPreviousMatchEnd => false,
            Expr::BackrefExistsCondition(_) => false,
            Expr::BacktrackingControlVerb(_) => false,
            _ => true,
        }
    }

    // ix, lo, hi
    fn parse_repeat(&self, ix: usize) -> Result<(usize, usize, usize)> {
        let ix = self.optional_whitespace(ix + 1)?; // skip opening '{'
        let bytes = self.re.as_bytes();
        if ix == self.re.len() {
            return Err(Error::ParseError(ix, ParseError::InvalidRepeat));
        }
        let mut end = ix;
        let lo = if bytes[ix] == b',' {
            0
        } else if let Some((next, lo)) = parse_decimal(self.re, ix) {
            end = next;
            lo
        } else {
            return Err(Error::ParseError(ix, ParseError::InvalidRepeat));
        };
        let ix = self.optional_whitespace(end)?; // past lo number
        if ix == self.re.len() {
            return Err(Error::ParseError(ix, ParseError::InvalidRepeat));
        }
        end = ix;
        let hi = match bytes[ix] {
            b'}' => lo,
            b',' => {
                end = self.optional_whitespace(ix + 1)?; // past ','
                if let Some((next, hi)) = parse_decimal(self.re, end) {
                    end = next;
                    hi
                } else {
                    usize::MAX
                }
            }
            _ => return Err(Error::ParseError(ix, ParseError::InvalidRepeat)),
        };
        let ix = self.optional_whitespace(end)?; // past hi number
        if ix == self.re.len() || bytes[ix] != b'}' {
            return Err(Error::ParseError(ix, ParseError::InvalidRepeat));
        }
        Ok((ix + 1, lo, hi))
    }

    fn parse_atom(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        let ix = self.optional_whitespace(ix)?;
        if ix == self.re.len() {
            return Ok((ix, Expr::Empty));
        }
        match self.re.as_bytes()[ix] {
            b'.' => Ok((
                ix + 1,
                Expr::Any {
                    newline: self.flag(FLAG_DOTNL),
                },
            )),
            b'^' => Ok((
                ix + 1,
                if self.flag(FLAG_MULTI) {
                    // TODO: support crlf flag
                    Expr::Assertion(Assertion::StartLine { crlf: false })
                } else {
                    Expr::Assertion(Assertion::StartText)
                },
            )),
            b'$' => Ok((
                ix + 1,
                if self.flag(FLAG_MULTI) {
                    // TODO: support crlf flag
                    Expr::Assertion(Assertion::EndLine { crlf: false })
                } else {
                    Expr::Assertion(Assertion::EndText)
                },
            )),
            b'(' => self.parse_group(ix, depth),
            b'\\' => self.parse_escape(ix, false),
            b'+' | b'*' | b'?' | b'|' | b')' => Ok((ix, Expr::Empty)),
            b'[' => self.parse_class(ix),
            b => {
                // TODO: maybe want to match multiple codepoints?
                let next = ix + codepoint_len(b);
                Ok((
                    next,
                    Expr::Literal {
                        val: String::from(&self.re[ix..next]),
                        casei: self.flag(FLAG_CASEI),
                    },
                ))
            }
        }
    }

    fn parse_named_backref(
        &mut self,
        ix: usize,
        open: &str,
        close: &str,
        allow_relative: bool,
    ) -> Result<(usize, Expr)> {
        let NamedBackrefOrSubroutine {
            ix: end,
            group_ix,
            group_name,
            recursion_level,
        } = self.parse_named_backref_or_subroutine(ix, open, close, allow_relative)?;
        if let Some(group) = group_ix {
            self.backrefs.insert(group);
            return Ok((
                end,
                if let Some(recursion_level) = recursion_level {
                    Expr::BackrefWithRelativeRecursionLevel {
                        group,
                        relative_level: recursion_level,
                        casei: self.flag(FLAG_CASEI),
                    }
                } else {
                    Expr::Backref {
                        group,
                        casei: self.flag(FLAG_CASEI),
                    }
                },
            ));
        }
        if let Some(group_name) = group_name {
            // here the name was parsed but doesn't match a capture group we have already parsed
            return Err(Error::ParseError(
                ix,
                ParseError::InvalidGroupNameBackref(group_name.to_string()),
            ));
        }
        unreachable!()
    }

    fn parse_named_subroutine_call(
        &mut self,
        ix: usize,
        open: &str,
        close: &str,
        allow_relative: bool,
    ) -> Result<(usize, Expr)> {
        let NamedBackrefOrSubroutine {
            ix: end,
            group_ix,
            group_name,
            recursion_level,
        } = self.parse_named_backref_or_subroutine(ix, open, close, allow_relative)?;
        if recursion_level.is_some() {
            return Err(Error::ParseError(ix, ParseError::InvalidGroupName));
        }
        if let Some(group) = group_ix {
            self.contains_subroutines = true;
            if group == 0 {
                self.self_recursive = true;
            }
            return Ok((end, Expr::SubroutineCall(group)));
        }
        if let Some(group_name) = group_name {
            // here the name was parsed but doesn't match a capture group we have already parsed
            let expr = Expr::UnresolvedNamedSubroutineCall {
                name: group_name.to_string(),
                ix,
            };
            self.has_unresolved_subroutines = true;
            self.contains_subroutines = true;
            return Ok((end, expr));
        }
        unreachable!()
    }

    fn parse_named_backref_or_subroutine(
        &self,
        ix: usize,
        open: &str,
        close: &str,
        allow_relative: bool,
    ) -> Result<NamedBackrefOrSubroutine<'_>> {
        if let Some(ParsedId {
            id,
            mut relative,
            skip,
        }) = parse_id(&self.re[ix..], open, close, allow_relative)
        {
            let group = if let Some(group) = self.named_groups.get(id) {
                Some(*group)
            } else if let Ok(group) = id.parse::<usize>() {
                Some(group)
            } else if let Some(relative_group) = relative {
                if id.is_empty() {
                    relative = None;
                    self.curr_group.checked_add_signed(if relative_group < 0 {
                        relative_group + 1
                    } else {
                        relative_group
                    })
                } else {
                    None
                }
            } else {
                None
            };
            if let Some(group) = group {
                Ok(NamedBackrefOrSubroutine {
                    ix: ix + skip,
                    group_ix: Some(group),
                    group_name: None,
                    recursion_level: relative,
                })
            } else {
                // here the name was parsed but doesn't match a capture group we have already parsed
                Ok(NamedBackrefOrSubroutine {
                    ix: ix + skip,
                    group_ix: None,
                    group_name: Some(id),
                    recursion_level: relative,
                })
            }
        } else {
            // in this case the name can't be parsed
            Err(Error::ParseError(ix, ParseError::InvalidGroupName))
        }
    }

    fn parse_numbered_backref(&mut self, ix: usize) -> Result<(usize, Expr)> {
        let (end, group) = self.parse_numbered_backref_or_subroutine_call(ix)?;
        self.numeric_backrefs = true;
        self.backrefs.insert(group);
        Ok((
            end,
            Expr::Backref {
                group,
                casei: self.flag(FLAG_CASEI),
            },
        ))
    }

    fn parse_numbered_subroutine_call(&mut self, ix: usize) -> Result<(usize, Expr)> {
        let (end, group) = self.parse_numbered_backref_or_subroutine_call(ix)?;
        self.numeric_backrefs = true;
        self.contains_subroutines = true;
        if group == 0 {
            self.self_recursive = true;
        }
        Ok((end, Expr::SubroutineCall(group)))
    }

    fn parse_numbered_backref_or_subroutine_call(&self, ix: usize) -> Result<(usize, usize)> {
        if let Some((end, group)) = parse_decimal(self.re, ix) {
            // protect BitSet against unreasonably large value
            if group < self.re.len() / 2 {
                return Ok((end, group));
            }
        }
        Err(Error::ParseError(ix, ParseError::InvalidBackref))
    }

    // ix points to \ character
    fn parse_escape(&mut self, ix: usize, in_class: bool) -> Result<(usize, Expr)> {
        let bytes = self.re.as_bytes();
        let Some(b) = bytes.get(ix + 1).copied() else {
            return Err(Error::ParseError(ix, ParseError::TrailingBackslash));
        };
        let end = ix + 1 + codepoint_len(b);
        Ok(if b.is_ascii_digit() {
            return self.parse_numbered_backref(ix + 1);
        } else if matches!(b, b'k') && !in_class {
            // Named backref: \k<name>
            if bytes.get(end) == Some(&b'\'') {
                return self.parse_named_backref(end, "'", "'", true);
            } else {
                return self.parse_named_backref(end, "<", ">", true);
            }
        } else if b == b'A' && !in_class {
            (end, Expr::Assertion(Assertion::StartText))
        } else if b == b'z' && !in_class {
            (end, Expr::Assertion(Assertion::EndText))
        } else if b == b'Z' && !in_class {
            // \Z matches at the end of the string, or before any number of newlines at the end
            (
                end,
                Expr::Assertion(Assertion::EndTextIgnoreTrailingNewlines),
            )
        } else if (b == b'b' || b == b'B') && !in_class {
            let check_pos = self.optional_whitespace(end)?;
            if bytes.get(check_pos) == Some(&b'{') {
                let next_open_brace_pos = self.optional_whitespace(check_pos + 1)?;
                let is_repetition = matches!(
                    bytes.get(next_open_brace_pos),
                    Some(&ch) if ch.is_ascii_digit() || ch == b','
                );
                if !is_repetition {
                    return self.parse_word_boundary_brace(ix);
                }
            }
            let expr = if b == b'b' {
                Expr::Assertion(Assertion::WordBoundary)
            } else {
                Expr::Assertion(Assertion::NotWordBoundary)
            };
            (end, expr)
        } else if b == b'<' && !in_class {
            let expr = if self.flag(FLAG_ONIGURUMA_MODE) {
                make_literal("<")
            } else {
                Expr::Assertion(Assertion::LeftWordBoundary)
            };
            (end, expr)
        } else if b == b'>' && !in_class {
            let expr = if self.flag(FLAG_ONIGURUMA_MODE) {
                make_literal(">")
            } else {
                Expr::Assertion(Assertion::RightWordBoundary)
            };
            (end, expr)
        } else if matches!(b | 32, b'd' | b's' | b'w') {
            (
                end,
                Expr::Delegate {
                    inner: String::from(&self.re[ix..end]),
                    casei: self.flag(FLAG_CASEI),
                },
            )
        } else if (b | 32) == b'h' {
            let s = if b == b'h' {
                "[0-9A-Fa-f]"
            } else {
                "[^0-9A-Fa-f]"
            };
            (
                end,
                Expr::Delegate {
                    inner: String::from(s),
                    casei: false,
                },
            )
        } else if b == b'x' {
            let end = self.optional_whitespace(end)?;
            return self.parse_hex(end, 2);
        } else if b == b'u' {
            let end = self.optional_whitespace(end)?;
            return self.parse_hex(end, 4);
        } else if b == b'U' {
            let end = self.optional_whitespace(end)?;
            return self.parse_hex(end, 8);
        } else if (b | 32) == b'p' && end != bytes.len() {
            let mut end = end;
            let b = bytes[end];
            end += codepoint_len(b);
            if b == b'{' {
                loop {
                    if end == self.re.len() {
                        return Err(Error::ParseError(ix, ParseError::UnclosedUnicodeName));
                    }
                    let b = bytes[end];
                    if b == b'}' {
                        end += 1;
                        break;
                    }
                    end += codepoint_len(b);
                }
            }
            (
                end,
                Expr::Delegate {
                    inner: remap_unicode_property_if_necessary(
                        &self.re[ix..end],
                        self.flag(FLAG_UNICODE),
                        in_class,
                    ),
                    casei: self.flag(FLAG_CASEI),
                },
            )
        } else if b == b'K' && !in_class {
            (end, Expr::KeepOut)
        } else if b == b'G' && !in_class {
            (end, Expr::ContinueFromPreviousMatchEnd)
        } else if b == b'O' && !in_class {
            (end, Expr::Any { newline: true })
        } else if b == b'N' && !in_class {
            (end, Expr::Any { newline: false })
        } else if b == b'g' && !in_class {
            if end == self.re.len() {
                return Err(Error::ParseError(
                    ix,
                    ParseError::InvalidEscape("\\g".to_string()),
                ));
            }
            let b = bytes[end];
            if b.is_ascii_digit() {
                self.parse_numbered_subroutine_call(end)?
            } else if b == b'\'' {
                self.parse_named_subroutine_call(end, "'", "'", true)?
            } else {
                self.parse_named_subroutine_call(end, "<", ">", true)?
            }
        } else {
            // printable ASCII (including space, see issue #29)
            (
                end,
                make_literal(match b {
                    b'a' => "\x07", // BEL
                    b'b' => "\x08", // BS
                    b'f' => "\x0c", // FF
                    b'n' => "\n",   // LF
                    b'r' => "\r",   // CR
                    b't' => "\t",   // TAB
                    b'v' => "\x0b", // VT
                    b'e' => "\x1b", // ESC
                    b' ' => " ",
                    b => {
                        let s = &self.re[ix + 1..end];
                        if b.is_ascii_alphabetic()
                            && !matches!(
                                b,
                                b'k' | b'A' | b'z' | b'b' | b'B' | b'<' | b'>' | b'K' | b'G'
                            )
                        {
                            return Err(Error::ParseError(
                                ix,
                                ParseError::InvalidEscape(format!("\\{}", s)),
                            ));
                        } else {
                            s
                        }
                    }
                }),
            )
        })
    }

    // ix points after '\x', eg to 'A0' or '{12345}', or after `\u` or `\U`
    fn parse_hex(&self, ix: usize, digits: usize) -> Result<(usize, Expr)> {
        if ix >= self.re.len() {
            // Incomplete escape sequence
            return Err(Error::ParseError(ix, ParseError::InvalidHex));
        }
        let bytes = self.re.as_bytes();
        let b = bytes[ix];
        // Parse fixed-width hex (e.g., \xAB)
        if ix + digits <= self.re.len() && bytes[ix..ix + digits].iter().all(|&b| is_hex_digit(b)) {
            let hex_str = &self.re[ix..ix + digits];
            return self.hex_to_literal(ix, ix + digits, hex_str);
        }
        // Parse brace-enclosed hex (e.g., \u{00AB})
        if b == b'{' {
            let mut pos = ix + 1;
            let mut hex_chars = String::new();
            while pos < self.re.len() {
                // Skip whitespace/comments if FLAG_IGNORE_SPACE is set
                pos = self.optional_whitespace(pos)?;
                if pos >= self.re.len() {
                    return Err(Error::ParseError(ix, ParseError::InvalidHex));
                }
                let b = bytes[pos];
                if b == b'}' && !hex_chars.is_empty() {
                    return self.hex_to_literal(ix, pos + 1, &hex_chars);
                }
                if is_hex_digit(b) && hex_chars.len() < 8 {
                    hex_chars.push(b as char);
                    pos += 1;
                } else {
                    return Err(Error::ParseError(ix, ParseError::InvalidHex));
                }
            }
        }
        Err(Error::ParseError(ix, ParseError::InvalidHex))
    }

    fn hex_to_literal(&self, ix: usize, end: usize, hex_str: &str) -> Result<(usize, Expr)> {
        let codepoint = u32::from_str_radix(hex_str, 16).unwrap();
        if let Some(c) = char::from_u32(codepoint) {
            Ok((
                end,
                Expr::Literal {
                    val: c.to_string(),
                    casei: self.flag(FLAG_CASEI),
                },
            ))
        } else {
            Err(Error::ParseError(ix, ParseError::InvalidCodepointValue))
        }
    }

    // ix points before '\b' or '\B'
    fn parse_word_boundary_brace(&self, ix: usize) -> Result<(usize, Expr)> {
        let bytes = self.re.as_bytes();

        // Verify that we have '\b' or '\B'
        if !matches!(bytes.get(ix..ix + 2), Some([b'\\', b'b' | b'B'])) {
            return Err(Error::ParseError(
                ix,
                ParseError::InvalidEscape("\\b{...}".to_string()),
            ));
        }
        // Skip whitespace/comments after \b or \B if FLAG_IGNORE_SPACE is set
        let brace_start = self.optional_whitespace(ix + 2)?;
        // Verify we have '{'
        if bytes.get(brace_start) != Some(&b'{') {
            return Err(Error::ParseError(
                ix,
                ParseError::InvalidEscape("\\b{...}".to_string()),
            ));
        }
        // Extract content between braces
        let mut pos = brace_start + 1;
        let mut content = String::new();
        while pos < self.re.len() {
            let b = bytes[pos];
            if b == b'}' {
                break;
            }
            // Skip whitespace/comments if FLAG_IGNORE_SPACE is set
            let next_pos = self.optional_whitespace(pos)?;
            if next_pos > pos {
                // Whitespace was skipped
                pos = next_pos;
                if pos >= self.re.len() || bytes[pos] == b'}' {
                    break;
                }
            }
            // Add non-whitespace character to content
            let b = bytes[pos];
            if b != b'}' {
                content.push(b as char);
                pos += codepoint_len(b);
            }
        }

        let end_brace = pos;
        if end_brace >= self.re.len() || bytes[end_brace] != b'}' {
            return Err(Error::ParseError(
                ix,
                ParseError::InvalidEscape("\\b{...}".to_string()),
            ));
        }

        // \B{...} is not supported
        if bytes[ix + 1] == b'B' {
            return Err(Error::ParseError(
                ix,
                ParseError::InvalidEscape(format!("\\B{{{}}}", content)),
            ));
        }

        let expr = match content.as_str() {
            "start" => Expr::Assertion(Assertion::LeftWordBoundary),
            "end" => Expr::Assertion(Assertion::RightWordBoundary),
            "start-half" => Expr::Assertion(Assertion::LeftWordHalfBoundary),
            "end-half" => Expr::Assertion(Assertion::RightWordHalfBoundary),
            _ => {
                return Err(Error::ParseError(
                    ix,
                    ParseError::InvalidEscape(format!("\\b{{{}}}", content)),
                ));
            }
        };

        Ok((end_brace + 1, expr))
    }

    fn parse_class(&mut self, ix: usize) -> Result<(usize, Expr)> {
        let bytes = self.re.as_bytes();
        let mut ix = ix + 1; // skip opening '['
        let mut class = String::new();
        let mut nest = 1;
        let mut seen_negated_property = false;
        class.push('[');

        // Negated character class
        if bytes.get(ix) == Some(&b'^') {
            class.push('^');
            ix += 1;
        }

        // `]` does not have to be escaped after opening `[` or `[^`
        if bytes.get(ix) == Some(&b']') {
            class.push(']');
            ix += 1;
        }

        loop {
            if ix == self.re.len() {
                return Err(Error::ParseError(ix, ParseError::InvalidClass));
            }
            let end = match bytes[ix] {
                b'\\' => {
                    // We support more escapes than regex, so parse it ourselves before delegating.
                    let (end, expr) = self.parse_escape(ix, true)?;
                    match expr {
                        Expr::Literal { val, .. } => {
                            debug_assert_eq!(val.chars().count(), 1);
                            escape_into(&val, &mut class);
                        }
                        Expr::Delegate { inner, .. } => {
                            // Check if this is a negated property that needs && prefix
                            // A negated property is a nested character class that starts with [^ and contains
                            // Unicode property patterns or POSIX classes
                            let is_negated_prop = inner.starts_with("[^")
                                && (inner.contains(r"\p{")
                                    || inner.contains(r"\P{")
                                    || inner.contains("[:"));
                            if is_negated_prop {
                                if seen_negated_property {
                                    // This is not the first negated property, add &&
                                    class.push_str("&&");
                                }
                                // Mark that we've seen a negated property
                                seen_negated_property = true;
                            }
                            class.push_str(&inner);
                        }
                        _ => {
                            return Err(Error::ParseError(ix, ParseError::InvalidClass));
                        }
                    }
                    end
                }
                b'[' => {
                    nest += 1;
                    class.push('[');
                    let mut end = ix + 1;

                    // Handle `]` after `[` or `[^` in nested classes
                    // Check for negated character class
                    if bytes.get(end) == Some(&b'^') {
                        class.push('^');
                        end += 1;
                    }

                    // `]` does not have to be escaped after opening `[` or `[^`
                    if bytes.get(end) == Some(&b']') {
                        class.push(']');
                        end += 1;
                    }

                    end
                }
                b']' => {
                    nest -= 1;
                    class.push(']');
                    if nest == 0 {
                        break;
                    }
                    ix + 1
                }
                b => {
                    let end = ix + codepoint_len(b);
                    class.push_str(&self.re[ix..end]);
                    end
                }
            };
            ix = end;
        }
        let class = Expr::Delegate {
            inner: class,
            casei: self.flag(FLAG_CASEI),
        };
        let ix = ix + 1; // skip closing ']'
        Ok((ix, class))
    }

    fn parse_group(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        let depth = depth + 1;
        if depth >= MAX_RECURSION {
            return Err(Error::ParseError(ix, ParseError::RecursionExceeded));
        }
        let ix = self.optional_whitespace(ix + 1)?;
        let (la, skip) = if self.re[ix..].starts_with("?=") {
            (Some(LookAhead), 2)
        } else if self.re[ix..].starts_with("?!") {
            (Some(LookAheadNeg), 2)
        } else if self.re[ix..].starts_with("?<=") {
            (Some(LookBehind), 3)
        } else if self.re[ix..].starts_with("?<!") {
            (Some(LookBehindNeg), 3)
        } else if self.re[ix..].starts_with("?<") || self.re[ix..].starts_with("?'") {
            // Named capture group using Oniguruma syntax: (?<name>...) or (?'name'...)
            self.curr_group += 1;
            let (open, close) = if self.re[ix..].starts_with("?<") {
                ("<", ">")
            } else {
                ("'", "'")
            };
            if let Some(ParsedId {
                id,
                relative: None,
                skip,
            }) = parse_id(&self.re[ix + 1..], open, close, false)
            {
                self.named_groups.insert(id.to_string(), self.curr_group);
                (None, skip + 1)
            } else {
                return Err(Error::ParseError(ix, ParseError::InvalidGroupName));
            }
        } else if self.re[ix..].starts_with("?P<") {
            // Named capture group using Python syntax: (?P<name>...)
            self.curr_group += 1; // this is a capture group
            if let Some(ParsedId {
                id,
                relative: None,
                skip,
            }) = parse_id(&self.re[ix + 2..], "<", ">", false)
            {
                self.named_groups.insert(id.to_string(), self.curr_group);
                (None, skip + 2)
            } else {
                return Err(Error::ParseError(ix, ParseError::InvalidGroupName));
            }
        } else if self.re[ix..].starts_with("?P=") {
            // Backref using Python syntax: (?P=name)
            return self.parse_named_backref(ix + 3, "", ")", false);
        } else if self.re[ix..].starts_with("?~") {
            return self.parse_absent(ix + 1, depth);
        } else if self.re[ix..].starts_with("?>") {
            (None, 2)
        } else if self.re[ix..].starts_with("?(") {
            return self.parse_conditional(ix + 2, depth);
        } else if self.re[ix..].starts_with("?P>") {
            return self.parse_named_subroutine_call(ix + 3, "", ")", false);
        } else if self.re[ix..].starts_with("*") {
            return self.parse_backtracking_control_verb(ix);
        } else if self.re[ix..].starts_with('?') {
            return self.parse_flags(ix, depth);
        } else {
            self.curr_group += 1; // this is a capture group
            (None, 0)
        };
        let ix = ix + skip;
        let (ix, child) = self.parse_re(ix, depth)?;
        let ix = self.check_for_close_paren(ix)?;
        let result = match (la, skip) {
            (Some(la), _) => Expr::LookAround(Box::new(child), la),
            (None, 2) => Expr::AtomicGroup(Box::new(child)),
            _ => make_group(child),
        };
        Ok((ix, result))
    }

    fn check_for_close_paren(&self, ix: usize) -> Result<usize> {
        let ix = self.optional_whitespace(ix)?;
        if ix == self.re.len() {
            return Err(Error::ParseError(ix, ParseError::UnclosedOpenParen));
        } else if self.re.as_bytes()[ix] != b')' {
            return Err(Error::ParseError(
                ix,
                ParseError::GeneralParseError("expected close paren".to_string()),
            ));
        }
        Ok(ix + 1)
    }

    // ix points to `?` in `(?`
    fn parse_flags(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        let start = ix + 1;

        fn unknown_flag(re: &str, start: usize, end: usize) -> Error {
            let after_end = end + codepoint_len(re.as_bytes()[end]);
            let s = format!("(?{}", &re[start..after_end]);
            Error::ParseError(start, ParseError::UnknownFlag(s))
        }

        let mut ix = start;
        let mut neg = false;
        let oldflags = self.flags;
        loop {
            ix = self.optional_whitespace(ix)?;
            if ix == self.re.len() {
                return Err(Error::ParseError(ix, ParseError::UnclosedOpenParen));
            }
            let b = self.re.as_bytes()[ix];
            match b {
                b'i' => self.update_flag(FLAG_CASEI, neg),
                b'm' => self.update_flag(FLAG_MULTI, neg),
                b's' => self.update_flag(FLAG_DOTNL, neg),
                b'U' => self.update_flag(FLAG_SWAP_GREED, neg),
                b'x' => self.update_flag(FLAG_IGNORE_SPACE, neg),
                b'u' => {
                    if neg {
                        return Err(Error::ParseError(ix, ParseError::NonUnicodeUnsupported));
                    }
                }
                b'-' => {
                    if neg {
                        return Err(unknown_flag(self.re, start, ix));
                    }
                    neg = true;
                }
                b')' => {
                    if ix == start || neg && ix == start + 1 {
                        return Err(unknown_flag(self.re, start, ix));
                    }
                    return Ok((ix + 1, Expr::Empty));
                }
                b':' => {
                    if neg && ix == start + 1 {
                        return Err(unknown_flag(self.re, start, ix));
                    }
                    ix += 1;
                    let (ix, child) = self.parse_re(ix, depth)?;
                    if ix == self.re.len() {
                        return Err(Error::ParseError(ix, ParseError::UnclosedOpenParen));
                    } else if self.re.as_bytes()[ix] != b')' {
                        return Err(Error::ParseError(
                            ix,
                            ParseError::GeneralParseError("expected close paren".to_string()),
                        ));
                    };
                    self.flags = oldflags;
                    return Ok((ix + 1, child));
                }
                _ => return Err(unknown_flag(self.re, start, ix)),
            }
            ix += 1;
        }
    }

    // ix points to after the last ( in (?(
    fn parse_conditional(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        if ix >= self.re.len() {
            return Err(Error::ParseError(ix, ParseError::UnclosedOpenParen));
        }
        let bytes = self.re.as_bytes();
        // get the character after the open paren
        let b = bytes[ix];
        let (next, condition) = if b == b'\'' {
            self.parse_named_backref(ix, "'", "')", true)?
        } else if b == b'<' {
            self.parse_named_backref(ix, "<", ">)", true)?
        } else if b == b'+' || b == b'-' || b.is_ascii_digit() {
            self.parse_named_backref(ix, "", ")", true)?
        } else if b == b'*' {
            self.parse_backtracking_control_verb(ix)?
        } else {
            let (next, condition) = self.parse_re(ix, depth)?;
            (self.check_for_close_paren(next)?, condition)
        };
        let (end, child) = self.parse_re(next, depth)?;
        if end == next {
            // Backreference validity checker
            if let Expr::Backref { group, .. } = condition {
                let after = self.check_for_close_paren(end)?;
                return Ok((after, Expr::BackrefExistsCondition(group)));
            } else {
                return Err(Error::ParseError(
                    end,
                    ParseError::GeneralParseError(
                        "expected conditional to be a backreference or at least an expression for when the condition is true".to_string()
                    )
                ));
            }
        }
        let if_true: Expr;
        let mut if_false: Expr = Expr::Empty;
        if let Expr::Alt(mut alternatives) = child {
            // the truth branch will be the first alternative
            if_true = alternatives.remove(0);
            // if there is only one alternative left, take it out the Expr::Alt
            if alternatives.len() == 1 {
                if_false = alternatives.pop().expect("expected 2 alternatives");
            } else {
                // otherwise the remaining branches become the false branch
                if_false = Expr::Alt(alternatives);
            }
        } else {
            // there is only one branch - the truth branch. i.e. "if" without "else"
            if_true = child;
        }
        let inner_condition = if let Expr::Backref { group, .. } = condition {
            Expr::BackrefExistsCondition(group)
        } else {
            condition
        };

        let after = self.check_for_close_paren(end)?;
        Ok((
            after,
            if if_true == Expr::Empty && if_false == Expr::Empty {
                inner_condition
            } else {
                Expr::Conditional {
                    condition: Box::new(inner_condition),
                    true_branch: Box::new(if_true),
                    false_branch: Box::new(if_false),
                }
            },
        ))
    }

    // ix points to * after (
    fn parse_backtracking_control_verb(&mut self, ix: usize) -> Result<(usize, Expr)> {
        if self.re[ix..].starts_with("*FAIL)") {
            Ok((
                ix + "*FAIL)".len(),
                Expr::BacktrackingControlVerb(BacktrackingControlVerb::Fail),
            ))
        } else if self.re[ix..].starts_with("*F)") {
            Ok((
                ix + "*F)".len(),
                Expr::BacktrackingControlVerb(BacktrackingControlVerb::Fail),
            ))
        } else if self.re[ix..].starts_with("*ACCEPT)") {
            Ok((
                ix + "*ACCEPT)".len(),
                Expr::BacktrackingControlVerb(BacktrackingControlVerb::Accept),
            ))
        } else if self.re[ix..].starts_with("*COMMIT)") {
            Ok((
                ix + "*COMMIT)".len(),
                Expr::BacktrackingControlVerb(BacktrackingControlVerb::Commit),
            ))
        } else if self.re[ix..].starts_with("*SKIP)") {
            Ok((
                ix + "*SKIP)".len(),
                Expr::BacktrackingControlVerb(BacktrackingControlVerb::Skip),
            ))
        } else if self.re[ix..].starts_with("*PRUNE)") {
            Ok((
                ix + "*PRUNE)".len(),
                Expr::BacktrackingControlVerb(BacktrackingControlVerb::Prune),
            ))
        } else {
            Err(Error::ParseError(ix, ParseError::TargetNotRepeatable))
        }
    }

    // ix points to ~ after (?
    fn parse_absent(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        let ix = ix + 1; // skip `~` character
        if ix >= self.re.len() {
            return Err(Error::ParseError(ix, ParseError::UnclosedOpenParen));
        }

        if self.re[ix..].starts_with('|') {
            // (?~|...) - either absent expression, absent stopper, or range clear
            let ix = ix + 1; // skip `|` character

            // Parse the absent part (up to | or ))
            let (ix, absent) = self.parse_branch(ix, depth)?;

            if ix >= self.re.len() {
                return Err(Error::ParseError(ix, ParseError::UnclosedOpenParen));
            }

            if self.re.as_bytes()[ix] == b'|' {
                // (?~|absent|exp) - absent expression
                let ix = ix + 1; // skip |
                let (ix, exp) = self.parse_branch(ix, depth)?;
                let ix = self.check_for_close_paren(ix)?;
                Ok((
                    ix,
                    Expr::Absent(Absent::Expression {
                        absent: Box::new(absent),
                        exp: Box::new(exp),
                    }),
                ))
            } else if self.re.as_bytes()[ix] == b')' {
                // (?~|absent) - absent stopper, or (?~|) - range clear
                if absent == Expr::Empty {
                    Ok((ix + 1, Expr::Absent(Absent::Clear)))
                } else {
                    Ok((ix + 1, Expr::Absent(Absent::Stopper(Box::new(absent)))))
                }
            } else {
                Err(Error::ParseError(
                    ix,
                    ParseError::GeneralParseError(
                        "expected '|' or ')' in absent expression".to_string(),
                    ),
                ))
            }
        } else {
            // (?~absent) - absent repeater
            let (ix, absent) = self.parse_re(ix, depth)?;
            let ix = self.check_for_close_paren(ix)?;
            Ok((ix, Expr::Absent(Absent::Repeater(Box::new(absent)))))
        }
    }

    fn flag(&self, flag: u32) -> bool {
        let v = self.flags & flag;
        v == flag
    }

    fn update_flag(&mut self, flag: u32, neg: bool) {
        if neg {
            self.flags &= !flag;
        } else {
            self.flags |= flag;
        }
    }

    fn optional_whitespace(&self, mut ix: usize) -> Result<usize> {
        let bytes = self.re.as_bytes();
        loop {
            if ix == self.re.len() {
                return Ok(ix);
            }
            match bytes[ix] {
                b'#' if self.flag(FLAG_IGNORE_SPACE) => {
                    match bytes[ix..].iter().position(|&c| c == b'\n') {
                        Some(x) => ix += x + 1,
                        None => return Ok(self.re.len()),
                    }
                }
                b' ' | b'\r' | b'\n' | b'\t' if self.flag(FLAG_IGNORE_SPACE) => ix += 1,
                b'(' if bytes[ix..].starts_with(b"(?#") => {
                    ix += 3;
                    loop {
                        if ix >= self.re.len() {
                            return Err(Error::ParseError(ix, ParseError::UnclosedOpenParen));
                        }
                        match bytes[ix] {
                            b')' => {
                                ix += 1;
                                break;
                            }
                            b'\\' => ix += 2,
                            _ => ix += 1,
                        }
                    }
                }
                _ => return Ok(ix),
            }
        }
    }

    fn resolve_named_subroutine_calls(&mut self, expr: &mut Expr) {
        match expr {
            Expr::UnresolvedNamedSubroutineCall { name, .. } => {
                if let Some(group) = self.named_groups.get(name) {
                    *expr = Expr::SubroutineCall(*group);
                } else {
                    self.has_unresolved_subroutines = true;
                }
            }
            _ if !expr.is_leaf_node() => {
                // recursively resolve in inner expressions
                for child in expr.children_iter_mut() {
                    self.resolve_named_subroutine_calls(child);
                }
            }
            _ => {}
        }
    }
}

// return (ix, value)
pub(crate) fn parse_decimal(s: &str, ix: usize) -> Option<(usize, usize)> {
    let mut end = ix;
    while end < s.len() && s.as_bytes()[end].is_ascii_digit() {
        end += 1;
    }
    s[ix..end].parse::<usize>().ok().map(|val| (end, val))
}

#[derive(Debug, PartialEq)]
pub(crate) struct ParsedId<'a> {
    pub id: &'a str,
    pub relative: Option<isize>,
    pub skip: usize,
}

/// Attempts to parse an identifier, optionally followed by a relative number between the
/// specified opening and closing delimiters.  On success, returns
/// `Some((id, relative, skip))`, where `skip` is how much of the string was used.
pub(crate) fn parse_id<'a>(
    s: &'a str,
    open: &'_ str,
    close: &'_ str,
    allow_relative: bool,
) -> Option<ParsedId<'a>> {
    debug_assert!(!close.starts_with(is_id_char));

    if !s.starts_with(open) || s.len() <= open.len() + close.len() {
        return None;
    }

    let id_start = open.len();
    let mut iter = s[id_start..].char_indices().peekable();
    let after_id = iter.find(|(_, ch)| !is_id_char(*ch));

    let id_len = match after_id.map(|(i, _)| i) {
        Some(id_len) => id_len,
        None if close.is_empty() => s.len(),
        _ => 0,
    };

    let id_end = id_start + id_len;
    if id_len > 0 && s[id_end..].starts_with(close) {
        return Some(ParsedId {
            id: &s[id_start..id_end],
            relative: None,
            skip: id_end + close.len(),
        });
    } else if !allow_relative {
        return None;
    }
    let relative_sign = s.as_bytes()[id_end];
    if relative_sign == b'+' || relative_sign == b'-' {
        if let Some((end, relative_amount)) = parse_decimal(s, id_end + 1) {
            if s[end..].starts_with(close) {
                if relative_amount == 0 && id_len == 0 {
                    return None;
                }
                let relative_amount_signed = if relative_sign == b'-' {
                    -(relative_amount as isize)
                } else {
                    relative_amount as isize
                };
                return Some(ParsedId {
                    id: &s[id_start..id_end],
                    relative: Some(relative_amount_signed),
                    skip: end + close.len(),
                });
            }
        }
    }
    None
}

fn is_id_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn is_hex_digit(b: u8) -> bool {
    b.is_ascii_digit() || (b'a' <= (b | 32) && (b | 32) <= b'f')
}

pub(crate) fn make_literal(s: &str) -> Expr {
    make_literal_case_insensitive(s, false)
}

pub(crate) fn make_literal_case_insensitive(s: &str, case_insensitive: bool) -> Expr {
    Expr::Literal {
        val: String::from(s),
        casei: case_insensitive,
    }
}

pub(crate) fn make_group(inner: Expr) -> Expr {
    Expr::Group(Arc::new(inner))
}

fn remap_unicode_property_if_necessary(
    property_name: &str,
    unicode_flag: bool,
    in_class: bool,
) -> String {
    let (mut neg, prop) = if let Some(p) = property_name.strip_prefix(r"\p{") {
        (false, p)
    } else if let Some(p) = property_name.strip_prefix(r"\P{") {
        (true, p)
    } else {
        return String::from(property_name);
    };
    if let Some(mut p) = prop.strip_suffix('}') {
        // Check if the property name starts with ^ for negation (Oniguruma syntax)
        // e.g., \p{^Emoji} should become \P{emoji}
        if let Some(stripped) = p.strip_prefix('^') {
            neg = !neg;
            p = stripped;
        }
        let apply_wrap = |inner: &str, in_c: bool, n: bool| {
            if in_c {
                if n {
                    format!("[^{}]", inner)
                } else {
                    inner.to_string()
                }
            } else {
                if n {
                    format!("[^{}]", inner)
                } else {
                    format!("[{}]", inner)
                }
            }
        };
        match (p.to_lowercase().as_ref(), unicode_flag, in_class, neg) {
            ("alnum", true, in_c, n) => apply_wrap(r"\p{alpha}\p{digit}", in_c, n),
            ("alnum", false, in_c, n) => apply_wrap(r"[:alnum:]", in_c, n),
            ("blank", true, in_c, n) => apply_wrap(r"\p{Zs}\x09", in_c, n),
            ("blank", false, in_c, n) => apply_wrap(r"\t ", in_c, n),
            ("cntrl", true, in_c, n) => apply_wrap(r"\x00-\x1F\x7F-\x9F", in_c, n),
            ("cntrl", false, in_c, n) => apply_wrap(r"[:cntrl:]", in_c, n),
            (r"word", true, _, false) => r"\w".to_string(),
            (r"word", true, _, true) => r"\W".to_string(),
            (r"word", false, false, false) => r"[_[:alnum:]]".to_string(),
            (r"word", false, false, true) => r"[^_[:alnum:]]".to_string(),
            (r"word", false, true, false) => r"_[:alnum:]".to_string(),
            (r"word", false, true, true) => r"[^_[:alnum:]]".to_string(),
            // graph - graphical/visible characters (excludes whitespace, control chars, etc)
            // For Unicode mode, we exclude White_Space and all Other category (\p{C})
            // Note: When used inside a character class, we return the full negated class
            // because we can't easily represent a negation as positive content
            (r"graph", true, _, false) => r"[^\p{White_Space}\p{C}]".to_string(),
            (r"graph", true, _, true) => r"[\p{White_Space}\p{C}]".to_string(),
            (r"graph", false, false, false) => r"[[:graph:]]".to_string(),
            (r"graph", false, false, true) => r"[^[:graph:]]".to_string(),
            (r"graph", false, true, false) => r"[:graph:]".to_string(),
            (r"graph", false, true, true) => r"[^[:graph:]]".to_string(),
            // print - printable characters (graph + space separator)
            // For Unicode mode: exclude Other category and non-space whitespace
            // Note: When used inside a character class, we return the full negated class
            (r"print", true, _, false) => r"[^\p{C}\t\n\v\f\r]".to_string(),
            (r"print", true, _, true) => r"[\p{C}\t\n\v\f\r]".to_string(),
            (r"print", false, false, false) => r"[[:print:]]".to_string(),
            (r"print", false, false, true) => r"[^[:print:]]".to_string(),
            (r"print", false, true, false) => r"[:print:]".to_string(),
            (r"print", false, true, true) => r"[^[:print:]]".to_string(),
            // cs - surrogates (only applies to UTF-16, never matches in UTF-8)
            ("cs", _, _, false) => r"\P{any}".to_string(),
            ("cs", _, _, true) => r"\p{any}".to_string(),
            // For other properties, convert to lowercase and reconstruct
            _ => {
                let lower = p.to_lowercase();
                if neg {
                    format!(r"\P{{{}}}", lower)
                } else {
                    format!(r"\p{{{}}}", lower)
                }
            }
        }
    } else {
        String::from(property_name)
    }
}

#[cfg(test)]
mod tests {
    use alloc::boxed::Box;
    use alloc::string::{String, ToString};
    use alloc::{format, vec};

    use crate::parse::{
        make_group, make_literal, make_literal_case_insensitive, parse_id,
        remap_unicode_property_if_necessary,
    };
    use crate::{Absent, Assertion, BacktrackingControlVerb, Expr};
    use crate::{LookAround::*, RegexOptions, SyntaxConfig};

    fn p(s: &str) -> Expr {
        Expr::parse_tree(s).unwrap().expr
    }

    fn parse_oniguruma(s: &str) -> crate::Result<Expr> {
        let mut options = RegexOptions::default();
        options.oniguruma_mode = true;
        Expr::parse_tree_with_flags(s, options.compute_flags()).map(|tree| tree.expr)
    }

    #[cfg_attr(feature = "track_caller", track_caller)]
    fn fail(s: &str) {
        assert!(
            Expr::parse_tree(s).is_err(),
            "Expected parse error, but was: {:?}",
            Expr::parse_tree(s)
        );
    }

    #[cfg_attr(feature = "track_caller", track_caller)]
    fn assert_error(re: &str, expected_error: &str) {
        let result = Expr::parse_tree(re);
        assert!(
            result.is_err(),
            "Expected parse error, but was: {:?}",
            result
        );
        assert_eq!(&format!("{}", result.err().unwrap()), expected_error);
    }

    #[cfg_attr(feature = "track_caller", track_caller)]
    fn assert_error_oniguruma(re: &str, expected_error: &str) {
        let result = parse_oniguruma(re);
        assert!(
            result.is_err(),
            "Expected parse error in Oniguruma mode, but was: {:?}",
            result
        );
        assert_eq!(&format!("{}", result.err().unwrap()), expected_error);
    }

    #[test]
    fn empty() {
        assert_eq!(p(""), Expr::Empty);
    }

    #[test]
    fn any() {
        assert_eq!(p("."), Expr::Any { newline: false });
        assert_eq!(p("(?s:.)"), Expr::Any { newline: true });
    }

    #[test]
    fn start_text() {
        assert_eq!(p("^"), Expr::Assertion(Assertion::StartText));
    }

    #[test]
    fn end_text() {
        assert_eq!(p("$"), Expr::Assertion(Assertion::EndText));
    }

    #[test]
    fn end_text_before_empty_lines() {
        assert_eq!(
            p("\\Z"),
            Expr::Assertion(Assertion::EndTextIgnoreTrailingNewlines)
        );
    }

    #[test]
    fn literal() {
        assert_eq!(p("a"), make_literal("a"));
    }

    #[test]
    fn literal_special() {
        assert_eq!(p("}"), make_literal("}"));
        assert_eq!(p("]"), make_literal("]"));
    }

    #[test]
    fn parse_id_test() {
        use crate::parse::ParsedId;
        fn create_id(id: &str, relative: Option<isize>, skip: usize) -> Option<ParsedId<'_>> {
            Some(ParsedId { id, relative, skip })
        }
        assert_eq!(parse_id("foo.", "", "", true), create_id("foo", None, 3));
        assert_eq!(parse_id("1.", "", "", true), create_id("1", None, 1));
        assert_eq!(parse_id("{foo}", "{", "}", true), create_id("foo", None, 5));
        assert_eq!(parse_id("{foo.", "{", "}", true), None);
        assert_eq!(parse_id("{foo", "{", "}", true), None);
        assert_eq!(parse_id("{}", "{", "}", true), None);
        assert_eq!(parse_id("", "", "", true), None);
        assert_eq!(parse_id("{-1}", "{", "}", true), create_id("", Some(-1), 4));
        assert_eq!(parse_id("{-1}", "{", "}", false), None);
        assert_eq!(parse_id("{-a}", "{", "}", true), None);
        assert_eq!(parse_id("{-a}", "{", "}", false), None);
        assert_eq!(parse_id("{+a}", "{", "}", false), None);
        assert_eq!(parse_id("+a", "", "", false), None);
        assert_eq!(parse_id("-a", "", "", false), None);
        assert_eq!(parse_id("2+a", "", "", false), create_id("2", None, 1));
        assert_eq!(parse_id("2-a", "", "", false), create_id("2", None, 1));

        assert_eq!(parse_id("<+1>", "<", ">", true), create_id("", Some(1), 4));
        assert_eq!(parse_id("<-3>", "<", ">", true), create_id("", Some(-3), 4));
        assert_eq!(
            parse_id("<n+1>", "<", ">", true),
            create_id("n", Some(1), 5)
        );
        assert_eq!(
            parse_id("<n-1>", "<", ">", true),
            create_id("n", Some(-1), 5)
        );
        assert_eq!(parse_id("<>", "<", ">", true), None);
        assert_eq!(parse_id("<", "<", ">", true), None);
        assert_eq!(parse_id("<+0>", "<", ">", true), None);
        assert_eq!(parse_id("<-0>", "<", ">", true), None);
        assert_eq!(
            parse_id("<n+0>", "<", ">", true),
            create_id("n", Some(0), 5)
        );
        assert_eq!(
            parse_id("<n-0>", "<", ">", true),
            create_id("n", Some(0), 5)
        );
        assert_eq!(
            parse_id("<2-0>", "<", ">", true),
            create_id("2", Some(0), 5)
        );
        assert_eq!(
            parse_id("<2+0>", "<", ">", true),
            create_id("2", Some(0), 5)
        );
        assert_eq!(
            parse_id("<2+1>", "<", ">", true),
            create_id("2", Some(1), 5)
        );
        assert_eq!(
            parse_id("<2-1>", "<", ">", true),
            create_id("2", Some(-1), 5)
        );
    }

    #[test]
    fn literal_unescaped_opening_curly() {
        // `{` in position where quantifier is not allowed results in literal `{`
        assert_eq!(p("{"), make_literal("{"));
        assert_eq!(p("({)"), make_group(make_literal("{"),));
        assert_eq!(
            p("a|{"),
            Expr::Alt(vec![make_literal("a"), make_literal("{"),])
        );
        assert_eq!(
            p("{{2}"),
            Expr::Repeat {
                child: Box::new(make_literal("{")),
                lo: 2,
                hi: 2,
                greedy: true
            }
        );
    }

    #[test]
    fn literal_escape() {
        assert_eq!(p("\\'"), make_literal("'"));
        assert_eq!(p("\\\""), make_literal("\""));
        assert_eq!(p("\\ "), make_literal(" "));
        assert_eq!(p("\\xA0"), make_literal("\u{A0}"));
        assert_eq!(p("\\x{1F4A9}"), make_literal("\u{1F4A9}"));
        assert_eq!(p("\\x{000000B7}"), make_literal("\u{B7}"));
        assert_eq!(p("\\u21D2"), make_literal("\u{21D2}"));
        assert_eq!(p("\\u{21D2}"), make_literal("\u{21D2}"));
        assert_eq!(p("\\u21D2x"), p("\u{21D2}x"));
        assert_eq!(p("\\U0001F60A"), make_literal("\u{1F60A}"));
        assert_eq!(p("\\U{0001F60A}"), make_literal("\u{1F60A}"));
    }

    #[test]
    fn hex_escape() {
        assert_eq!(
            p("\\h"),
            Expr::Delegate {
                inner: String::from("[0-9A-Fa-f]"),
                casei: false
            }
        );
        assert_eq!(
            p("\\H"),
            Expr::Delegate {
                inner: String::from("[^0-9A-Fa-f]"),
                casei: false
            }
        );
    }

    #[test]
    fn invalid_escape() {
        assert_error(
            "\\",
            "Parsing error at position 0: Backslash without following character",
        );
        assert_error("\\q", "Parsing error at position 0: Invalid escape: \\q");
        assert_error("\\u", "Parsing error at position 2: Invalid hex escape");
        assert_error("\\U", "Parsing error at position 2: Invalid hex escape");
        assert_error("\\x", "Parsing error at position 2: Invalid hex escape");
        assert_error("\\xAG", "Parsing error at position 2: Invalid hex escape");
        assert_error("\\xA", "Parsing error at position 2: Invalid hex escape");
        assert_error("\\x{}", "Parsing error at position 2: Invalid hex escape");
        assert_error("\\x{AG}", "Parsing error at position 2: Invalid hex escape");
        assert_error("\\x{42", "Parsing error at position 2: Invalid hex escape");
        assert_error(
            "\\x{D800}",
            "Parsing error at position 2: Invalid codepoint for hex or unicode escape",
        );
        assert_error(
            "\\x{110000}",
            "Parsing error at position 2: Invalid codepoint for hex or unicode escape",
        );
        assert_error("\\u123", "Parsing error at position 2: Invalid hex escape");
        assert_error("\\u123x", "Parsing error at position 2: Invalid hex escape");
        assert_error("\\u{}", "Parsing error at position 2: Invalid hex escape");
        assert_error(
            "\\U1234567",
            "Parsing error at position 2: Invalid hex escape",
        );
        assert_error("\\U{}", "Parsing error at position 2: Invalid hex escape");
    }

    #[test]
    fn concat() {
        assert_eq!(
            p("ab"),
            Expr::Concat(vec![make_literal("a"), make_literal("b"),])
        );
    }

    #[test]
    fn alt() {
        assert_eq!(
            p("a|b"),
            Expr::Alt(vec![make_literal("a"), make_literal("b"),])
        );
    }

    #[test]
    fn group() {
        assert_eq!(p("(a)"), make_group(make_literal("a"),));
    }

    #[test]
    fn named_group() {
        assert_eq!(p("(?'name'a)"), make_group(make_literal("a"),));
        assert_eq!(p("(?<name>a)"), make_group(make_literal("a"),));
    }

    #[test]
    fn group_repeat() {
        assert_eq!(
            p("(a){2}"),
            Expr::Repeat {
                child: Box::new(make_group(make_literal("a"))),
                lo: 2,
                hi: 2,
                greedy: true
            }
        );
    }

    #[test]
    fn repeat() {
        assert_eq!(
            p("a{2,42}"),
            Expr::Repeat {
                child: Box::new(make_literal("a")),
                lo: 2,
                hi: 42,
                greedy: true
            }
        );
        assert_eq!(
            p("a{2,}"),
            Expr::Repeat {
                child: Box::new(make_literal("a")),
                lo: 2,
                hi: usize::MAX,
                greedy: true
            }
        );
        assert_eq!(
            p("a{2}"),
            Expr::Repeat {
                child: Box::new(make_literal("a")),
                lo: 2,
                hi: 2,
                greedy: true
            }
        );
        assert_eq!(
            p("a{,2}"),
            Expr::Repeat {
                child: Box::new(make_literal("a")),
                lo: 0,
                hi: 2,
                greedy: true
            }
        );

        assert_eq!(
            p("a{2,42}?"),
            Expr::Repeat {
                child: Box::new(make_literal("a")),
                lo: 2,
                hi: 42,
                greedy: false
            }
        );
        assert_eq!(
            p("a{2,}?"),
            Expr::Repeat {
                child: Box::new(make_literal("a")),
                lo: 2,
                hi: usize::MAX,
                greedy: false
            }
        );
        assert_eq!(
            p("a{2}?"),
            Expr::Repeat {
                child: Box::new(make_literal("a")),
                lo: 2,
                hi: 2,
                greedy: false
            }
        );
        assert_eq!(
            p("a{,2}?"),
            Expr::Repeat {
                child: Box::new(make_literal("a")),
                lo: 0,
                hi: 2,
                greedy: false
            }
        );
    }

    #[test]
    fn invalid_repeat() {
        // Invalid repeat syntax results in literal
        assert_eq!(
            p("a{"),
            Expr::Concat(vec![make_literal("a"), make_literal("{"),])
        );
        assert_eq!(
            p("a{6"),
            Expr::Concat(vec![
                make_literal("a"),
                make_literal("{"),
                make_literal("6"),
            ])
        );
        assert_eq!(
            p("a{6,"),
            Expr::Concat(vec![
                make_literal("a"),
                make_literal("{"),
                make_literal("6"),
                make_literal(","),
            ])
        );
        assert_eq!(
            p("a{1,A}"),
            Expr::Concat(vec![
                make_literal("a"),
                make_literal("{"),
                make_literal("1"),
                make_literal(","),
                make_literal("A"),
                make_literal("}"),
            ])
        );
        assert_eq!(
            p("a{1,2A}"),
            Expr::Concat(vec![
                make_literal("a"),
                make_literal("{"),
                make_literal("1"),
                make_literal(","),
                make_literal("2"),
                make_literal("A"),
                make_literal("}"),
            ])
        );
    }

    #[test]
    fn quantifiers_on_assertions_in_regex_mode() {
        assert_eq!(
            p(r"\b{1}"),
            Expr::Repeat {
                child: Box::new(Expr::Assertion(Assertion::WordBoundary)),
                lo: 1,
                hi: 1,
                greedy: true
            }
        );
        assert_eq!(
            p(r"\B{2}"),
            Expr::Repeat {
                child: Box::new(Expr::Assertion(Assertion::NotWordBoundary)),
                lo: 2,
                hi: 2,
                greedy: true
            }
        );
        assert_eq!(
            p(r"^{3}"),
            Expr::Repeat {
                child: Box::new(Expr::Assertion(Assertion::StartText)),
                lo: 3,
                hi: 3,
                greedy: true
            }
        );
        assert_eq!(
            p(r"${1,5}"),
            Expr::Repeat {
                child: Box::new(Expr::Assertion(Assertion::EndText)),
                lo: 1,
                hi: 5,
                greedy: true
            }
        );
        assert_eq!(
            p(r"\A*"),
            Expr::Repeat {
                child: Box::new(Expr::Assertion(Assertion::StartText)),
                lo: 0,
                hi: usize::MAX,
                greedy: true
            }
        );
        assert_eq!(
            p(r"\z+"),
            Expr::Repeat {
                child: Box::new(Expr::Assertion(Assertion::EndText)),
                lo: 1,
                hi: usize::MAX,
                greedy: true
            }
        );
        assert_eq!(
            p(r"^?"),
            Expr::Repeat {
                child: Box::new(Expr::Assertion(Assertion::StartText)),
                lo: 0,
                hi: 1,
                greedy: true
            }
        );
        assert_eq!(
            p(r"${2}"),
            Expr::Repeat {
                child: Box::new(Expr::Assertion(Assertion::EndText)),
                lo: 2,
                hi: 2,
                greedy: true
            }
        );
        assert_eq!(
            p(r"(?m)^?"),
            Expr::Repeat {
                child: Box::new(Expr::Assertion(Assertion::StartLine { crlf: false })),
                lo: 0,
                hi: 1,
                greedy: true
            }
        );
        assert_eq!(
            p(r"(?m)${2}"),
            Expr::Repeat {
                child: Box::new(Expr::Assertion(Assertion::EndLine { crlf: false })),
                lo: 2,
                hi: 2,
                greedy: true
            }
        );
        assert_eq!(
            p(r"\b+"),
            Expr::Repeat {
                child: Box::new(Expr::Assertion(Assertion::WordBoundary)),
                lo: 1,
                hi: usize::MAX,
                greedy: true
            }
        );
    }

    #[test]
    fn delegate_zero() {
        assert_eq!(p("\\b"), Expr::Assertion(Assertion::WordBoundary),);
        assert_eq!(p("\\B"), Expr::Assertion(Assertion::NotWordBoundary),);
    }

    #[test]
    fn delegate_named_group() {
        assert_eq!(
            p("\\p{Greek}"),
            Expr::Delegate {
                inner: String::from("\\p{greek}"),
                casei: false
            }
        );
        assert_eq!(
            p("\\pL"),
            Expr::Delegate {
                inner: String::from("\\pL"),
                casei: false
            }
        );
        assert_eq!(
            p("\\P{Greek}"),
            Expr::Delegate {
                inner: String::from("\\P{greek}"),
                casei: false
            }
        );
        assert_eq!(
            p("\\PL"),
            Expr::Delegate {
                inner: String::from("\\PL"),
                casei: false
            }
        );
        assert_eq!(
            p("(?i)\\p{Ll}"),
            Expr::Delegate {
                inner: String::from("\\p{ll}"),
                casei: true
            }
        );
    }

    #[test]
    fn backref() {
        assert_eq!(
            p("(.)\\1"),
            Expr::Concat(vec![
                make_group(Expr::Any { newline: false }),
                Expr::Backref {
                    group: 1,
                    casei: false,
                },
            ])
        );
    }

    #[test]
    fn named_backref() {
        assert_eq!(
            p("(?<i>.)\\k<i>"),
            Expr::Concat(vec![
                make_group(Expr::Any { newline: false }),
                Expr::Backref {
                    group: 1,
                    casei: false,
                },
            ])
        );
    }

    #[test]
    fn relative_backref() {
        assert_eq!(
            p(r"(a)(.)\k<-1>"),
            Expr::Concat(vec![
                make_group(make_literal("a")),
                make_group(Expr::Any { newline: false }),
                Expr::Backref {
                    group: 2,
                    casei: false,
                },
            ])
        );

        assert_eq!(
            p(r"(a)\k<+1>(.)"),
            Expr::Concat(vec![
                make_group(make_literal("a")),
                Expr::Backref {
                    group: 2,
                    casei: false,
                },
                make_group(Expr::Any { newline: false }),
            ])
        );

        fail("(?P<->.)");
        fail("(.)(?P=-)");
        fail(r"(a)\k<-0>(.)");
        fail(r"(a)\k<+0>(.)");
        fail(r"(a)\k<+>(.)");
        fail(r"(a)\k<->(.)");
        fail(r"(a)\k<>(.)");
    }

    #[test]
    fn relative_backref_with_recursion_level() {
        assert_eq!(
            p(r"()\k<1+3>"),
            Expr::Concat(vec![
                make_group(Expr::Empty),
                Expr::BackrefWithRelativeRecursionLevel {
                    group: 1,
                    relative_level: 3,
                    casei: false,
                },
            ]),
        );

        assert_eq!(
            p(r"()\k<1-0>"),
            Expr::Concat(vec![
                make_group(Expr::Empty),
                Expr::BackrefWithRelativeRecursionLevel {
                    group: 1,
                    relative_level: 0,
                    casei: false,
                },
            ]),
        );

        assert_eq!(
            p(r"(?<n>)\k<n+3>"),
            Expr::Concat(vec![
                make_group(Expr::Empty),
                Expr::BackrefWithRelativeRecursionLevel {
                    group: 1,
                    relative_level: 3,
                    casei: false,
                },
            ]),
        );

        assert_eq!(
            p(r"(?<n>)\k<n-3>"),
            Expr::Concat(vec![
                make_group(Expr::Empty),
                Expr::BackrefWithRelativeRecursionLevel {
                    group: 1,
                    relative_level: -3,
                    casei: false,
                }
            ]),
        );

        assert_eq!(
            p(r"\A(?<a>|.|(?:(?<b>.)\g<a>\k<b+0>))\z"),
            Expr::Concat(vec![
                Expr::Assertion(Assertion::StartText),
                make_group(Expr::Alt(vec![
                    Expr::Empty,
                    Expr::Any { newline: false },
                    Expr::Concat(vec![
                        make_group(Expr::Any { newline: false }),
                        Expr::SubroutineCall(1),
                        Expr::BackrefWithRelativeRecursionLevel {
                            group: 2,
                            relative_level: 0,
                            casei: false,
                        },
                    ])
                ])),
                Expr::Assertion(Assertion::EndText)
            ]),
        );
    }

    #[test]
    fn relative_subroutine_call() {
        assert_eq!(
            p(r"(a)(.)\g<-1>"),
            Expr::Concat(vec![
                make_group(make_literal("a")),
                make_group(Expr::Any { newline: false }),
                Expr::SubroutineCall(2),
            ])
        );

        assert_eq!(
            p(r"(a)\g<+1>(.)"),
            Expr::Concat(vec![
                make_group(make_literal("a")),
                Expr::SubroutineCall(2),
                make_group(Expr::Any { newline: false }),
            ])
        );

        fail(r"(a)\g<-0>(.)");
        fail(r"(a)\g<+0>(.)");
        fail(r"(a)\g<+>(.)");
        fail(r"(a)\g<->(.)");
        fail(r"(a)\g<>(.)");
    }

    #[test]
    fn lookaround() {
        assert_eq!(
            p("(?=a)"),
            Expr::LookAround(Box::new(make_literal("a")), LookAhead)
        );
        assert_eq!(
            p("(?!a)"),
            Expr::LookAround(Box::new(make_literal("a")), LookAheadNeg)
        );
        assert_eq!(
            p("(?<=a)"),
            Expr::LookAround(Box::new(make_literal("a")), LookBehind)
        );
        assert_eq!(
            p("(?<!a)"),
            Expr::LookAround(Box::new(make_literal("a")), LookBehindNeg)
        );
    }

    #[test]
    fn shy_group() {
        assert_eq!(
            p("(?:ab)c"),
            Expr::Concat(vec![
                Expr::Concat(vec![make_literal("a"), make_literal("b"),]),
                make_literal("c"),
            ])
        );
    }

    #[test]
    fn flag_state() {
        assert_eq!(p("(?s)."), Expr::Any { newline: true });
        assert_eq!(p("(?s:(?-s:.))"), Expr::Any { newline: false });
        assert_eq!(
            p("(?s:.)."),
            Expr::Concat(vec![
                Expr::Any { newline: true },
                Expr::Any { newline: false },
            ])
        );
        assert_eq!(
            p("(?:(?s).)."),
            Expr::Concat(vec![
                Expr::Any { newline: true },
                Expr::Any { newline: false },
            ])
        );
    }

    #[test]
    fn flag_multiline() {
        assert_eq!(p("^"), Expr::Assertion(Assertion::StartText));
        assert_eq!(
            p("(?m:^)"),
            Expr::Assertion(Assertion::StartLine { crlf: false })
        );
        assert_eq!(p("$"), Expr::Assertion(Assertion::EndText));
        assert_eq!(
            p("(?m:$)"),
            Expr::Assertion(Assertion::EndLine { crlf: false })
        );
    }

    #[test]
    fn flag_swap_greed() {
        assert_eq!(p("a*"), p("(?U:a*?)"));
        assert_eq!(p("a*?"), p("(?U:a*)"));
    }

    #[test]
    fn invalid_flags() {
        assert!(Expr::parse_tree("(?").is_err());
        assert!(Expr::parse_tree("(?)").is_err());
        assert!(Expr::parse_tree("(?-)").is_err());
        assert!(Expr::parse_tree("(?-:a)").is_err());
        assert!(Expr::parse_tree("(?q:a)").is_err());
    }

    #[test]
    fn lifetime() {
        assert_eq!(
            p("\\'[a-zA-Z_][a-zA-Z0-9_]*(?!\\')\\b"),
            Expr::Concat(vec![
                make_literal("'"),
                Expr::Delegate {
                    inner: String::from("[a-zA-Z_]"),
                    casei: false
                },
                Expr::Repeat {
                    child: Box::new(Expr::Delegate {
                        inner: String::from("[a-zA-Z0-9_]"),
                        casei: false
                    }),
                    lo: 0,
                    hi: usize::MAX,
                    greedy: true
                },
                Expr::LookAround(Box::new(make_literal("'")), LookAheadNeg),
                Expr::Assertion(Assertion::WordBoundary),
            ])
        );
    }

    #[test]
    fn ignore_whitespace() {
        assert_eq!(p("(?x: )"), p(""));
        assert_eq!(p("(?x) | "), p("|"));
        assert_eq!(p("(?x: a )"), p("a"));
        assert_eq!(p("(?x: a # ) bobby tables\n b )"), p("ab"));
        assert_eq!(p("(?x: a | b )"), p("a|b"));
        assert_eq!(p("(?x: ( a b ) )"), p("(ab)"));
        assert_eq!(p("(?x: a + )"), p("a+"));
        assert_eq!(p("(?x: a {2} )"), p("a{2}"));
        assert_eq!(p("(?x: a { 2 } )"), p("a{2}"));
        assert_eq!(p("(?x: a { 2 , } )"), p("a{2,}"));
        assert_eq!(p("(?x: a { , 2 } )"), p("a{,2}"));
        assert_eq!(p("(?x: a { 2 , 3 } )"), p("a{2,3}"));
        assert_eq!(p("(?x: a { 2 , 3 } ? )"), p("a{2,3}?"));
        assert_eq!(p("(?x: ( ? i : . ) )"), p("(?i:.)"));
        assert_eq!(p("(?x: ( ?= a ) )"), p("(?=a)"));
        assert_eq!(p("(?x: [ ] )"), p("[ ]"));
        assert_eq!(p("(?x: [ ^] )"), p("[ ^]"));
        assert_eq!(p("(?x: [a - z] )"), p("[a - z]"));
        assert_eq!(p("(?x: [ \\] \\\\] )"), p("[ \\] \\\\]"));
        assert_eq!(p("(?x: a\\ b )"), p("a b"));
        assert_eq!(p("(?x: a (?-x:#) b )"), p("a#b"));
        assert_eq!(p("(?x: a (?# b))c"), p("ac"));
        assert_eq!(p("(?x: \\b { s t a r t} a )"), p("\\b{start}a"));
        assert_eq!(p("(?x: \\b {end} )"), p("\\b{end}"));
        assert_eq!(p("(?x: \\b { start-half } )"), p("\\b{start-half}"));
        assert_eq!(p("(?x: \\b { e n d - h a l f } )"), p("\\b{end-half}"));
        assert_eq!(p("(?x: \\b{s\nt\ra\trt} a)"), p("\\b{start}a"));
        assert_eq!(p("(?x: \\u { 0 0 3 2} a)"), p("\\u{0032}a"));
        assert_eq!(p("(?x: \\U { 0001 F600 } a)"), p("\\U{0001F600}a"));
        assert_eq!(p("(?x: \\x { 3 2} a)"), p("\\x{32}a"));
        assert_eq!(p("(?x: \\b { s t a # x\nr t} a )"), p("\\b{start}a"));
        assert_eq!(p("(?x: \\b { s t a (?# x)r t} a )"), p("\\b{start}a"));
        assert_eq!(p("(?x: \\u { 0 0 # x\n3 2} a)"), p("\\u{0032}a"));
        assert_eq!(p("(?x: \\u { 0 0 (?# x)3 2} a)"), p("\\u{0032}a"));
    }

    #[test]
    fn comments() {
        assert_eq!(p(r"ab(?# comment)"), p("ab"));
        assert_eq!(p(r"ab(?#)"), p("ab"));
        assert_eq!(p(r"(?# comment 1)(?# comment 2)ab"), p("ab"));
        assert_eq!(p(r"ab(?# comment \))c"), p("abc"));
        assert_eq!(p(r"ab(?# comment \\)c"), p("abc"));
        assert_eq!(p(r"ab(?# comment ()c"), p("abc"));
        assert_eq!(p(r"ab(?# comment)*"), p("ab*"));
        fail(r"ab(?# comment");
        fail(r"ab(?# comment\");
    }

    #[test]
    fn atomic_group() {
        assert_eq!(p("(?>a)"), Expr::AtomicGroup(Box::new(make_literal("a"))));
    }

    #[test]
    fn possessive() {
        assert_eq!(
            p("a++"),
            Expr::AtomicGroup(Box::new(Expr::Repeat {
                child: Box::new(make_literal("a")),
                lo: 1,
                hi: usize::MAX,
                greedy: true
            }))
        );
        assert_eq!(
            p("a*+"),
            Expr::AtomicGroup(Box::new(Expr::Repeat {
                child: Box::new(make_literal("a")),
                lo: 0,
                hi: usize::MAX,
                greedy: true
            }))
        );
        assert_eq!(
            p("a?+"),
            Expr::AtomicGroup(Box::new(Expr::Repeat {
                child: Box::new(make_literal("a")),
                lo: 0,
                hi: 1,
                greedy: true
            }))
        );
    }

    #[test]
    fn invalid_backref() {
        // only syntactic tests; see similar test in analyze module
        assert_error(
            r".\18446744073709551616",
            "Parsing error at position 2: Invalid back reference",
        ); // unreasonably large number
        assert_error(r".\c", "Parsing error at position 1: Invalid escape: \\c"); // not decimal

        assert_error(
            r"a\1",
            "Parsing error at position 2: Invalid back reference",
        ); // invalid back reference according to regex length - not long enough to contain that many paren pairs
        assert_error(
            r"(a)\2",
            "Parsing error at position 4: Invalid back reference",
        ); // invalid back reference according to regex length - not long enough to contain that many paren pairs
    }

    #[test]
    fn invalid_group_name_backref() {
        assert_error(
            "\\k<id>(?<id>.)",
            "Parsing error at position 2: Invalid group name in back reference: id",
        );
    }

    #[test]
    fn named_backref_only() {
        assert_error("(?<id>.)\\1", "Error compiling regex: Numbered backref/call not allowed because named group was used, use a named backref instead");
        assert_error("(a)\\1(?<name>b)", "Error compiling regex: Numbered backref/call not allowed because named group was used, use a named backref instead");
    }

    #[test]
    fn invalid_group_name() {
        assert_error(
            "(?<id)",
            "Parsing error at position 1: Could not parse group name",
        );
        assert_error(
            "(?<>)",
            "Parsing error at position 1: Could not parse group name",
        );
        assert_error(
            "(?<#>)",
            "Parsing error at position 1: Could not parse group name",
        );
        assert_error(
            "\\kxxx<id>",
            "Parsing error at position 2: Could not parse group name",
        );
        // "-" can only be used after a name for relative recursion level, so must be followed by a number
        assert_error(
            "\\k<id-withdash>",
            "Parsing error at position 2: Could not parse group name",
        );
        assert_error(
            "\\k<-id>",
            "Parsing error at position 2: Could not parse group name",
        );
    }

    #[test]
    fn unknown_flag() {
        assert_error(
            "(?-:a)",
            "Parsing error at position 2: Unknown group flag: (?-:",
        );
        assert_error(
            "(?)",
            "Parsing error at position 2: Unknown group flag: (?)",
        );
        assert_error(
            "(?--)",
            "Parsing error at position 2: Unknown group flag: (?--",
        );
        // Check that we don't split on char boundary
        assert_error(
            "(?\u{1F60A})",
            "Parsing error at position 2: Unknown group flag: (?\u{1F60A}",
        );
    }

    #[test]
    fn no_quantifiers_on_lookarounds() {
        assert_error(
            "(?=hello)+",
            "Parsing error at position 9: Target of repeat operator is invalid",
        );
        assert_error(
            "(?<!hello)*",
            "Parsing error at position 10: Target of repeat operator is invalid",
        );
        assert_error(
            "(?<=hello){2,3}",
            "Parsing error at position 14: Target of repeat operator is invalid",
        );
        assert_error(
            "(?!hello)?",
            "Parsing error at position 9: Target of repeat operator is invalid",
        );
        assert_error(
            "(a|b|?)",
            "Parsing error at position 5: Target of repeat operator is invalid",
        );
    }

    #[test]
    fn no_quantifiers_on_assertions_in_oniguruma_mode() {
        assert_error_oniguruma(
            r"\b{1}",
            "Parsing error at position 4: Target of repeat operator is invalid",
        );
        assert_error_oniguruma(
            r"\B{2}",
            "Parsing error at position 4: Target of repeat operator is invalid",
        );
        assert_error_oniguruma(
            r"^{3}",
            "Parsing error at position 3: Target of repeat operator is invalid",
        );
        assert_error_oniguruma(
            r"${1,5}",
            "Parsing error at position 5: Target of repeat operator is invalid",
        );
        assert_error_oniguruma(
            r"\A*",
            "Parsing error at position 2: Target of repeat operator is invalid",
        );
        assert_error_oniguruma(
            r"\z+",
            "Parsing error at position 2: Target of repeat operator is invalid",
        );
        assert_error_oniguruma(
            r"^?",
            "Parsing error at position 1: Target of repeat operator is invalid",
        );
        assert_error_oniguruma(
            r"${2}",
            "Parsing error at position 3: Target of repeat operator is invalid",
        );
        assert_error_oniguruma(
            r"(?m)^?",
            "Parsing error at position 5: Target of repeat operator is invalid",
        );
        assert_error_oniguruma(
            r"(?m)${2}",
            "Parsing error at position 7: Target of repeat operator is invalid",
        );
        assert_error_oniguruma(
            r"\b+",
            "Parsing error at position 2: Target of repeat operator is invalid",
        );
    }

    #[test]
    fn keepout() {
        assert_eq!(
            p("a\\Kb"),
            Expr::Concat(vec![make_literal("a"), Expr::KeepOut, make_literal("b"),])
        );
    }

    #[test]
    fn no_quantifiers_on_other_non_repeatable_expressions() {
        assert_error(
            r"\K?",
            "Parsing error at position 2: Target of repeat operator is invalid",
        );
        assert_error(
            r"\G*",
            "Parsing error at position 2: Target of repeat operator is invalid",
        );
    }

    #[test]
    fn backref_exists_condition() {
        assert_eq!(
            p("(h)?(?(1))"),
            Expr::Concat(vec![
                Expr::Repeat {
                    child: Box::new(make_group(make_literal("h"))),
                    lo: 0,
                    hi: 1,
                    greedy: true
                },
                Expr::BackrefExistsCondition(1)
            ])
        );
        assert_eq!(
            p("(?<h>h)?(?('h'))"),
            Expr::Concat(vec![
                Expr::Repeat {
                    child: Box::new(make_group(make_literal("h"))),
                    lo: 0,
                    hi: 1,
                    greedy: true
                },
                Expr::BackrefExistsCondition(1)
            ])
        );
    }

    #[test]
    fn conditional_non_backref_validity_check_without_branches() {
        assert_error(
            "(?(foo))",
            "Parsing error at position 7: General parsing error: expected conditional to be a backreference or at least an expression for when the condition is true",
        );
    }

    #[test]
    fn conditional_invalid_target_of_repeat_operator() {
        assert_error(
            r"(?(?=\d)\w|!)",
            "Parsing error at position 3: Target of repeat operator is invalid",
        );
    }

    #[test]
    fn conditional_unclosed_at_end_of_pattern() {
        assert_error(
            r"(?(",
            "Parsing error at position 3: Opening parenthesis without closing parenthesis",
        );
    }

    #[test]
    fn subroutine_call_unclosed_at_end_of_pattern() {
        assert_error(
            r"\g<",
            "Parsing error at position 2: Could not parse group name",
        );

        assert_error(
            r"\g<name",
            "Parsing error at position 2: Could not parse group name",
        );

        assert_error(
            r"\g'",
            "Parsing error at position 2: Could not parse group name",
        );

        assert_error(
            r"\g''",
            "Parsing error at position 2: Could not parse group name",
        );

        assert_error(
            r"\g<>",
            "Parsing error at position 2: Could not parse group name",
        );

        assert_error(r"\g", "Parsing error at position 0: Invalid escape: \\g");

        assert_error(
            r"\g test",
            "Parsing error at position 2: Could not parse group name",
        );
    }

    #[test]
    fn subroutine_call_missing_subroutine_reference() {
        assert_error(
            r"\g test",
            "Parsing error at position 2: Could not parse group name",
        );
    }

    #[test]
    fn subroutine_call_name_includes_dash() {
        assert_error(
            r"\g<1-0>(a)",
            "Parsing error at position 2: Could not parse group name",
        );
        assert_error(
            r"\g<name+1>(?'name'a)",
            "Parsing error at position 2: Could not parse group name",
        );
    }

    #[test]
    fn backref_condition_with_one_two_or_three_branches() {
        assert_eq!(
            p("(h)?(?(1)i|x)"),
            Expr::Concat(vec![
                Expr::Repeat {
                    child: Box::new(make_group(make_literal("h"))),
                    lo: 0,
                    hi: 1,
                    greedy: true
                },
                Expr::Conditional {
                    condition: Box::new(Expr::BackrefExistsCondition(1)),
                    true_branch: Box::new(make_literal("i")),
                    false_branch: Box::new(make_literal("x")),
                },
            ])
        );

        assert_eq!(
            p("(h)?(?(1)i)"),
            Expr::Concat(vec![
                Expr::Repeat {
                    child: Box::new(make_group(make_literal("h"))),
                    lo: 0,
                    hi: 1,
                    greedy: true
                },
                Expr::Conditional {
                    condition: Box::new(Expr::BackrefExistsCondition(1)),
                    true_branch: Box::new(make_literal("i")),
                    false_branch: Box::new(Expr::Empty),
                },
            ])
        );

        assert_eq!(
            p("(h)?(?(1)ii|xy|z)"),
            Expr::Concat(vec![
                Expr::Repeat {
                    child: Box::new(make_group(make_literal("h"))),
                    lo: 0,
                    hi: 1,
                    greedy: true
                },
                Expr::Conditional {
                    condition: Box::new(Expr::BackrefExistsCondition(1)),
                    true_branch: Box::new(Expr::Concat(
                        vec![make_literal("i"), make_literal("i"),]
                    )),
                    false_branch: Box::new(Expr::Alt(vec![
                        Expr::Concat(vec![make_literal("x"), make_literal("y"),]),
                        make_literal("z"),
                    ])),
                },
            ])
        );

        assert_eq!(
            p("(?<cap>h)?(?(<cap>)ii|xy|z)"),
            Expr::Concat(vec![
                Expr::Repeat {
                    child: Box::new(make_group(make_literal("h"))),
                    lo: 0,
                    hi: 1,
                    greedy: true
                },
                Expr::Conditional {
                    condition: Box::new(Expr::BackrefExistsCondition(1)),
                    true_branch: Box::new(Expr::Concat(
                        vec![make_literal("i"), make_literal("i"),]
                    )),
                    false_branch: Box::new(Expr::Alt(vec![
                        Expr::Concat(vec![make_literal("x"), make_literal("y"),]),
                        make_literal("z"),
                    ])),
                },
            ])
        );
    }

    #[test]
    fn conditional() {
        assert_eq!(
            p("((?(a)b|c))(\\1)"),
            Expr::Concat(vec![
                make_group(Expr::Conditional {
                    condition: Box::new(make_literal("a")),
                    true_branch: Box::new(make_literal("b")),
                    false_branch: Box::new(make_literal("c"))
                }),
                make_group(Expr::Backref {
                    group: 1,
                    casei: false,
                },)
            ])
        );

        assert_eq!(
            p(r"^(?(\d)abc|\d!)$"),
            Expr::Concat(vec![
                Expr::Assertion(Assertion::StartText),
                Expr::Conditional {
                    condition: Box::new(Expr::Delegate {
                        inner: "\\d".to_string(),
                        casei: false,
                    }),
                    true_branch: Box::new(Expr::Concat(vec![
                        make_literal("a"),
                        make_literal("b"),
                        make_literal("c"),
                    ])),
                    false_branch: Box::new(Expr::Concat(vec![
                        Expr::Delegate {
                            inner: "\\d".to_string(),
                            casei: false,
                        },
                        make_literal("!"),
                    ])),
                },
                Expr::Assertion(Assertion::EndText),
            ])
        );

        assert_eq!(
            p(r"(?((?=\d))\w|!)"),
            Expr::Conditional {
                condition: Box::new(Expr::LookAround(
                    Box::new(Expr::Delegate {
                        inner: "\\d".to_string(),
                        casei: false
                    }),
                    LookAhead
                )),
                true_branch: Box::new(Expr::Delegate {
                    inner: "\\w".to_string(),
                    casei: false,
                }),
                false_branch: Box::new(make_literal("!")),
            },
        );

        assert_eq!(
            p(r"(?((ab))c|d)"),
            Expr::Conditional {
                condition: Box::new(make_group(Expr::Concat(vec![
                    make_literal("a"),
                    make_literal("b"),
                ]),)),
                true_branch: Box::new(make_literal("c")),
                false_branch: Box::new(make_literal("d")),
            },
        );
    }

    #[test]
    fn subroutines() {
        assert_eq!(
            p(r"(a)\g1"),
            Expr::Concat(vec![make_group(make_literal("a")), Expr::SubroutineCall(1)])
        );

        assert_eq!(
            p(r"(a)\g<1>"),
            Expr::Concat(vec![make_group(make_literal("a")), Expr::SubroutineCall(1)])
        );

        assert_eq!(
            p(r"(?<group_name>a)\g<group_name>"),
            Expr::Concat(vec![make_group(make_literal("a")), Expr::SubroutineCall(1)])
        );

        assert_eq!(
            p(r"(?<group_name>a)\g'group_name'"),
            Expr::Concat(vec![make_group(make_literal("a")), Expr::SubroutineCall(1)])
        );

        assert_eq!(
            p(r"(?<group_name>a)(?P>group_name)"),
            Expr::Concat(vec![make_group(make_literal("a")), Expr::SubroutineCall(1)])
        );
    }

    #[test]
    fn subroutine_defined_later() {
        assert_eq!(
            p(r"\g<name>(?<name>a)"),
            Expr::Concat(vec![Expr::SubroutineCall(1), make_group(make_literal("a")),])
        );

        assert_eq!(
            p(r"\g<c>(?:a|b|(?<c>c)?)"),
            Expr::Concat(vec![
                Expr::SubroutineCall(1),
                Expr::Alt(vec![
                    make_literal("a"),
                    make_literal("b"),
                    Expr::Repeat {
                        child: Box::new(make_group(make_literal("c"))),
                        lo: 0,
                        hi: 1,
                        greedy: true
                    }
                ])
            ])
        );

        assert_eq!(
            p(r"(?<a>a)?\g<b>(?(<a>)(?<b>b)|c)"),
            Expr::Concat(vec![
                Expr::Repeat {
                    child: Box::new(make_group(make_literal("a"))),
                    lo: 0,
                    hi: 1,
                    greedy: true
                },
                Expr::SubroutineCall(2),
                Expr::Conditional {
                    condition: Box::new(Expr::BackrefExistsCondition(1)),
                    true_branch: Box::new(make_group(make_literal("b"))),
                    false_branch: Box::new(make_literal("c")),
                }
            ])
        );

        assert_eq!(
            p(r"\g<1>(a)"),
            Expr::Concat(vec![Expr::SubroutineCall(1), make_group(make_literal("a")),])
        );
    }

    #[test]
    fn recursive_subroutine_call() {
        assert_eq!(
            p(r"\A(?<a>|.|(?:(?<b>.)\g<a>\k<b>))\z"),
            Expr::Concat(vec![
                Expr::Assertion(Assertion::StartText,),
                make_group(Expr::Alt(vec![
                    Expr::Empty,
                    Expr::Any { newline: false },
                    Expr::Concat(vec![
                        make_group(Expr::Any { newline: false },),
                        Expr::SubroutineCall(1,),
                        Expr::Backref {
                            group: 2,
                            casei: false,
                        },
                    ],),
                ],),),
                Expr::Assertion(Assertion::EndText,),
            ],)
        );
    }

    #[test]
    fn self_recursive_subroutine_call() {
        let tree = Expr::parse_tree(r"hello\g<0>?world").unwrap();
        assert!(tree.self_recursive);

        let tree = Expr::parse_tree(r"hello\g0?world").unwrap();
        assert!(tree.self_recursive);

        let tree = Expr::parse_tree(r"hello world").unwrap();
        assert!(!tree.self_recursive);

        let tree = Expr::parse_tree(r"hello\g1world").unwrap();
        assert!(!tree.self_recursive);

        let tree = Expr::parse_tree(r"hello\g<1>world").unwrap();
        assert!(!tree.self_recursive);

        let tree = Expr::parse_tree(r"(hello\g1?world)").unwrap();
        assert!(!tree.self_recursive);

        let tree = Expr::parse_tree(r"(?<a>hello\g<a>world)").unwrap();
        assert!(!tree.self_recursive);
    }

    #[test]
    fn named_subroutine_not_defined_later() {
        assert_eq!(
            p(r"\g<wrong_name>(?<different_name>a)"),
            Expr::Concat(vec![
                Expr::UnresolvedNamedSubroutineCall {
                    name: "wrong_name".to_string(),
                    ix: 2
                },
                make_group(make_literal("a")),
            ])
        );
    }

    // found by cargo fuzz, then minimized
    #[test]
    fn fuzz_1() {
        p(r"\ä");
    }

    #[test]
    fn fuzz_2() {
        p(r"\pä");
    }

    #[test]
    fn fuzz_3() {
        fail(r"(?()^");
        fail(r#"!w(?()\"Kuz>"#);
    }

    #[test]
    fn fuzz_4() {
        fail(r"\u{2}(?(2)");
    }

    #[test]
    fn word_boundary_brace_syntax() {
        // Valid patterns produce correct assertions
        assert_eq!(
            p(r"\b{start}"),
            Expr::Assertion(Assertion::LeftWordBoundary)
        );
        assert_eq!(p(r"\b{end}"), Expr::Assertion(Assertion::RightWordBoundary));
        assert_eq!(
            p(r"\b{start-half}"),
            Expr::Assertion(Assertion::LeftWordHalfBoundary)
        );
        assert_eq!(
            p(r"\b{end-half}"),
            Expr::Assertion(Assertion::RightWordHalfBoundary)
        );
    }

    #[test]
    fn word_boundary_brace_parsing_errors() {
        // Invalid patterns produce expected errors
        let test_cases = [
            (r"\b{invalid}", "Invalid escape: \\b{invalid}"),
            (r"\b{start ", "Invalid escape: \\b{...}"),
            (r"\b{end ", "Invalid escape: \\b{...}"),
            (r"\b{}", "Invalid escape: \\b{}"),
            (r"\b{", "Invalid escape: \\b{...}"),
            (r"\b{ }", "Invalid escape: \\b{ }"),
            (r"\b{START}", "Invalid escape: \\b{START}"),
            (r"\b{END}", "Invalid escape: \\b{END}"),
            (r"\b{ s t a r t }", "Invalid escape: \\b{ s t a r t }"),
            // \B{...} patterns should fail
            (r"\B{start}", "Invalid escape: \\B{start}"),
            (r"\B{end}", "Invalid escape: \\B{end}"),
            (r"\B{start-half}", "Invalid escape: \\B{start-half}"),
            (r"\B{end-half}", "Invalid escape: \\B{end-half}"),
            (r"\c{start}", "Invalid escape: \\c"),
        ];

        for (pattern, expected_error) in test_cases {
            assert_error(
                pattern,
                &format!("Parsing error at position 0: {}", expected_error),
            );
        }
    }

    fn get_options(func: impl Fn(SyntaxConfig) -> SyntaxConfig) -> RegexOptions {
        let mut options = RegexOptions::default();
        options.syntaxc = func(options.syntaxc);
        options
    }

    #[test]
    fn parse_with_case_insensitive_in_pattern() {
        let tree = Expr::parse_tree("(?i)hello");
        let expr = tree.unwrap().expr;

        assert_eq!(
            expr,
            Expr::Concat(vec![
                make_literal_case_insensitive("h", true),
                make_literal_case_insensitive("e", true),
                make_literal_case_insensitive("l", true),
                make_literal_case_insensitive("l", true),
                make_literal_case_insensitive("o", true)
            ])
        );
    }

    #[test]
    fn parse_with_case_insensitive_option() {
        let options = get_options(|x| x.case_insensitive(true));

        let tree = Expr::parse_tree_with_flags("hello", options.compute_flags());
        let expr = tree.unwrap().expr;

        assert_eq!(
            expr,
            Expr::Concat(vec![
                make_literal_case_insensitive("h", true),
                make_literal_case_insensitive("e", true),
                make_literal_case_insensitive("l", true),
                make_literal_case_insensitive("l", true),
                make_literal_case_insensitive("o", true)
            ])
        );
    }

    #[test]
    fn parse_with_multiline_in_pattern() {
        let options = get_options(|x| x);

        let tree = Expr::parse_tree_with_flags("(?m)^hello$", options.compute_flags());
        let expr = tree.unwrap().expr;

        assert_eq!(
            expr,
            Expr::Concat(vec![
                Expr::Assertion(Assertion::StartLine { crlf: false }),
                make_literal("h"),
                make_literal("e"),
                make_literal("l"),
                make_literal("l"),
                make_literal("o"),
                Expr::Assertion(Assertion::EndLine { crlf: false })
            ])
        );
    }

    #[test]
    fn pparse_with_multiline_option() {
        let options = get_options(|x| x.multi_line(true));

        let tree = Expr::parse_tree_with_flags("^hello$", options.compute_flags());
        let expr = tree.unwrap().expr;

        assert_eq!(
            expr,
            Expr::Concat(vec![
                Expr::Assertion(Assertion::StartLine { crlf: false }),
                make_literal("h"),
                make_literal("e"),
                make_literal("l"),
                make_literal("l"),
                make_literal("o"),
                Expr::Assertion(Assertion::EndLine { crlf: false })
            ])
        );
    }

    #[test]
    fn parse_with_dot_matches_new_line_in_pattern() {
        let options = get_options(|x| x);

        let tree = Expr::parse_tree_with_flags("(?s)(.*)", options.compute_flags());
        let expr = tree.unwrap().expr;

        assert_eq!(
            expr,
            make_group(Expr::Repeat {
                child: Box::new(Expr::Any { newline: true }),
                lo: 0,
                hi: usize::MAX,
                greedy: true
            })
        );
    }

    #[test]
    fn parse_with_dot_matches_new_line_option() {
        let options = get_options(|x| x.dot_matches_new_line(true));

        let tree = Expr::parse_tree_with_flags("(.*)", options.compute_flags());
        let expr = tree.unwrap().expr;

        assert_eq!(
            expr,
            make_group(Expr::Repeat {
                child: Box::new(Expr::Any { newline: true }),
                lo: 0,
                hi: usize::MAX,
                greedy: true
            })
        );
    }

    #[test]
    fn parse_fancy_with_dot_matches_new_line_in_pattern() {
        let options = get_options(|x| x.dot_matches_new_line(true));

        let tree = Expr::parse_tree_with_flags("(.*)(?<=hugo)", options.compute_flags());
        let expr = tree.unwrap().expr;

        assert_eq!(
            expr,
            Expr::Concat(vec![
                make_group(Expr::Repeat {
                    child: Box::new(Expr::Any { newline: true }),
                    lo: 0,
                    hi: usize::MAX,
                    greedy: true
                }),
                Expr::LookAround(
                    Box::new(Expr::Concat(vec![
                        make_literal("h"),
                        make_literal("u"),
                        make_literal("g"),
                        make_literal("o")
                    ])),
                    LookBehind
                )
            ])
        );
    }

    #[test]
    fn parse_with_case_insensitive_from_pattern_and_multi_line_option() {
        let options = get_options(|x| x.multi_line(true));

        let tree = Expr::parse_tree_with_flags("(?i)^hello$", options.compute_flags());
        let expr = tree.unwrap().expr;

        assert_eq!(
            expr,
            Expr::Concat(vec![
                Expr::Assertion(Assertion::StartLine { crlf: false }),
                make_literal_case_insensitive("h", true),
                make_literal_case_insensitive("e", true),
                make_literal_case_insensitive("l", true),
                make_literal_case_insensitive("l", true),
                make_literal_case_insensitive("o", true),
                Expr::Assertion(Assertion::EndLine { crlf: false })
            ])
        );
    }

    #[test]
    fn parse_with_multi_line_and_case_insensitive_options() {
        let mut options = get_options(|x| x.multi_line(true));
        options.syntaxc = options.syntaxc.case_insensitive(true);

        let tree = Expr::parse_tree_with_flags("^hello$", options.compute_flags());
        let expr = tree.unwrap().expr;

        assert_eq!(
            expr,
            Expr::Concat(vec![
                Expr::Assertion(Assertion::StartLine { crlf: false }),
                make_literal_case_insensitive("h", true),
                make_literal_case_insensitive("e", true),
                make_literal_case_insensitive("l", true),
                make_literal_case_insensitive("l", true),
                make_literal_case_insensitive("o", true),
                Expr::Assertion(Assertion::EndLine { crlf: false })
            ])
        );
    }

    #[test]
    fn parse_backtracking_control_verbs() {
        assert_eq!(
            p(r"(*FAIL)"),
            Expr::BacktrackingControlVerb(BacktrackingControlVerb::Fail)
        );
        assert_eq!(
            p(r"(*F)"),
            Expr::BacktrackingControlVerb(BacktrackingControlVerb::Fail)
        );
        assert_eq!(
            p(r"a(*FAIL)"),
            Expr::Concat(vec![
                make_literal("a"),
                Expr::BacktrackingControlVerb(BacktrackingControlVerb::Fail)
            ])
        );
        assert_eq!(
            p(r"(?(*FAIL)a|b)"),
            Expr::Conditional {
                condition: Box::new(Expr::BacktrackingControlVerb(BacktrackingControlVerb::Fail)),
                true_branch: Box::new(make_literal("a")),
                false_branch: Box::new(make_literal("b"))
            }
        );

        assert_eq!(
            p(r"(*ACCEPT)"),
            Expr::BacktrackingControlVerb(BacktrackingControlVerb::Accept)
        );
        assert_eq!(
            p(r"(*COMMIT)"),
            Expr::BacktrackingControlVerb(BacktrackingControlVerb::Commit)
        );
        assert_eq!(
            p(r"(*SKIP)"),
            Expr::BacktrackingControlVerb(BacktrackingControlVerb::Skip)
        );
        assert_eq!(
            p(r"(*PRUNE)"),
            Expr::BacktrackingControlVerb(BacktrackingControlVerb::Prune)
        );

        assert_error(
            "(*RANDOM)",
            "Parsing error at position 1: Target of repeat operator is invalid",
        );
    }

    #[test]
    fn remap_unicode_property_if_necessary_outside_class_tests() {
        // Test \p with unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{alnum}", true, false),
            r"[\p{alpha}\p{digit}]"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{blank}", true, false),
            r"[\p{Zs}\x09]"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{word}", true, false),
            r"\w"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{Greek}", true, false),
            r"\p{greek}"
        );

        // Test \p without unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{alnum}", false, false),
            r"[[:alnum:]]"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{blank}", false, false),
            r"[\t ]"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{word}", false, false),
            r"[_[:alnum:]]"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{Greek}", false, false),
            r"\p{greek}"
        );

        // Test \P with unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{alnum}", true, false),
            r"[^\p{alpha}\p{digit}]"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{blank}", true, false),
            r"[^\p{Zs}\x09]"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{word}", true, false),
            r"\W"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{Greek}", true, false),
            r"\P{greek}"
        );

        // Test \P without unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{alnum}", false, false),
            r"[^[:alnum:]]"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{blank}", false, false),
            r"[^\t ]"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{word}", false, false),
            r"[^_[:alnum:]]"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{Greek}", false, false),
            r"\P{greek}"
        );

        // Test \p{cntrl} with unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{cntrl}", true, false),
            r"[\x00-\x1F\x7F-\x9F]"
        );
        // Test \P{cntrl} with unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{cntrl}", true, false),
            r"[^\x00-\x1F\x7F-\x9F]"
        );
        // Test \p{cntrl} without unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{cntrl}", false, false),
            r"[[:cntrl:]]"
        );
        // Test \P{cntrl} without unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{cntrl}", false, false),
            r"[^[:cntrl:]]"
        );

        // Test \p{graph} with unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{graph}", true, false),
            r"[^\p{White_Space}\p{C}]"
        );
        // Test \P{graph} with unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{graph}", true, false),
            r"[\p{White_Space}\p{C}]"
        );
        // Test \p{graph} without unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{graph}", false, false),
            r"[[:graph:]]"
        );
        // Test \P{graph} without unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{graph}", false, false),
            r"[^[:graph:]]"
        );

        // Test \p{print} with unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{print}", true, false),
            r"[^\p{C}\t\n\v\f\r]"
        );
        // Test \P{print} with unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{print}", true, false),
            r"[\p{C}\t\n\v\f\r]"
        );
        // Test \p{print} without unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{print}", false, false),
            r"[[:print:]]"
        );
        // Test \P{print} without unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{print}", false, false),
            r"[^[:print:]]"
        );
        // Test \p{cs} and \P{cs} (surrogates, never matches in UTF-8)
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{Cs}", true, false),
            r"\P{any}"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{Cs}", true, false),
            r"\p{any}"
        );
    }

    #[test]
    fn remap_unicode_property_if_necessary_inside_class_tests() {
        // Test \p with unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{alnum}", true, true),
            r"\p{alpha}\p{digit}"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{blank}", true, true),
            r"\p{Zs}\x09"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{word}", true, true),
            r"\w"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{Greek}", true, true),
            r"\p{greek}"
        );

        // Test \p without unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{alnum}", false, true),
            r"[:alnum:]"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{blank}", false, true),
            r"\t "
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{word}", false, true),
            r"_[:alnum:]"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{Greek}", false, true),
            r"\p{greek}"
        );

        // Test \P with unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{alnum}", true, true),
            r"[^\p{alpha}\p{digit}]"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{blank}", true, true),
            r"[^\p{Zs}\x09]"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{word}", true, true),
            r"\W"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{Greek}", true, true),
            r"\P{greek}"
        );

        // Test \p{cntrl} inside class with unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{cntrl}", true, true),
            r"\x00-\x1F\x7F-\x9F"
        );
        // Test \P{cntrl} inside class with unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{cntrl}", true, true),
            r"[^\x00-\x1F\x7F-\x9F]"
        );
        // Test \p{cntrl} inside class without unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{cntrl}", false, true),
            r"[:cntrl:]"
        );
        // Test \P{cntrl} inside class without unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{cntrl}", false, true),
            r"[^[:cntrl:]]"
        );

        // Test \p{graph} inside class with unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{graph}", true, true),
            r"[^\p{White_Space}\p{C}]"
        );
        // Test \P{graph} inside class with unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{graph}", true, true),
            r"[\p{White_Space}\p{C}]"
        );
        // Test \p{graph} inside class without unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{graph}", false, true),
            r"[:graph:]"
        );
        // Test \P{graph} inside class without unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{graph}", false, true),
            r"[^[:graph:]]"
        );

        // Test \p{print} inside class with unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{print}", true, true),
            r"[^\p{C}\t\n\v\f\r]"
        );
        // Test \P{print} inside class with unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{print}", true, true),
            r"[\p{C}\t\n\v\f\r]"
        );
        // Test \p{print} inside class without unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{print}", false, true),
            r"[:print:]"
        );
        // Test \P{print} inside class without unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{print}", false, true),
            r"[^[:print:]]"
        );

        // Test \P without unicode flag
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{alnum}", false, true),
            r"[^[:alnum:]]"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{blank}", false, true),
            r"[^\t ]"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{word}", false, true),
            r"[^_[:alnum:]]"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{Greek}", false, true),
            r"\P{greek}"
        );
    }

    #[test]
    fn remap_unicode_property_case_insensitive() {
        // Test case-insensitive conversion for Emoji and other properties
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{Emoji}", true, false),
            r"\p{emoji}"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{EMOJI}", true, false),
            r"\p{emoji}"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{Emoji}", true, false),
            r"\P{emoji}"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{EMOJI}", true, false),
            r"\P{emoji}"
        );

        // Test Word property (both uppercase and lowercase)
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{Word}", true, false),
            r"\w"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{WORD}", true, false),
            r"\w"
        );
    }

    #[test]
    fn remap_unicode_property_caret_negation() {
        // Test \p{^...} syntax (Oniguruma negation inside braces)
        // \p{^Emoji} should become \P{emoji}
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{^Emoji}", true, false),
            r"\P{emoji}"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{^EMOJI}", true, false),
            r"\P{emoji}"
        );

        // \P{^Word} should become \p{word} which becomes \w
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{^Word}", true, false),
            r"\w"
        );
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{^WORD}", true, false),
            r"\w"
        );

        // \p{^Greek} should become \P{greek}
        assert_eq!(
            remap_unicode_property_if_necessary(r"\p{^Greek}", true, false),
            r"\P{greek}"
        );

        // \P{^Greek} should become \p{greek}
        assert_eq!(
            remap_unicode_property_if_necessary(r"\P{^Greek}", true, false),
            r"\p{greek}"
        );
    }

    #[test]
    fn parse_absent_repeater() {
        assert_eq!(
            p(r"(?~abc)"),
            Expr::Absent(Absent::Repeater(Box::new(Expr::Concat(vec![
                make_literal("a"),
                make_literal("b"),
                make_literal("c"),
            ]))))
        );
    }

    #[test]
    fn parse_absent_expression() {
        assert_eq!(
            p(r"(?~|abc|\d+)"),
            Expr::Absent(Absent::Expression {
                absent: Box::new(Expr::Concat(vec![
                    make_literal("a"),
                    make_literal("b"),
                    make_literal("c"),
                ])),
                exp: Box::new(Expr::Repeat {
                    child: Box::new(Expr::Delegate {
                        inner: "\\d".to_string(),
                        casei: false
                    }),
                    lo: 1,
                    hi: usize::MAX,
                    greedy: true
                }),
            })
        );
    }

    #[test]
    fn parse_absent_stopper() {
        assert_eq!(
            p(r"(?~|abc)"),
            Expr::Absent(Absent::Stopper(Box::new(Expr::Concat(vec![
                make_literal("a"),
                make_literal("b"),
                make_literal("c"),
            ]))))
        );
    }

    #[test]
    fn parse_absent_range_clear() {
        assert_eq!(p(r"(?~|)"), Expr::Absent(Absent::Clear));
    }
}
