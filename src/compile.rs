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

//! Compilation of regexes to VM.

use std::usize;
use regex;

use Expr;
use Result;
use Error;
use LookAround;
use LookAround::*;
use analyze::Info;
use vm::{Insn,Prog};

// I'm thinking it probably doesn't make a lot of sense having this split
// out from Compiler.
struct VMBuilder {
    prog: Vec<Insn>,
    n_saves: usize,
}

impl VMBuilder {
    fn new(max_group: usize) -> VMBuilder {
        VMBuilder {
            prog: Vec::new(),
            n_saves: max_group * 2,
        }
    }

    fn build(self) -> Prog {
        Prog::new(self.prog, self.n_saves)
    }

    fn newsave(&mut self) -> usize {
        let result = self.n_saves;
        self.n_saves += 1;
        result
    }

    fn pc(&self) -> usize {
        self.prog.len()
    }

    // would "emit" be a better name?
    fn add(&mut self, insn: Insn) {
        self.prog.push(insn);
    }

    fn set_jmp_target(&mut self, jmp_pc: usize, target: usize) {
        match self.prog[jmp_pc] {
            Insn::Jmp(ref mut next) => *next = target,
            _ => panic!("mutating instruction other than Jmp")
        }
    }

    fn set_split_target(&mut self, jmp_pc: usize, target: usize, second: bool) {
        match self.prog[jmp_pc] {
            Insn::Split(_, ref mut y) if second => *y = target,
            Insn::Split(ref mut x, _) => *x = target,
            _ => panic!("mutating instruction other than Split")
        }
    }

    fn set_repeat_target(&mut self, jmp_pc: usize, target: usize) {
        match self.prog[jmp_pc] {
            Insn::RepeatGr { ref mut next, .. } |
            Insn::RepeatNg { ref mut next, .. } |
            Insn::RepeatEpsilonGr { ref mut next, .. } |
            Insn::RepeatEpsilonNg { ref mut next, .. } => *next = target,
            _ => panic!("mutating instruction other than Repeat")
        }
    }
}

struct Compiler {
    b: VMBuilder,
}

impl Compiler {
    fn visit(&mut self, info: &Info, hard: bool) -> Result<()> {
        if !hard && !info.hard {
            // easy case, delegate entire subexpr
            return self.compile_delegates(&[info]);
        }
        match *info.expr {
            Expr::Empty => (),
            Expr::Literal{ ref val, casei } => {
                if !casei {
                    self.compile_delegates(&[info])?;
                } else {
                    self.b.add(Insn::Lit(val.clone()));
                }
            }
            Expr::Any { newline: true } => {
                self.b.add(Insn::Any);
            }
            Expr::Any { newline: false } => {
                self.b.add(Insn::AnyNoNL);
            }
            Expr::Concat(_) => {
                self.compile_concat(info, hard)?;
            }
            Expr::Alt(_) => {
                let count = info.children.len();
                self.compile_alt(count, |compiler, i| {
                    compiler.visit(&info.children[i], hard)
                })?;
            }
            Expr::Group(_) => {
                let group = info.start_group;
                self.b.add(Insn::Save(group * 2));
                self.visit(&info.children[0], hard)?;
                self.b.add(Insn::Save(group * 2 + 1));
            }
            Expr::Repeat { lo, hi, greedy, .. } => {
                self.compile_repeat(info, lo, hi, greedy, hard)?;
            }
            Expr::LookAround(_, la) => {
                self.compile_lookaround(info, la)?;
            }
            Expr::Backref(group) => {
                self.b.add(Insn::Backref(group * 2));
            }
            Expr::AtomicGroup(_) => {
                // TODO optimization: atomic insns are not needed if the
                // child doesn't do any backtracking.
                self.b.add(Insn::BeginAtomic);
                self.visit(&info.children[0], false)?;
                self.b.add(Insn::EndAtomic);
            }
            Expr::Delegate { .. } | Expr::StartText | Expr::EndText
            | Expr::StartLine | Expr::EndLine => {
                // TODO: might want to have more specialized impls
                self.compile_delegates(&[info])?;
            }
        }
        Ok(())
    }

    fn compile_alt<F>(&mut self, count: usize, mut handle_alternative: F) -> Result<()>
    where
        F: FnMut(&mut Compiler, usize) -> Result<()>,
    {
        let mut jmps = Vec::new();
        let mut last_pc = usize::MAX;
        for i in 0..count {
            let has_next = i != count - 1;
            let pc = self.b.pc();
            if has_next {
                self.b.add(Insn::Split(pc + 1, usize::MAX));
            }
            if last_pc != usize::MAX {
                self.b.set_split_target(last_pc, pc, true);
            }
            last_pc = pc;

            handle_alternative(self, i)?;

            if has_next {
                // All except the last branch need to jump over instructions of
                // other branches. The last branch can just continue to the next
                // instruction.
                let pc = self.b.pc();
                jmps.push(pc);
                self.b.add(Insn::Jmp(0));
            }
        }
        let next_pc = self.b.pc();
        for jmp_pc in jmps {
            self.b.set_jmp_target(jmp_pc, next_pc);
        }
        Ok(())
    }

    fn compile_concat(&mut self, info: &Info, hard: bool) -> Result<()> {
        let children: Vec<_> = info.children.iter().map(|c| c).collect();

        // First: determine a prefix which is constant size and not hard.
        let mut prefix_end = 0;
        for child in &children {
            if !child.const_size || child.hard {
                break;
            }
            prefix_end += 1;
        }

        // If incoming difficulty is not hard, the suffix after the last
        // hard child can be done with NFA.
        let mut suffix_begin = children.len();
        if !hard {
            for child in children[prefix_end..].iter().rev() {
                if child.hard {
                    break;
                }
                suffix_begin -= 1;
            }
        }

        self.compile_delegates(&children[..prefix_end])?;

        if prefix_end < suffix_begin {
            for child in children[prefix_end..suffix_begin - 1].iter() {
                self.visit(child, true)?;
            }
            self.visit(children[suffix_begin - 1], hard)?;
        }

        self.compile_delegates(&children[suffix_begin..])
    }

    fn compile_repeat(&mut self, info: &Info, lo: usize, hi: usize, greedy: bool,
                      hard: bool) -> Result<()> {
        let child = &info.children[0];
        if lo == 0 && hi == 1 {
            // e?
            let pc = self.b.pc();
            self.b.add(Insn::Split(pc + 1, pc + 1));
            // TODO: do we want to do an epsilon check here? If we do
            // it here and in Alt, we might be able to make a good
            // bound on stack depth
            self.visit(child, hard)?;
            let next_pc = self.b.pc();
            self.b.set_split_target(pc, next_pc, greedy);
            return Ok(());
        }
        let hard = hard | info.hard;
        if hi == usize::MAX && child.min_size == 0 {
            // Use RepeatEpsilon instructions to prevent empty repeat
            let repeat = self.b.newsave();
            let check = self.b.newsave();
            self.b.add(Insn::Save0(repeat));
            let pc = self.b.pc();
            if greedy {
                self.b.add(Insn::RepeatEpsilonGr{ lo: lo, next: usize::MAX,
                        repeat: repeat, check: check });
            } else {
                self.b.add(Insn::RepeatEpsilonNg{ lo: lo, next: usize::MAX,
                        repeat: repeat, check: check });
            }
            self.visit(child, hard)?;
            self.b.add(Insn::Jmp(pc));
            let next_pc = self.b.pc();
            self.b.set_repeat_target(pc, next_pc);
        } else if lo == 0 && hi == usize::MAX {
            // e*
            let pc = self.b.pc();
            self.b.add(Insn::Split(pc + 1, pc + 1));
            self.visit(child, hard)?;
            self.b.add(Insn::Jmp(pc));
            let next_pc = self.b.pc();
            self.b.set_split_target(pc, next_pc, greedy);
        } else if lo == 1 && hi == usize::MAX {
            // e+
            let pc = self.b.pc();
            self.visit(child, hard)?;
            let next = self.b.pc() + 1;
            let (x, y) = if greedy { (pc, next) } else { (next, pc) };
            self.b.add(Insn::Split(x, y));
        } else {
            let repeat = self.b.newsave();
            self.b.add(Insn::Save0(repeat));
            let pc = self.b.pc();
            if greedy {
                self.b.add(Insn::RepeatGr{ lo: lo, hi: hi, next: usize::MAX,
                        repeat: repeat });
            } else {
                self.b.add(Insn::RepeatNg{ lo: lo, hi: hi, next: usize::MAX,
                        repeat: repeat });
            }
            self.visit(child, hard)?;
            self.b.add(Insn::Jmp(pc));
            let next_pc = self.b.pc();
            self.b.set_repeat_target(pc, next_pc);
        }
        Ok(())
    }

    fn compile_lookaround(&mut self, info: &Info, la: LookAround) -> Result<()> {
        let inner = &info.children[0];
        match la {
            LookBehind => {
                if let &Info { const_size: false, expr: &Expr::Alt(_), .. } = inner {
                    // Make const size by transforming `(?<=a|bb)` to `(?<=a)|(?<=bb)`
                    let alternatives = &inner.children;
                    self.compile_alt(alternatives.len(), |compiler, i| {
                        let alternative = &alternatives[i];
                        compiler.compile_positive_lookaround(alternative, la)
                    })
                } else {
                    self.compile_positive_lookaround(inner, la)
                }
            }
            LookBehindNeg => {
                if let &Info { const_size: false, expr: &Expr::Alt(_), .. } = inner {
                    // Make const size by transforming `(?<!a|bb)` to `(?<!a)(?<!bb)`
                    let alternatives = &inner.children;
                    for alternative in alternatives {
                        self.compile_negative_lookaround(alternative, la)?;
                    }
                    Ok(())
                } else {
                    self.compile_negative_lookaround(inner, la)
                }
            }
            LookAhead => self.compile_positive_lookaround(inner, la),
            LookAheadNeg => self.compile_negative_lookaround(inner, la),
        }
    }

    fn compile_positive_lookaround(&mut self, inner: &Info, la: LookAround) -> Result<()> {
        let save = self.b.newsave();
        self.b.add(Insn::Save(save));
        self.compile_lookaround_inner(inner, la)?;
        self.b.add(Insn::Restore(save));
        Ok(())
    }

    fn compile_negative_lookaround(&mut self, inner: &Info, la: LookAround) -> Result<()> {
        let pc = self.b.pc();
        self.b.add(Insn::Split(pc + 1, usize::MAX));
        self.compile_lookaround_inner(inner, la)?;
        self.b.add(Insn::FailNegativeLookAround);
        let next_pc = self.b.pc();
        self.b.set_split_target(pc, next_pc, true);
        Ok(())
    }

    fn compile_lookaround_inner(&mut self, inner: &Info, la: LookAround) -> Result<()> {
        if la == LookBehind || la == LookBehindNeg {
            if !inner.const_size {
                return Err(Error::LookBehindNotConst);
            }
            self.b.add(Insn::GoBack(inner.min_size));
        }
        self.visit(inner, false)
    }

    fn compile_delegates(&mut self, infos: &[&Info]) -> Result<()> {
        if infos.is_empty() {
            return Ok(());
        }
        // TODO: might want to do something similar for case insensitive literals
        // (have is_literal return an additional bool for casei)
        if infos.iter().all(|e| e.is_literal()) {
            let mut val = String::new();
            for info in infos {
                info.push_literal(&mut val);
            }
            self.b.add(Insn::Lit(val));
            return Ok(());
        }
        // TODO: might want to detect case of a group with no captures
        // inside, so we can run find() instead of captures()
        let mut annotated = String::new();
        annotated.push('^');
        let mut min_size = 0;
        let mut const_size = true;
        let mut looks_left = false;
        for info in infos {
            looks_left |= info.looks_left && min_size == 0;
            min_size += info.min_size;
            const_size &= info.const_size;

            // Add expression. The precedence argument has to be 1 here to
            // ensure correct grouping in these cases:
            //
            // If we have multiple expressions, we are building a concat.
            // Without grouping, we'd turn ["a", "b|c"] into "^ab|c". But we
            // want "^a(?:b|c)".
            //
            // Even with a single expression, because we add `^` at the
            // beginning, we need a group. Otherwise `["a|b"]` would be turned
            // into `"^a|b"` instead of `"^(?:a|b)"`.
            info.expr.to_str(&mut annotated, 1);
        }
        let start_group = infos[0].start_group;
        let end_group = infos[infos.len() - 1].end_group;
        self.make_delegate(&annotated, min_size, const_size, looks_left,
            start_group, end_group)
    }

    fn make_delegate(&mut self, inner_re: &str,
            min_size: usize, const_size: bool, looks_left: bool,
            start_group: usize, end_group: usize) -> Result<()> {
        let compiled = compile_inner(inner_re)?;
        if looks_left {
            let inner1 = ["^(?s:.)", &inner_re[1..]].concat();
            let compiled1 = compile_inner(&inner1)?;
            self.b.add(Insn::Delegate {
                inner: Box::new(compiled),
                inner1: Some(Box::new(compiled1)),
                start_group: start_group,
                end_group: end_group,
            });
        } else if const_size && start_group == end_group {
            let size = min_size;
            self.b.add(Insn::DelegateSized(Box::new(compiled), size));
        } else {
            self.b.add(Insn::Delegate {
                inner: Box::new(compiled),
                inner1: None,
                start_group: start_group,
                end_group: end_group,
            });
        }
        Ok(())
    }

}

pub fn compile_inner(inner_re: &str) -> Result<regex::Regex> {
    regex::Regex::new(inner_re).map_err(Error::InnerError)
}

// Don't need the expr because the analysis info points to it
pub fn compile(info: &Info) -> Result<Prog> {
    let mut c = Compiler {
        b: VMBuilder::new(info.end_group),
    };
    c.visit(info, false)?;
    c.b.add(Insn::End);
    Ok(c.b.build())
}

#[cfg(test)]
mod tests {

    use super::*;
    use analyze::analyze;
    use bit_set::BitSet;

    #[test]
    fn jumps_for_alternation() {
        let expr = Expr::Alt(vec![
            Expr::Literal{ val: "a".into(), casei: false },
            Expr::Literal{ val: "b".into(), casei: false },
            Expr::Literal{ val: "c".into(), casei: false },
        ]);
        let backrefs = BitSet::new();
        let info = analyze(&expr, &backrefs).unwrap();

        let mut c = Compiler {
            b: VMBuilder::new(0),
        };
        // Force "hard" so that compiler doesn't just delegate
        c.visit(&info, true).unwrap();
        c.b.add(Insn::End);

        let prog = c.b.prog;

        assert_eq!(prog.len(), 8, "prog: {:?}", prog);
        assert_matches!(prog[0], Insn::Split(1, 3));
        assert_matches!(prog[1], Insn::Lit(ref l) if l == "a");
        assert_matches!(prog[2], Insn::Jmp(7));
        assert_matches!(prog[3], Insn::Split(4, 6));
        assert_matches!(prog[4], Insn::Lit(ref l) if l == "b");
        assert_matches!(prog[5], Insn::Jmp(7));
        assert_matches!(prog[6], Insn::Lit(ref l) if l == "c");
        assert_matches!(prog[7], Insn::End);
    }
}
