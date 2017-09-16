// Copyright 2016 Google Inc. All rights reserved.
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
                    try!(self.compile_delegates(&[info]));
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
                try!(self.compile_concat(info, hard));
            }
            Expr::Alt(_) => {
                let mut jmps = Vec::new();
                let mut last_pc = usize::MAX;
                for (i, child) in info.children.iter().enumerate() {
                    let has_next = i < info.children.len() - 1;
                    let pc = self.b.pc();
                    if has_next {
                        self.b.add(Insn::Split(pc + 1, usize::MAX));
                    }
                    if last_pc != usize::MAX {
                        self.b.set_split_target(last_pc, pc, true);
                    }
                    last_pc = pc;
                    try!(self.visit(child, hard));
                    let pc = self.b.pc();
                    jmps.push(pc);
                    self.b.add(Insn::Jmp(0));
                }
                let pc = self.b.pc();
                for jmp_pc in jmps {
                    self.b.set_jmp_target(jmp_pc, pc);
                }
            }
            Expr::Group(_) => {
                let group = info.start_group;
                self.b.add(Insn::Save(group * 2));
                try!(self.visit(&info.children[0], hard));
                self.b.add(Insn::Save(group * 2 + 1));
            }
            Expr::Repeat { lo, hi, greedy, .. } => {
                try!(self.compile_repeat(info, lo, hi, greedy, hard));
            }
            Expr::LookAround(_, la) if la == LookAhead || la == LookBehind => {
                let save = self.b.newsave();
                self.b.add(Insn::Save(save));
                try!(self.compile_lookaround(&info.children[0], la));
                self.b.add(Insn::Restore(save));
            }
            Expr::LookAround(_, la) => {  // negative look-around
                let pc = self.b.pc();
                self.b.add(Insn::Split(pc + 1, usize::MAX));
                try!(self.compile_lookaround(&info.children[0], la));
                self.b.add(Insn::DoubleFail);
                let next_pc = self.b.pc();
                self.b.set_split_target(pc, next_pc, true);
            }
            Expr::Backref(group) => {
                self.b.add(Insn::Backref(group * 2));
            }
            Expr::AtomicGroup(_) => {
                // TODO optimization: atomic insns are not needed if the
                // child doesn't do any backtracking.
                self.b.add(Insn::BeginAtomic);
                try!(self.visit(&info.children[0], false));
                self.b.add(Insn::EndAtomic);
            }
            Expr::Delegate { .. } | Expr::StartText | Expr::EndText
            | Expr::StartLine | Expr::EndLine => {
                // TODO: might want to have more specialized impls
                try!(self.compile_delegates(&[info]));
            }
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

        try!(self.compile_delegates(&children[..prefix_end]));

        if prefix_end < suffix_begin {
            for child in children[prefix_end..suffix_begin - 1].iter() {
                try!(self.visit(child, true));
            }
            try!(self.visit(children[suffix_begin - 1], hard));
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
            try!(self.visit(child, hard));
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
            try!(self.visit(child, hard));
            self.b.add(Insn::Jmp(pc));
            let next_pc = self.b.pc();
            self.b.set_repeat_target(pc, next_pc);
        } else if lo == 0 && hi == usize::MAX {
            // e*
            let pc = self.b.pc();
            self.b.add(Insn::Split(pc + 1, pc + 1));
            try!(self.visit(child, hard));
            self.b.add(Insn::Jmp(pc));
            let next_pc = self.b.pc();
            self.b.set_split_target(pc, next_pc, greedy);
        } else if lo == 1 && hi == usize::MAX {
            // e+
            let pc = self.b.pc();
            try!(self.visit(child, hard));
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
            try!(self.visit(child, hard));
            self.b.add(Insn::Jmp(pc));
            let next_pc = self.b.pc();
            self.b.set_repeat_target(pc, next_pc);
        }
        Ok(())
    }

    fn compile_lookaround(&mut self, child: &Info, la: LookAround) -> Result<()> {
        if la == LookBehind || la == LookBehindNeg {
            if !child.const_size {
                // TODO: should be able to handle an Alt of const-size subexprs
                return Err(Error::LookBehindNotConst);
            }
            self.b.add(Insn::GoBack(child.min_size));
        }
        self.visit(child, false)
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
        // Anchor whole expression and group the contents to make sure precedence is right
        // (e.g. `^a|b` and `^(?:a|b)` are different)
        annotated.push_str("^(?:");
        let mut min_size = 0;
        let mut const_size = true;
        let mut looks_left = false;
        for info in infos {
            looks_left |= info.looks_left && min_size == 0;
            min_size += info.min_size;
            const_size &= info.const_size;
            info.expr.to_str(&mut annotated, 0);
        }
        annotated.push(')');
        let start_group = infos[0].start_group;
        let end_group = infos[infos.len() - 1].end_group;
        self.make_delegate(&annotated, min_size, const_size, looks_left,
            start_group, end_group)
    }

    fn make_delegate(&mut self, inner_re: &str,
            min_size: usize, const_size: bool, looks_left: bool,
            start_group: usize, end_group: usize) -> Result<()> {
        let compiled = try!(compile_inner(inner_re));
        if looks_left {
            let inner1 = ["^(?s:.)", &inner_re[1..]].concat();
            let compiled1 = try!(compile_inner(&inner1));
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
    try!(c.visit(info, false));
    c.b.add(Insn::End);
    Ok(c.b.build())
}

