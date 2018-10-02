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

//! Backtracking VM for implementing fancy regexes.

use std::usize;
use std::collections::BTreeSet;
use regex::Regex;

use codepoint_len;
use prev_codepoint_ix;
use Result;
use Error;

pub const OPTION_TRACE: u32 = 1;

// TODO: make configurable
const MAX_STACK: usize = 1000000;

#[derive(Debug)]
pub enum Insn {
    End,
    Any,
    AnyNoNL,
    Lit(String),  // should be cow?
    Split(usize, usize),
    Jmp(usize),
    Save(usize),
    Save0(usize),
    Restore(usize),
    RepeatGr { lo: usize, hi: usize, next: usize, repeat: usize },
    RepeatNg { lo: usize, hi: usize, next: usize, repeat: usize },
    RepeatEpsilonGr { lo: usize, next: usize, repeat: usize, check: usize },
    RepeatEpsilonNg { lo: usize, next: usize, repeat: usize, check: usize },
    FailNegativeLookAround,
    GoBack(usize),
    Backref(usize),
    BeginAtomic,
    EndAtomic,
    DelegateSized(Box<Regex>, usize),
    Delegate {
        inner: Box<Regex>,
        inner1: Option<Box<Regex>>,  // regex with 1-char look-behind
        start_group: usize,
        end_group: usize,
    },
}

#[derive(Debug)]
pub struct Prog {
    pub body: Vec<Insn>,
    n_saves: usize,
}

impl Prog {
    pub fn new(body: Vec<Insn>, n_saves: usize) -> Prog {
        Prog {
            body: body,
            n_saves: n_saves,
        }
    }

    pub fn debug_print(&self) {
        for (i, insn) in self.body.iter().enumerate() {
            println!("{:3}: {:?}", i, insn);
        }
    }
}

struct State {
    pub saves: Vec<usize>,  // mostly indices to s, but can be repeat values etc

    // pc, index to string, nsave value
    pub stack: Vec<(usize, usize, usize)>,

    oldsave: Vec<(usize, usize)>,
    nsave: usize,
    explicit_sp: usize,
    max_stack: usize,
    options: u32,
}

// Each element in the stack conceptually represents the entire state
// of the machine: the pc (index into prog), the index into the
// string, and the entire vector of saves. However, copying the save
// vector on every push/pop would be inefficient, so instead we use a
// copy-on-write approach for each slot within the save vector. The
// top `nsave` elements in `oldsave` represent the delta from the
// current machine state to the top of stack.

impl State {
    fn new(n_saves: usize, max_stack: usize, options: u32) -> State {
        State {
            saves: vec![usize::MAX; n_saves],
            stack: Vec::new(),
            oldsave: Vec::new(),
            nsave: 0,
            explicit_sp: n_saves,
            max_stack,
            options
        }
    }

    // push a backtrack branch
    fn push(&mut self, pc: usize, ix: usize) -> Result<()> {
        if self.stack.len() < self.max_stack {
            self.stack.push((pc, ix, self.nsave));
            self.nsave = 0;
            self.trace_stack("push");
            Ok(())
        } else {
            Err(Error::StackOverflow)
        }
    }

    // pop a backtrack branch
    fn pop(&mut self) -> (usize, usize) {
        for _ in 0..self.nsave {
            let (slot, val) = self.oldsave.pop().unwrap();
            self.saves[slot] = val;
        }
        let (pc, ix, nsave) = self.stack.pop().unwrap();
        self.nsave = nsave;
        self.trace_stack("pop");
        (pc, ix)
    }

    fn save(&mut self, slot: usize, val: usize) {
        for i in 0..self.nsave {
            // could avoid this iteration with some overhead; worth it?
            if self.oldsave[self.oldsave.len() - i - 1].0 == slot {
                // already saved, just update
                self.saves[slot] = val;
                return;
            }
        }
        self.oldsave.push((slot, self.saves[slot]));
        self.nsave += 1;
        self.saves[slot] = val;

        if self.options & OPTION_TRACE != 0 {
            println!("saves: {:?}", self.saves);
        }
    }

    fn get(&self, slot: usize) -> usize {
        self.saves[slot]
    }

    // push a value onto the explicit stack; note: the entire contents of
    // the explicit stack is saved and restored on backtrack.
    fn stack_push(&mut self, val: usize) {
        if self.saves.len() == self.explicit_sp {
            self.saves.push(self.explicit_sp + 1);
        }
        let explicit_sp = self.explicit_sp;
        let sp = self.get(explicit_sp);
        if self.saves.len() == sp {
            self.saves.push(val);
        } else {
            self.save(sp, val);
        }
        self.save(explicit_sp, sp + 1);
    }

    // pop a value from the explicit stack
    fn stack_pop(&mut self) -> usize {
        let explicit_sp = self.explicit_sp;
        let sp = self.get(explicit_sp) - 1;
        let result = self.get(sp);
        self.save(explicit_sp, sp);
        result
    }

    // get the count of backtracks
    fn backtrack_count(&self) -> usize {
        self.stack.len()
    }

    // discard backtracks since the corresponding call to backtrack_count
    fn backtrack_cut(&mut self, count: usize) {
        if self.stack.len() == count {
            return;
        }
        let mut oldsave_ix = self.oldsave.len() - self.nsave;
        for &(_pc, _ix, nsave) in &self.stack[count + 1 ..] {
            oldsave_ix -= nsave;
        }
        let mut saved = BTreeSet::new();
        let oldsave_start = oldsave_ix - self.stack[count].2;
        for &(slot, _val) in &self.oldsave[oldsave_start .. oldsave_ix] {
            saved.insert(slot);
        }
        // retain all oldsave values, but only the first and only if not
        // already saved.
        for ix in oldsave_ix..self.oldsave.len() {
            let (slot, _val) = self.oldsave[ix];
            if saved.insert(slot) {
                self.oldsave.swap(oldsave_ix, ix);
                oldsave_ix += 1;
            }
        }
        self.stack.truncate(count);
        self.oldsave.truncate(oldsave_ix);
        self.nsave = oldsave_ix - oldsave_start;
    }

    #[inline]
    fn trace_stack(&self, operation: &str) {
        if self.options & OPTION_TRACE != 0 {
            println!("stack after {}: {:?}", operation, self.stack);
        }
    }
}

fn codepoint_len_at(s: &str, ix: usize) -> usize {
    codepoint_len(s.as_bytes()[ix])
}

pub fn run(prog: &Prog, s: &str, pos: usize, options: u32) ->
        Result<Option<Vec<usize>>> {
    let mut state = State::new(prog.n_saves, MAX_STACK, options);
    if options & OPTION_TRACE != 0 {
        println!("{}\t{}", "pos", "instruction");
    }
    let mut pc = 0;
    let mut ix = pos;
    loop {
        // break from this loop to fail, causes stack to pop
        'fail: loop {
            if options & OPTION_TRACE != 0 {
                println!("{}\t{} {:?}",
                         ix, pc, prog.body[pc]);
            }
            match prog.body[pc] {
                Insn::End => {
                    // save of end position into slot 1 is now done
                    // with an explicit group; we might want to
                    // optimize that.
                    //state.saves[1] = ix;
                    if options & OPTION_TRACE != 0 {
                        println!("saves: {:?}", state.saves);
                    }
                    return Ok(Some(state.saves));
                }
                Insn::Any => {
                    if ix < s.len() {
                        ix += codepoint_len_at(s, ix)
                    } else {
                        break 'fail;
                    }
                }
                Insn::AnyNoNL => {
                    if ix < s.len() && s.as_bytes()[ix] != b'\n' {
                        ix += codepoint_len_at(s, ix)
                    } else {
                        break 'fail;
                    }
                }
                Insn::Lit(ref val) => {
                    let end = ix + val.len();
                    if end > s.len() || &s.as_bytes()[ix..end] != val.as_bytes() {
                        break 'fail;
                    }
                    ix = end;
                }
                Insn::Split(x, y) => {
                    state.push(y, ix)?;
                    pc = x;
                    continue;
                }
                Insn::Jmp(target) => {
                    pc = target;
                    continue;
                }
                Insn::Save(slot) => state.save(slot, ix),
                Insn::Save0(slot) => state.save(slot, 0),
                Insn::Restore(slot) => ix = state.get(slot),
                Insn::RepeatGr { lo, hi, next, repeat } => {
                    let repcount = state.get(repeat);
                    if repcount == hi {
                        pc = next;
                        continue;
                    }
                    state.save(repeat, repcount + 1);
                    if repcount >= lo {
                        state.push(next, ix)?;
                    }
                }
                Insn::RepeatNg { lo, hi, next, repeat } => {
                    let repcount = state.get(repeat);
                    if repcount == hi {
                        pc = next;
                        continue;
                    }
                    state.save(repeat, repcount + 1);
                    if repcount >= lo {
                        state.push(pc + 1, ix)?;
                        pc = next;
                        continue;
                    }
                }
                Insn::RepeatEpsilonGr { lo, next, repeat, check } => {
                    let repcount = state.get(repeat);
                    if repcount > lo && state.get(check) == ix {
                        // prevent zero-length match on repeat
                        break 'fail;
                    }
                    state.save(repeat, repcount + 1);
                    if repcount >= lo {
                        state.save(check, ix);
                        state.push(next, ix)?;
                    }
                }
                Insn::RepeatEpsilonNg { lo, next, repeat, check } => {
                    let repcount = state.get(repeat);
                    if repcount > lo && state.get(check) == ix {
                        // prevent zero-length match on repeat
                        break 'fail;
                    }
                    state.save(repeat, repcount + 1);
                    if repcount >= lo {
                        state.save(check, ix);
                        state.push(pc + 1, ix)?;
                        pc = next;
                        continue;
                    }
                }
                Insn::GoBack(count) => {
                    for _ in 0..count {
                        if ix == 0 {
                            break 'fail;
                        }
                        ix = prev_codepoint_ix(s, ix);
                    }
                }
                Insn::FailNegativeLookAround => {
                    // Reaching this instruction means that the body of the
                    // lookaround matched. Because it's a *negative* lookaround,
                    // that means the lookaround itself should fail (not match).
                    // But before, we need to discard all the states that have
                    // been pushed with the lookaround, because we don't want to
                    // explore them.
                    loop {
                        let (popped_pc, _) = state.pop();
                        if popped_pc == pc + 1 {
                            // We've reached the state that would jump us to
                            // after the lookaround (in case the lookaround
                            // succeeded). That means we popped enough states.
                            break;
                        }
                    }
                    break 'fail;
                }
                Insn::Backref(slot) => {
                    let lo = state.get(slot);
                    let hi = state.get(slot + 1);
                    let ix_end = ix + (hi - lo);
                    if ix_end > s.len() || s[ix..ix_end] != s[lo..hi] {
                        break 'fail;
                    }
                    ix = ix_end;
                }
                Insn::BeginAtomic => {
                    let count = state.backtrack_count();
                    state.stack_push(count);
                }
                Insn::EndAtomic => {
                    let count = state.stack_pop();
                    state.backtrack_cut(count);
                }
                Insn::DelegateSized(ref inner, size) => {
                    if inner.is_match(&s[ix..]) {
                        // We could analyze for ascii-only, and ix += size in
                        // that case. Unlikely to be speed-limiting though.
                        for _ in 0..size {
                            ix += codepoint_len_at(s, ix);
                        }
                    } else {
                        break 'fail;
                    }
                }
                Insn::Delegate { ref inner, ref inner1, start_group, end_group } => {
                    let re = match *inner1 {
                        Some(ref inner1) if ix > 0 => {
                            ix = prev_codepoint_ix(s, ix);
                            inner1
                        }
                        _ => inner,
                    };
                    if start_group == end_group {
                        match re.find(&s[ix..]) {
                            Some(m) => ix += m.end(),
                            _ => break 'fail
                        }
                    } else if let Some(caps) = re.captures(&s[ix..]) {
                        let mut slot = start_group * 2;
                        for i in 0..(end_group - start_group) {
                            if let Some(m) = caps.get(i + 1) {
                                state.save(slot, ix + m.start());
                                state.save(slot + 1, ix + m.end());
                            } else {
                                state.save(slot, usize::MAX);
                                state.save(slot + 1, usize::MAX);
                            }
                            slot += 2;
                        }
                        ix += caps.get(0).unwrap().end();
                    } else {
                        break 'fail;
                    }
                }
            }
            pc += 1;
        }
        if options & OPTION_TRACE != 0 {
            println!("fail");
        }
        // "break 'fail" goes here
        if state.stack.is_empty() {
            return Ok(None);
        }
        let (newpc, newix) = state.pop();
        pc = newpc;
        ix = newix;
    }

}


#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::{Arbitrary, Gen};

    #[test]
    fn state_push_pop() {
        let mut state = State::new(1, MAX_STACK, 0);

        state.push(0, 0).unwrap();
        state.push(1, 1).unwrap();
        assert_eq!(state.pop(), (1, 1));
        assert_eq!(state.pop(), (0, 0));
        assert!(state.stack.is_empty());

        state.push(2, 2).unwrap();
        assert_eq!(state.pop(), (2, 2));
        assert!(state.stack.is_empty());
    }

    #[test]
    fn state_save_override() {
        let mut state = State::new(1, MAX_STACK, 0);
        state.save(0, 10);
        state.push(0, 0).unwrap();
        state.save(0, 20);
        assert_eq!(state.pop(), (0, 0));
        assert_eq!(state.get(0), 10);
    }

    #[test]
    fn state_save_override_twice() {
        let mut state = State::new(1, MAX_STACK, 0);
        state.save(0, 10);
        state.push(0, 0).unwrap();
        state.save(0, 20);
        state.push(1, 1).unwrap();
        state.save(0, 30);

        assert_eq!(state.get(0), 30);
        assert_eq!(state.pop(), (1, 1));
        assert_eq!(state.get(0), 20);
        assert_eq!(state.pop(), (0, 0));
        assert_eq!(state.get(0), 10);
    }

    #[derive(Clone, Debug)]
    enum Operation {
        Push,
        Pop,
        Save(usize, usize),
    }

    impl Arbitrary for Operation {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            match g.gen_range(0, 3) {
                0 => Operation::Push,
                1 => Operation::Pop,
                _ => Operation::Save(g.gen_range(0, 5), g.gen_range(0, usize::MAX)),
            }
        }
    }

    fn check_saves_for_operations(operations: Vec<Operation>) -> bool {
        let slots = operations
            .iter()
            .map(|o| match o {
                &Operation::Save(slot, _) => slot + 1,
                _ => 0,
            })
            .max()
            .unwrap_or(0);
        if slots == 0 {
            // No point checking if there's no save instructions
            return true;
        }

        // Stack with the complete VM state (including saves)
        let mut stack = Vec::new();
        let mut saves = vec![usize::MAX; slots];

        let mut state = State::new(slots, MAX_STACK, 0);

        let mut expected = Vec::new();
        let mut actual = Vec::new();

        for operation in operations {
            match operation {
                Operation::Push => {
                    // We're not checking pc and ix later, so don't bother
                    // putting in random values.
                    stack.push((0, 0, saves.clone()));
                    state.push(0, 0).unwrap();
                }
                Operation::Pop => {
                    // Note that because we generate the operations randomly
                    // there might be more pops than pushes. So ignore a pop
                    // if the stack was empty.
                    if let Some((_, _, previous_saves)) = stack.pop() {
                        saves = previous_saves;
                        state.pop();
                    }
                }
                Operation::Save(slot, value) => {
                    saves[slot] = value;
                    state.save(slot, value);
                }
            }

            // Remember state of saves for checking later
            expected.push(saves.clone());
            let mut actual_saves = vec![usize::MAX; slots];
            for i in 0..slots {
                actual_saves[i] = state.get(i);
            }
            actual.push(actual_saves);
        }

        expected == actual
    }

    quickcheck! {
        fn state_save_quickcheck(operations: Vec<Operation>) -> bool {
            check_saves_for_operations(operations)
        }
    }
}
