# Subroutines: reusable patterns with stable meaning

Subroutines in fancy-regex are not "inline expansions" and not "dynamic calls".
They are *compiled pattern definitions* that can be invoked safely and predictably.

## What is a subroutine

Any capture group can become a subroutine - it just needs to be "called".

Think of a subroutine as:

- defined by a capture group
- executed exactly the way the capture group was originally defined
- reusable from multiple places

>> Calling a subroutine does not recompile it in the caller's context.

## Side effects

A subroutine call has one side-effect - it updates the capture group position, which affects backref matching etc.

## Example

Let's imagine a pattern which will match a digit and capture it into group 1. Then it will call that capture group as a subroutine. Then it will do a backref to group 1.

This will match only when a digit is followed by another digit and then itself.

```rust
use fancy_regex::Regex;

let re = Regex::new(r"(\d)\g<1>\1").expect("expected compilation to be successful");
let result = re.captures("foo 711").expect("expected execution to be complete");

let captures = result.expect("expected to find a match");

let m = captures.get(0).expect("expected capture group 0 to exist");

assert_eq!(m.start(), 4);
assert_eq!(m.end(), 7);
assert_eq!(m.as_str(), "711");

let group = captures.get(1).expect("expected capture group 1 to exist");
assert_eq!(group.as_str(), "1");

assert!(!re.is_match("foo 717").expect("expected execution to complete"));
```

In the above example, 7 was stored in capture group 1. Then it was replaced with 1 by the subroutine call. Then the backreference to group 1 can only match the literal `1`.
