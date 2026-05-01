The README has a quick introduction to the performance of this crate.
This will look at some examples and compare them to the Oniguruma engine.

## Catastrophic backtracking

Backtracking engines can have worst-case performance when the regular
expression forces the engine to consider an exponentially increasing
number of sub-cases.

For a good explanation of that, read
[Runaway Regular Expressions: Catastrophic Backtracking][].

Let's look at the regex from the README again:

```regex
    (a|b|ab)*bc
```

And the input text:

    ababababababababababababababababababababababababababababac

Python's engine has exponential runtime. The regex crate and fancy-regex
however have no problem with it.

## Oniguruma

[Oniguruma][] implements a backtracking
engine. So we'd expect it to have a problem with the above regex too.

However, in the above case, it quickly finds that there's no match. How
is that possible? The answer is that it has optimizations which
sometimes help it avoid having to do any matching at all:

In the pattern `(a|b|ab)*bc`, you might notice that if the input doesn't
contain `bc`, the pattern will never match. Oniguruma detects that and,
before it tries to do any matching, tries to find `bc` in the input.

But what happens if we add `bc` at the end of the input, like this:

    ababababababababababababababababababababababababababababacbc

Now the optimization doesn't help anymore, and Oniguruma is slow too.

## fancy-regex

For `(a|b|ab)*bc` fancy-regex is fast in all cases because it can
delegate to the regex crate which matches it in linear runtime.

Let's look at another regex, one that makes use of a "fancy" look-ahead:

```regex
    (a|b|ab)*(?=c)
```
When fancy-regex matches it against this input:

    abababababababababababababababababababababababababababab

It's still fast! The reason is that although `(?=c)` is not supported
by the regex crate, fancy-regex detects the trailing positive lookahead
and is able to essentially rewrite the pattern into

```regex
    ((a|b|ab)*)c
```

and thus delegate the whole thing to the regex crate, and fixup the
captures/match boundaries after a match is found.

If, however, the lookahead didn't come at the end of the pattern,
it would be slow! The reason is that fancy-regex would need to handle
the lookahead with backtracking. And because `(a|b|ab)*` is before it,
that also needs to be done with backtracking as well.

Oniguruma doesn't have a problem with this particular case because its
optimization saves it again: It checks if there's a `c` in the input
before doing any matching.

fancy-regex now has a **Seek** pre-filter optimization (disabled by
default) for hard (backtracking) patterns, which provides similar
benefits for many cases. It derives a regular approximation of the
pattern and uses it to skip directly to the earliest plausible match
position in the haystack, so the backtracking VM only needs to run from
that position onward. For patterns where the hard parts leave useful
literals or anchors in the approximation — for example, backreferences
like `(\w+) \1` or anchored patterns like `(.{5})\1$` — the Seek
pre-filter can dramatically reduce the number of positions the VM tries.
Lookarounds are currently dropped when building the approximation pattern,
so the Seek pre-filter does not directly replicate Oniguruma's "find `c`
first" step in that specific case. The Seek optimization can be toggled
on/off via the `seek` method on `RegexBuilder`/`RegexOptionsBuilder`.

Note that how much fancy-regex can do without backtracking depends on
the structure of the regex. For example, with `(?=(a|b|ab)*bc)`, the
inner part of the look-ahead can be delegated to regex-automata entirely.

### Summary

* If the regex doesn't use fancy features, or the features used in the
  pattern can be identified as being syntactic sugar and slightly
  rewritten, fancy-regex should have linear runtime compared to
  Oniguruma's exponential worst-case.
* Even if the regex doesn't use any fancy features, Oniguruma can be
  faster because it is a mature and highly optimized engine.
* With fancy features, Oniguruma can still be faster in some cases. Its
  mature literal pre-search covers more patterns than the Seek pre-filter
  (for example, lookaround patterns where the Seek approximation drops
  the lookahead). The Seek pre-filter can be turned on or off via
  `RegexBuilder::seek(seek_enabled)` if needed.

[Runaway Regular Expressions: Catastrophic Backtracking]: https://www.regular-expressions.info/catastrophic.html
[Oniguruma]: https://github.com/kkos/oniguruma
