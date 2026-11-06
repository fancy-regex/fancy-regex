# Optimizer

`src/optimize.rs` rewrites the parsed `Expr` tree before analysis/compilation.
Its goal is to preserve match semantics while making the program cheaper to run:
it reduces VM/backtracking work and, where safe, reshapes patterns so more work
can be delegated to the underlying `regex-automata` engine.

`optimize()` applies the enabled passes and returns whether capture group 0 must
be treated as an explicit wrapper group (see `keepout` / trailing-lookahead
handling in the root API). It skips the root-level capture-group rewrites for
self-recursive patterns, because moving that structure out of group 0 would be
incorrect.

## Passes

### KeepOut / trailing positive lookahead (`optimize_keepout`, `optimize_trailing_lookahead`)

A root-level `\K` resets the reported match start, so the reported match is the
suffix after `\K`. When the prefix before `\K` contains no capture groups, the
optimizer wraps that suffix in an explicit capture group and, if a trailing
positive lookahead is present, merges both into a single wrapper:

- `ab\Kcd` → `ab(cd)`
- `ab\Kcd(?=e)` → `ab(cd)e` (one wrapper group, lookahead inner moved outside)
- `a(?=b)` / `(?=b)` → `(a)b` / `()b`

Safety: capture groups in the prefix would renumber ahead of the new wrapper and
break the mechanism that reads the match span from group 1, so those cases are
left alone. The pass is root-concat-level (or the specific lookahead form) and
is not applied to self-recursive trees.

### Nested repeats (`optimize_nested_repeats`)

Simplifies provably equivalent nested greedy quantifiers, e.g. `(?:x+)+` → `x+`,
to avoid catastrophic backtracking from redundant nesting. It only folds
combinations where `can_simplify` / `quantifier_kind` say the bounds and
greediness are equivalent.

Safety around captures and backreferences:

- A bare (non-capturing/transparent) repeat of a repeat can be folded freely.
- A capturing group is special when the *outer* repeat has `lo == 0`
  (`*`/`?`): the group can be skipped entirely, so capture participation
  differs (`None` vs `Some("")`). Those cases are not folded. When `outer_lo > 0`
  the group always participates, so folding preserves capture semantics.
- With no backreferences and a self-absorbing inner language (an unbounded
  repeat / empty), an outer `*` over a participating group can be downgraded to
  `?` (`(X+)*` = `(X+)?`) because participation count is not observable; this is
  **not** done when backreferences exist.

### Ambiguous concat repeats (`optimize_ambiguous_concat_repeats`)

Targets ambiguity where two greedy repeats of the *same* atom sit around a
nullable/optional middle repeat in a concat: `A{L,*} M{0,h} A{R,*}` (`L,R` any
required minimum, greedy, same `A` on both sides, `M` nullable with `hi != 0`).
The original backtracks heavily because the boundary between the two `A` runs
is ambiguous when `M` is absent.

The rewrite factors that boundary, with the key invariant:

> A nullable wrapper must not discard a required minimum (`lo`) that the original
> still requires on a path where the wrapper (or choice) can be skipped.

Applicability/shape (`build_concat_repeat_triplet`). Both edges must be greedy
repeats (`X{lo,}` or `X{lo,hi}`); the middle must be nullable with `hi != 0`;
the two edges must be the same atom.

- `L == 0` (the left edge is nullable, e.g. `\w*\.?\w+`, `a{0,}b?a{2,}`,
  `a{0,}b?a{0,}`): keep `A{R,*}` mandatory as a suffix and wrap `A{L,*}M{1,h}`
  as an optional prefix. Safe because the moved edge has `lo == 0`, so the
  required minimum still comes from the mandatory right repeat (if any; when
  the right edge is also nullable the rewrite still matches the empty string,
  as the original does).
- `L >= R` and `R == 0` (the right edge is nullable, e.g. `\w+\w?\w*`,
  `a{2,}b?a{0,}`): keep `A{L,*}` mandatory as a prefix and wrap
  `M{1,h}A{R,*}` as an optional suffix. Safe because the moved edge has
  `lo == 0`.
- `L >= R` and `R >= 1`, both edges unbounded (e.g. `a+b?a+`, `a{2,}b?a{2,}`):
  **do not** put `A{R,*}` inside an optional group — when `M` is absent the
  original still requires `A{L,*}A{R,*}` (combined minimum `L+R`), so dropping
  `A{R,*}` is unsound. Instead emit `A{L,*} ( M{1,h}A{R,*} | A{R,R} )`: the left
  run stays mandatory and the tail chooses the middle path or consuming exactly
  the right run's minimum. This preserves the combined minimum and bounds the
  no-middle backtracking to a single `A{R}` choice instead of an unbounded
  split. (A form like `A+(?:(M A+)?|A{R})` would *not* be sound because the
  inner optional can still let that alternative contribute nothing.)
- `L >= R` and `R >= 1`, at least one edge bounded above (e.g.
  `a{2,5}b?a{1,4}`): keeping the edges separate still backtracks over their
  boundary, so instead emit `( A{L,*} M{1,h} A{R,*} | A{L+R, L_hi+R_hi} )`.
  The first alternative still consumes the middle; the second absorbs the whole
  run in a single repeat with the combined bounds, so the no-middle path has
  no split point to backtrack over. This is only safe when neither edge
  contains a capture group, because merging changes how many times any group
  inside an edge participates.

`check_repeated_concat` / `build_repeated_concat` apply a related factoring for a
quantified whole group whose body already has the `[prefix, optional-tail]`
shape. The rewrite is bottom-up and does not depend on whether the pattern is
otherwise "fancy", so semantic correctness of the rewritten tree matters for all
patterns.
