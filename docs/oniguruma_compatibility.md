# Oniguruma compatibility

fancy-regex aims to be a memory-safe Rust alternative for many Oniguruma-style
patterns, but it does not yet provide full feature parity.

This section summarizes the current compatibility status based on:

- Oniguruma syntax docs: [doc/RE](https://github.com/kkos/oniguruma/blob/master/doc/RE)
  and [doc/SYNTAX.md](https://github.com/kkos/oniguruma/blob/master/doc/SYNTAX.md)
- fancy-regex compatibility suite in `tests/oniguruma/test_utf8.c`
- Known failing subset tracked in `tests/oniguruma/test_utf8_ignore.c`

## Coverage snapshot

The imported Oniguruma UTF-8 suite currently contains 1011 cases.
At the time of writing, 868 pass and 143 are listed in the ignore file.

Treat this as a moving target rather than a hard guarantee.

## What works well today

In Oniguruma mode (`RegexBuilder::oniguruma_mode(true)`), fancy-regex supports
many high-value features used by Oniguruma users, including:

- Look-around (`(?=...)`, `(?!...)`, `(?<=...)`, `(?<!...)`)
- Backreferences and named backreferences (`\1`, `\k<name>`)
- Subroutine calls (`\g<name>`, `\g<1>`) with left-recursion checks
- Atomic groups (`(?>...)`)
- Conditionals (`(?(cond)then|else)`)
- Absent repeater (`(?~absent)`)
- Common escapes and anchors (`\A`, `\Z`, `\z`, `\G`, `\K`, `\R`, `\N`, `\O`)

## Known gaps vs Oniguruma

The remaining gaps are concentrated in specific advanced or edge-case areas:

- **Full Unicode case folding**
  - Patterns like `(?i:ss)` vs `ß`/`ẞ` are currently not equivalent.
  - This is consistent with Oniguruma's broader fold behavior that includes
    multi-character case mappings.
- **Text segment / grapheme operators and options**
  - `\X`, `\y`, `\Y`, and inline `(?y{g})` / `(?y{w})` are not currently
    supported.
- **Oniguruma-specific inline flags**
  - `W`, `D`, `S`, and `P` option toggles (ASCII-only mode switches) are not
    currently supported.
- **Absent family beyond repeater**
  - Absent expression `(?~|absent|exp)`, absent stopper `(?~|absent)`, and
    range clear `(?~|)` are not yet implemented.
- **Recursion-level backreferences**
  - Relative recursion-level forms such as `\k<name+level>` are not yet
    implemented.
- **Certain escape forms**
  - Examples include `\cX`/`\C-X`, `\o{...}`, and one-digit `\x` forms accepted
    by Oniguruma.
- **Compatibility edge cases under duplicate group names / capture-state rules**
  - Some cases differ in backreference resolution and group-state behavior.
- **A few parsing and quantifier semantics differences**
  - Examples include class-range corner cases and some invalid-range quantifier
    behaviors accepted by Oniguruma.

## Migration guidance for Oniguruma users

- Enable Oniguruma parsing mode when porting patterns:
  `RegexBuilder::oniguruma_mode(true)`.
- Validate critical production patterns against fancy-regex, especially if they
  rely on:
  - Unicode case-insensitive matching with multi-character folds
  - `\X`/`\y`/`\Y` or `(?y{...})`
  - Absent expression/stopper/range-clear forms
  - Recursion-level backreferences
- When possible, prefer simpler equivalent constructs that avoid unsupported
  operators.
- Use `tests/oniguruma/test_utf8_ignore.c` as a concrete list of currently known
  incompatibilities.
