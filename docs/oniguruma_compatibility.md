# Oniguruma compatibility

fancy-regex aims to be a memory-safe Rust alternative for many Oniguruma-style patterns, but it does not yet provide full feature parity.

This section summarizes the current compatibility status based on:

- Oniguruma syntax docs: [doc/RE](https://github.com/kkos/oniguruma/blob/master/doc/RE)
  and [doc/SYNTAX.md](https://github.com/kkos/oniguruma/blob/master/doc/SYNTAX.md)
- Oniguruma test cases [`tests/oniguruma/test_utf8.c`](https://github.com/kkos/oniguruma/blob/master/test/test_utf8.c)
- Known failing subset tracked in `tests/oniguruma/test_utf8_ignore.c`

## Coverage snapshot

The imported Oniguruma UTF-8 suite contains 1011 test cases.
At the time of writing, about 86% pass and 14% are listed in the ignore file.

Treat this as a moving target rather than a hard guarantee.

## What works well today

In Oniguruma mode (`RegexBuilder::oniguruma_mode(true)`), fancy-regex supports many high-value features used by Oniguruma users, including:

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
  - This is consistent with Oniguruma's broader fold behavior that includes multi-character case mappings.
- **Text segment / grapheme operators and options**
  - `\X`, `\y`, `\Y`, and inline `(?y{g})` / `(?y{w})` are not currently supported.
- **Oniguruma-specific inline flags**
  - `W`, `D`, `S`, and `P` option toggles (ASCII-only mode switches) are not currently supported.
- **Absent family beyond repeater**
  - Absent expression `(?~|absent|exp)`, absent stopper `(?~|absent)`, and range clear `(?~|)` are not yet implemented.
- **Recursion-level backreferences**
  - Relative recursion-level forms such as `\k<name+level>` are not yet implemented.
- **Certain escape forms**
  - Examples include `\cX`/`\C-X`, `\o{...}`, and one-digit `\x` forms accepted by Oniguruma.
- **Compatibility edge cases under duplicate group names / capture-state rules**
  - Some cases differ in backreference resolution and group-state behavior.
- **A few parsing and quantifier semantics differences**
  - Examples include class-range corner cases and some invalid-range quantifier behaviors accepted by Oniguruma.

## Migration guidance for Oniguruma users

- Enable Oniguruma parsing mode when porting patterns:
  `RegexBuilder::oniguruma_mode(true)`.
- Validate critical production patterns against fancy-regex, especially if they rely on:
  - Unicode case-insensitive matching with multi-character folds
  - `\X`/`\y`/`\Y` or `(?y{...})`
  - Absent expression/stopper/range-clear forms
  - Recursion-level backreferences
- When possible, prefer simpler equivalent constructs that avoid unsupported operators.
- Use `tests/oniguruma/test_utf8_ignore.c` as a concrete list of currently known incompatibilities.

## Alternative: a full Oniguruma port in Rust

fancy-regex is a hybrid engine that aims for *approximate* Oniguruma compatibility. If you need Oniguruma's behavior exactly, there is a better fit: [Ferroni](https://ferroni.dev), a line-by-line port of the C Oniguruma engine to Rust, maintained by Sebastian Software.

- **Verified parity, not approximation.** Ferroni keeps Oniguruma's parser, compiler and optimizer, and passes all 2,974 upstream UTF-8 test cases. A pattern that works in C Oniguruma works the same way in Ferroni.
- **No C dependency.** `cargo add ferroni` is the whole setup: no C toolchain, no bindgen, memory-safe Rust throughout, with ADRs documenting decisions taken so you alleviate any worries about ported C and can trust the design matches your needs.
- **More complete than C Oniguruma.** Unicode 18.0 data (C's last release ships 16.0), a vscode-oniguruma-compatible multi-pattern `Scanner` with UTF-16 offsets, per-search timeouts and retry limits, and a compile-time check for catastrophic backtracking.
- **Scope.** ASCII and UTF-8 only (two of Oniguruma's 29 encodings), and no POSIX/GNU APIs. TextMate grammars, VS Code, Shiki and jq are covered; PHP's `mbregex` in a non-UTF-8 encoding is not. BSD-2-Clause licensed, same as Oniguruma.

### When to choose which

- **Use Ferroni** when dropping C Oniguruma and keeping byte-for-byte behavior matters most - syntax highlighting of TextMate grammars, or any pattern that hits the gaps listed above. It is also faster than C Oniguruma.
- **Stay with fancy-regex** when your patterns fit the `regex` crate's syntax, when you want the hybrid NFA/backtracking performance guarantees, or you prefer an engine with modern design. It also supports a few things that Ferroni does not:
  - raw-bytes input: `BytesMode::Ascii` matches arbitrary bytes with ASCII-only classes, and `BytesMode::UnicodeBytes` accepts non-UTF-8 input while keeping Unicode-aware classes - useful for binary data or filenames.
  - GNU's leftmost-longest match semantics can be opted in to.
  - position assertions like `^`/`$` and `\G` can be overridden to match or not match in places they normally would match.

Ferroni is independent of the original Oniguruma author (the C project ended in April 2025) and is not affiliated with it. It is a maintained, feature-complete release as of v2.0.

The Ferroni website has a very detailed performance comparison, worth taking a look at to aid the decision.
