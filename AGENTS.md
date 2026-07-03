This is a Rust based repository for a regular expression engine.

## How it works

The core concept behind this library is to implement a backtracking virtual machine (VM) for regular expression matching, similar to PCRE.
However, whenever possible, this VM delegates work to an underlying regular expression engine - the Rust regex-automata crate - which does not otherwise support "fancy" features like lookarounds and backreferences, but has other desirable design goals - specifically, regex-automata has runtime linear to input length.

For regular expressions that do not use "fancy" features, the library acts primarily as a lightweight wrapper around the underlying engine.
When such features are present, the library performs an analysis to determine which parts of the expression must be handled by the backtracking engine and which can be safely delegated.

This analysis operates in two phases:

### Phase 1 - Bottom-Up Analysis

Each subexpression is analyzed to determine three key properties:

- *hard*: Whether the subexpression requires backtracking features (backreferences, look-around, atomic groups, conditionals)
- *minimum size*: The minimum number of characters this subexpression will match
- *constant size*: Whether the subexpression always matches the same number of characters

### Phase 2 - Top-Down Compilation

The compilation phase proceeds from the root of the expression, passing a "hard context" that flows from parent to child expressions. This context indicates whether match length variations will affect backtracking decisions.

*Delegation Strategy*: If both the subexpression and context are "easy", the compiler generates a `Delegate` instruction to offload work to the high-performance NFA engine. Otherwise, it generates explicit VM instructions.

*Concatenation Optimization*: For sequences of subexpressions, the compiler employs a sophisticated strategy:

1. Identify a prefix of constant-size, easy subexpressions that can be safely delegated (because they won't affect backtracking)
2. If the context is easy, identify a suffix of easy subexpressions for delegation
3. Compile the remaining "hard" middle section with explicit backtracking instructions
4. The hard context flows from right to left - only the rightmost hard subexpression gets an easy context

This ensures maximum delegation while preserving correct backtracking semantics.

### Summary

In summary, the system efficiently combines backtracking and automaton-based matching by delegating as much work as possible to the underlying high-performance NFA engine, only resorting to backtracking where strictly necessary. This hybrid approach provides both expressive power and performance for advanced regular expression features.

## Contributing

See the @CONTRIBUTING.md guide for details.

## Code Standards

### Required Before Each Commit
- Run `cargo fmt` before committing any changes to ensure proper code formatting

### Development Flow
- Test: `cargo test`
- Full CI check:
  - `cargo check --no-default-features`
  - `cargo check --no-default-features --features "unicode,perf,variable-lookbehinds"`
  - `cargo test --examples`
  - `cargo check --benches`
  - `cd playground && cargo test`

## Repository Structure
- `playground/`: Web based playground where fancy-regex is compiled to WASM for easy experimenting
- `src/`: All code

## Key Guidelines
1. Follow Rust best practices and idiomatic patterns - avoid writing unsafe code
2. Maintain existing code structure and organization unless asked to reorganize
3. Write unit tests for new functionality
5. Document public APIs and complex logic. Suggest changes to the Markdown documents when appropriate

## Tests

Tests are considered to be unit tests if they are inside `mod test` of a Rust source code file which is in the `src` folder and not in the `tests` folder. One exception could be `lib.rs` because it ties everything together, so here could be integration or unit tests. Prefer writing unit tests where possible, unless it makes sense to have an integration test. Documentation tests can be written for integration testing, the bonus being that it also improves documentation at the same time. But this should only be done when it will really bring benefit. There is no need to have many similar but mainly duplicate doc/integration tests, which just check that a different error is raised etc.

When adding unit tests, try to follow the conventions used in the file, like calling private methods from the `mod test` section instead of more integration style testing building new Regex instances.

You should *NEVER* modify `tests/oniguruma/test_utf8.c` - these tests come from Oniguruma directly, and are how we measure compatibility. If some Oniguruma tests fail (or no longer fail), update the ignore file (`tests/oniguruma/test_utf8_ignore.c` accordingly.

### Integration Test overview:
`matching.rs` caters for whether a pattern matches a haystack.
`finding.rs` caters for the position at which a pattern matches a haystack.
`captures.rs` caters for the positions at which capture groups match.

The other test modules are probably self explanatory.
It's also in scope to test that a `RuntimeError` occurred during matching.

### Unit Tests
Unit tests are also useful for checking private methods behave as expected which could be hard or time consuming to test end to end via an integration test.
For tests that need to check compilation errors, unit tests in `analyze.rs` or `compile.rs` (depending which file emits the error) should suffice.
