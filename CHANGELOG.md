# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/).
This project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html),
with the exception that 0.x versions can break between minor versions.

## [unreleased] - 2019-10-19
### Added
- Add limit for backtracking so that execution errors instead of running
  for a long time in case of catastrophic backtracking.
- Add `RegexBuilder` with `backtrack_limit` to configure the new
  backtrack limit per regex.

## [0.2.0] - 2019-10-19
### Added
- More documentation and examples
- Support character class nesting and intersections (implemented in
  regex crate)
- Support atomic groups, both the the `(?>foo)` group syntax and the
  `a++`, `a*+` and `a?+` possessive syntax
- Support `\b`, `\f`, `\t`, `\n`, `\r`, `\v`
- Support look-behind with variable sized alternative
- Implement `Debug` for `Regex`
- More test coverage including running one of Oniguruma's test suites
### Changed
- Change `find` to return a `Match` struct (breaking change)
- Change `Captures` API (breaking change):
  - Replace `at` and `pos` with `get` that returns a `Match` struct
  - Remove `is_empty` (use `len`)
- Allow unescaped `]` and `}` as literals
- Allow unescaped `{` as literal when not after atom
- Allow escapes such as `\<` or `\e` inside character classes
- Allow up to 8 characters in `\x{...}` escape
- Allow escaping of space to make literal space
- Allow `(a|)`
- Reject invalid backreferences
### Fixed
- Multiple fixes for alternatives in look-arounds
- Fix hex escape to not include letters after "F"
- Fix handling of unescaped `]` in character classes
- Fix case insensitive character classes and other escapes
- Don't ignore spaces in character classes even with "comment mode"

## [0.1.0] - 2017-02-06
### Added
- Initial release

[unreleased]: https://github.com/fancy-regex/fancy-regex/compare/0.2.0...master
[0.2.0]: https://github.com/fancy-regex/fancy-regex/compare/0.1.0...0.2.0
[0.1.0]: https://github.com/fancy-regex/fancy-regex/commits/0.1.0
