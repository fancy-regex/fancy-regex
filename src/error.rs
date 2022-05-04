use std::fmt;

/// Result type for this crate with specific error enum.
pub type Result<T> = ::std::result::Result<T, Error>;

pub type ParseErrorPosition = usize;

/// An error for the result of compiling or running a regex.
#[derive(Debug)]
pub enum Error {
    // Compile time errors
    /// General parsing error
    ParseError(ParseErrorPosition, String),
    /// Opening parenthesis without closing parenthesis, e.g. `(a|b`
    UnclosedOpenParen(ParseErrorPosition),
    /// Invalid repeat syntax
    InvalidRepeat(ParseErrorPosition),
    /// Pattern too deeply nested
    RecursionExceeded(ParseErrorPosition),
    /// Look-behind assertion without constant size
    LookBehindNotConst(ParseErrorPosition),
    /// Backslash without following character
    TrailingBackslash(ParseErrorPosition),
    /// Invalid escape
    InvalidEscape(ParseErrorPosition, String),
    /// Unicode escape not closed
    UnclosedUnicodeName(ParseErrorPosition),
    /// Invalid hex escape
    InvalidHex(ParseErrorPosition),
    /// Invalid codepoint for hex or unicode escape
    InvalidCodepointValue(ParseErrorPosition),
    /// Invalid character class
    InvalidClass(ParseErrorPosition),
    /// Unknown group flag
    UnknownFlag(ParseErrorPosition, String),
    /// Disabling Unicode not supported
    NonUnicodeUnsupported(ParseErrorPosition),
    /// Invalid back reference
    InvalidBackref(ParseErrorPosition),
    /// Regex crate error
    InnerError(regex::Error),
    /// Couldn't parse group name
    InvalidGroupName(ParseErrorPosition),
    /// Invalid group id in escape sequence
    InvalidGroupNameBackref(ParseErrorPosition, String),
    /// Once named groups are used you cannot refer to groups by number
    NamedBackrefOnly,

    /// Quantifier on lookaround or other zero-width assertion
    TargetNotRepeatable(ParseErrorPosition),

    // Run time errors
    /// Max stack size exceeded for backtracking while executing regex.
    StackOverflow,
    /// Max limit for backtracking count exceeded while executing the regex.
    /// Configure using
    /// [`RegexBuilder::backtrack_limit`](struct.RegexBuilder.html#method.backtrack_limit).
    BacktrackLimitExceeded,

    /// This enum may grow additional variants, so this makes sure clients don't count on exhaustive
    /// matching. Otherwise, adding a new variant could break existing code.
    #[doc(hidden)]
    __Nonexhaustive,
}

impl ::std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // We should make these more helpful, e.g. by including the parts of the regex that lead to
        // the error.
        match self {
            Error::ParseError(position, s) => write!(f, "General parsing error at position {}: {}", position, s),
            Error::UnclosedOpenParen(position) => {
                write!(f, "Opening parenthesis without closing parenthesis at position {}", position)
            }
            Error::InvalidRepeat(position) => write!(f, "Invalid repeat syntax at position {}", position),
            Error::RecursionExceeded(position) => write!(f, "Pattern too deeply nested at position {}", position),
            Error::LookBehindNotConst(position) => write!(f, "Look-behind assertion without constant size at position {}", position),
            Error::TrailingBackslash(position) => write!(f, "Backslash without following character at position {}", position),
            Error::InvalidEscape(position, s) => write!(f, "Invalid escape at position {}: {}", position, s),
            Error::UnclosedUnicodeName(position) => write!(f, "Unicode escape not closed at position {}", position),
            Error::InvalidHex(position) => write!(f, "Invalid hex escape at position {}", position),
            Error::InvalidCodepointValue(position) => {
                write!(f, "Invalid codepoint for hex or unicode escape at position {}", position)
            }
            Error::InvalidClass(position) => write!(f, "Invalid character class at position {}", position),
            Error::UnknownFlag(position, s) => write!(f, "Unknown group flag at position {}: {}", position, s),
            Error::NonUnicodeUnsupported(position) => write!(f, "Disabling Unicode not supported at position {}", position),
            Error::InvalidBackref(position) => write!(f, "Invalid back reference at position {}", position),
            Error::InnerError(e) => write!(f, "Regex error: {}", e),
            Error::StackOverflow => write!(f, "Max stack size exceeded for backtracking"),
            Error::BacktrackLimitExceeded => write!(f, "Max limit for backtracking count exceeded"),
            Error::__Nonexhaustive => unreachable!(),
            Error::InvalidGroupName(position) => write!(f, "Could not parse group name at position {}", position),
            Error::InvalidGroupNameBackref(position, s) => write!(f, "Invalid group name in back reference at position {}: {}", position, s),
            Error::TargetNotRepeatable(position) => write!(f, "Target of repeat operator is invalid at position {}", position),
            Error::NamedBackrefOnly => write!(f, "Numbered backref/call not allowed because named group was used, use a named backref instead"),
        }
    }
}
