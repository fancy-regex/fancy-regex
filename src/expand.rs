use crate::parse::{parse_decimal, parse_id};
use crate::Captures;
use std::borrow::Cow;
use std::io;
use std::mem;

/// A set of options for expanding a template string using the contents
/// of capture groups.
#[derive(Debug)]
pub struct Expander {
    sub_char: char,
    open: &'static str,
    close: &'static str,
    allow_undelimited_name: bool,
    strict: bool,
}

impl Expander {
    /// Returns an expander that uses Python-compatible syntax.
    ///
    /// Expands all instances of `\num` or `\g<name>` in `replacement`
    /// to the corresponding capture group `num` or `name`, and writes
    /// them to the `dst` buffer given.
    ///
    /// `name` may be an integer corresponding to the index of the
    /// capture group (counted by order of opening parenthesis where `\0` is the
    /// entire match) or it can be a name (consisting of letters, digits or
    /// underscores) corresponding to a named capture group.
    ///
    /// `num` must be an integer corresponding to the index of the
    /// capture group.
    ///
    /// If `num` or `name` isn't a valid capture group (whether the name doesn't exist
    /// or isn't a valid index), then it is replaced with the empty string.
    ///
    /// The longest possible number is used. e.g., `\10` looks up capture
    /// group 10 and not capture group 1 followed by a literal 0.
    ///
    /// To write a literal `\`, use `\\`.
    pub fn python() -> Self {
        Expander {
            sub_char: '\\',
            open: "g<",
            close: ">",
            allow_undelimited_name: false,
            strict: false,
        }
    }

    /// Sets whether this expander will report errors caused by unknown
    /// group names and unclosed substitution expressions.
    ///
    /// Expanders are non-strict by default.
    pub fn set_strict(&mut self, value: bool) {
        self.strict = value;
    }

    /// Quotes the substitution character in `text` so it appears literally
    /// in the output of `expansion`.
    ///
    /// ```
    /// assert_eq!(
    ///     fancy_regex::Expander::default().quote("Has a literal $ sign."),
    ///     "Has a literal $$ sign.",
    /// );
    /// ```
    pub fn quote<'a>(&self, text: &'a str) -> Cow<'a, str> {
        if text.contains(self.sub_char) {
            let mut quoted = String::with_capacity(self.sub_char.len_utf8() * 2);
            quoted.push(self.sub_char);
            quoted.push(self.sub_char);
            Cow::Owned(text.replace(self.sub_char, &quoted))
        } else {
            Cow::Borrowed(text)
        }
    }

    /// Expands the template string `template` using the syntax defined
    /// by this expander and the values of capture groups from `captures`.
    ///
    /// Always succeeds when this expander is not strict.
    pub fn expansion<'t>(&self, template: &str, captures: &Captures<'t>) -> io::Result<String> {
        let mut cursor = io::Cursor::new(Vec::new());
        self.write_expansion(&mut cursor, template, captures)?;
        Ok(String::from_utf8(cursor.into_inner()).expect("expansion is UTF-8"))
    }

    /// Appends the expansion produced by `expansion` to `dst`.  Possibly more efficient
    /// than calling `expansion` directly.
    pub fn append_expansion<'t>(
        &self,
        dst: &mut String,
        template: &str,
        captures: &Captures<'t>,
    ) -> io::Result<()> {
        let mut cursor = io::Cursor::new(mem::replace(dst, String::new()).into_bytes());
        self.write_expansion(&mut cursor, template, captures)?;
        *dst = String::from_utf8(cursor.into_inner()).expect("expansion is UTF-8");
        Ok(())
    }

    /// Writes the expansion produced by `expansion` to `dst`.  Possibly more efficient
    /// than calling `expansion` directly.
    pub fn write_expansion<'t>(
        &self,
        mut dst: impl io::Write,
        template: &str,
        captures: &Captures<'t>,
    ) -> io::Result<()> {
        debug_assert!(!self.open.is_empty());
        debug_assert!(!self.close.is_empty());
        let mut iter = template.char_indices();
        while let Some((index, c)) = iter.next() {
            if c == self.sub_char {
                let tail = iter.as_str();
                let skip = if tail.starts_with(self.sub_char) {
                    write!(dst, "{}", self.sub_char)?;
                    1
                } else if let Some((id, skip)) =
                    parse_id(tail, self.open, self.close).or_else(|| {
                        if self.allow_undelimited_name {
                            parse_id(tail, "", "")
                        } else {
                            None
                        }
                    })
                {
                    if let Some(m) = captures.name(id) {
                        write!(dst, "{}", m.as_str())?;
                    } else if let Some(m) = id.parse().ok().and_then(|num| captures.get(num)) {
                        write!(dst, "{}", m.as_str())?;
                    } else if self.strict {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidData,
                            format!("invalid substitution group: {:?}", id),
                        ));
                    }
                    skip
                } else if let Some((skip, num)) = parse_decimal(tail, 0) {
                    if let Some(m) = captures.get(num) {
                        write!(dst, "{}", m.as_str())?;
                    }
                    skip
                } else if self.strict {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("invalid substitution sequence at position {}", index),
                    ));
                } else {
                    write!(dst, "{}", self.sub_char)?;
                    0
                };
                iter = iter.as_str()[skip..].char_indices();
            } else {
                write!(dst, "{}", c)?;
            }
        }
        Ok(())
    }
}

/// The default expander used by [`Captures::expand`].
///
/// [`Captures::expand`]: struct.Captures.html#expand
impl Default for Expander {
    fn default() -> Self {
        Expander {
            sub_char: '$',
            open: "{",
            close: "}",
            allow_undelimited_name: true,
            strict: false,
        }
    }
}
