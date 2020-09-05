use crate::parse::{parse_decimal, parse_id};
use crate::Captures;

pub struct Expander {
    pub sub_char: char,
    pub open: &'static str,
    pub close: &'static str,
    pub allow_undelimited_name: bool,
}

impl Expander {
    pub fn expand<'t>(&self, captures: &Captures<'t>, replacement: &str, dst: &mut String) {
        let mut iter = replacement.chars();
        while let Some(c) = iter.next() {
            if c == self.sub_char {
                let tail = iter.as_str();
                let skip = if tail.starts_with(self.sub_char) {
                    dst.push(self.sub_char);
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
                        *dst += m.as_str();
                    } else if let Ok(num) = id.parse() {
                        captures.get(num).map(|m| *dst += m.as_str());
                    }
                    skip
                } else if let Some((skip, num)) = parse_decimal(tail, 0) {
                    captures.get(num).map(|m| *dst += m.as_str());
                    skip
                } else {
                    dst.push(self.sub_char);
                    0
                };
                iter = iter.as_str()[skip..].chars();
            } else {
                dst.push(c);
            }
        }
    }
}
