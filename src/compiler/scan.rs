use lazy_static::lazy_static;
use regex::Regex;

pub struct CharIndices<'a> {
    chars: std::str::CharIndices<'a>,
    next: Option<(usize, char)>,
    input: &'a str,
    pub line: i32,
    pub col: i32,
}

#[derive(Clone, Copy)]
pub struct LocatedChar {
    pub value: char,
    pub start: usize,
    pub end: usize,
    pub line: i32,
    pub col: i32,
}

impl<'a> std::iter::Iterator for CharIndices<'a> {
    type Item = LocatedChar;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next {
            Some((start, value)) => {
                let next = self.chars.next();
                let end = match next {
                    Some((end, _)) => end,
                    None => self.input.len(),
                };
                if value == '\n' {
                    self.line += 1
                }
                self.next = next;
                Some(LocatedChar {
                    value,
                    start,
                    end,
                    line: self.line,
                    col: self.col,
                })
            }
            None => None,
        }
    }
}

impl<'a> CharIndices<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut chars = input.char_indices();
        let next = chars.next();

        Self {
            chars,
            next,
            input,
            line: 0,
            col: 0,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Token {
    pub kind: i32,
    pub start: usize,
    pub end: usize,
    pub line_start: i32,
    pub line_end: i32,
    pub col_start: i32,
    pub col_end: i32,
}

pub struct Scan<'a> {
    pub filename: &'a str,
    pub chars: CharIndices<'a>,
    pub input: &'a str,
    l0: Option<LocatedChar>,
}

impl<'a> Scan<'a> {
    pub fn new(input: &'a str, filename: &'a str) -> Self {
        let mut chars = CharIndices::new(input);
        let l0 = chars.next();
        Self {
            filename,
            chars,
            input,
            l0,
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.l0 {
            if ch.value.is_whitespace() {
                self.l0 = self.chars.next();
            } else {
                break;
            }
        }
    }

    fn create_token(&self, start: &LocatedChar, end: &LocatedChar) -> Token {
        Token {
            kind: get_token_kind(&self.input[start.start..end.end]),
            start: start.start,
            end: end.end,
            line_start: start.line,
            line_end: end.line,
            col_start: start.col,
            col_end: end.col,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let start;
        match self.l0 {
            None => {
                return Token {
                    kind: token::EOF,
                    start: self.input.len(),
                    end: self.input.len(),
                    line_start: self.chars.line,
                    line_end: self.chars.line,
                    col_start: self.chars.col,
                    col_end: self.chars.col,
                }
            }
            Some(ch) => start = ch,
        }

        let mut end = start;

        while let Some(ch) = self.l0 {
            match ch.value {
                '{' | '}' | '[' | ']' | '(' | ')' | '.' => {
                    // breaking character
                    if start.start == ch.start {
                        self.l0 = self.chars.next();
                        return self.create_token(&start, &ch);
                    }
                    return self.create_token(&start, &end);
                }

                '=' => {
                    if start.start != ch.start {
                        if start.value == '=' || can_before_equals(start.value) {
                            self.l0 = self.chars.next();
                            return self.create_token(&start, &ch);
                        } else {
                            return self.create_token(&start, &end);
                        }
                    }
                }

                value if can_before_equals(value) => {
                    if start.start != ch.start {
                        return self.create_token(&start, &end);
                    }
                    self.l0 = self.chars.next();
                }

                value if value.is_whitespace() => {
                    self.l0 = self.chars.next();
                    break;
                }

                _ => {
                    self.l0 = self.chars.next();
                    end = ch;
                }
            }
        }

        return self.create_token(&start, &end);
    }
}

pub mod token {
    use iota::iota;

    pub const EOF: i32 = 0;
    pub const ERROR: i32 = 1;

    iota! {
        pub const INT: i32 = iota + 256;
        , FLOAT
        , IDENT
        , EQ
        , LE
        , GE
        , NE
    }
}

#[cfg(test)]
mod tests {
    use super::{token, Scan};

    #[test]
    fn test_scanner() {
        let tests: Box<[(&'static str, Box<[i32]>)]> = Box::new([
            (
                "Hello < World",
                Box::new([token::IDENT, '<' as i32, token::IDENT, token::EOF]),
            ),
            (
                "Hello <= World",
                Box::new([token::IDENT, token::LE, token::IDENT, token::EOF]),
            ),
        ]);

        for (input, tokens) in tests.into_iter() {
            let mut scan = Scan::new(input, "<inline>");
            for &want_kind in tokens.into_iter() {
                let has_kind = scan.next_token().kind;
                assert_eq!(want_kind, has_kind);
            }
        }
    }
}

fn can_before_equals(c: char) -> bool {
    match c {
        '<' | '>' | '!' | '&' | '|' | '+' | '-' | '*' | '/' => true,
        _ => false,
    }
}

trait ParseToken {
    fn kind(&self) -> i32;
    fn is_match(&self, s: &str) -> bool;
}

struct RegexParseToken {
    expr: Regex,
    kind: i32,
}

impl ParseToken for RegexParseToken {
    fn is_match(&self, s: &str) -> bool {
        self.expr.is_match(s)
    }

    fn kind(&self) -> i32 {
        self.kind
    }
}

struct StringParseToken {
    str: &'static str,
    kind: i32,
}

impl ParseToken for StringParseToken {
    fn is_match(&self, s: &str) -> bool {
        s == self.str
    }

    fn kind(&self) -> i32 {
        self.kind
    }
}

struct CharParseToken {
    kind: i32,
    bytes: [u8; 4],
    len: usize,
}

impl ParseToken for CharParseToken {
    fn is_match(&self, s: &str) -> bool {
        s.as_bytes() == &self.bytes[..self.len]
    }

    fn kind(&self) -> i32 {
        self.kind
    }
}

fn expr_token(expr: &str, kind: i32) -> Box<dyn ParseToken + Sync> {
    Box::new(RegexParseToken {
        expr: Regex::new(expr).unwrap(),
        kind,
    })
}

fn str_token(str: &'static str, kind: i32) -> Box<dyn ParseToken + Sync> {
    Box::new(StringParseToken { str, kind })
}

fn char_token(c: char) -> Box<dyn ParseToken + Sync> {
    let mut bytes = [0u8; 4];
    let s = c.encode_utf8(&mut bytes);
    let len = s.len();

    Box::new(CharParseToken {
        bytes,
        kind: c as i32,
        len,
    })
}

lazy_static! {
    static ref TOKEN_EXPRS: Box<[Box<dyn ParseToken + Sync>]> = Box::new([
        char_token('<'),
        char_token('>'),
        char_token('['),
        char_token(']'),
        char_token('('),
        char_token(')'),
        char_token('<'),
        char_token('>'),
        char_token('='),
        char_token('+'),
        char_token('-'),
        char_token('*'),
        char_token('/'),
        str_token(">=", token::GE),
        str_token("<=", token::LE),
        expr_token(r"^[1-9][0-9]*$", token::INT),
        expr_token(r"^([1-9][0-9]*|0)\.[0-9]+$", token::FLOAT),
        expr_token(r"^[a-zA-Z_][a-zA-Z_0-9]*$", token::IDENT),
    ]);
}

fn get_token_kind(value: &str) -> i32 {
    for pt in TOKEN_EXPRS.iter() {
        if pt.is_match(value) {
            return pt.kind();
        }
    }
    return token::ERROR;
}