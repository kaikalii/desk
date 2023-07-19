use std::fmt;

#[derive(Clone, Copy)]
pub struct Span<'i> {
    pub start: usize,
    pub end: usize,
    pub path: &'i str,
    pub input: &'i str,
}

impl<'i> Span<'i> {
    pub fn loc(&self) -> (usize, usize) {
        let mut line = 1;
        let mut col = 1;
        for (i, c) in self.input.chars().enumerate() {
            if i == self.start {
                break;
            }
            if c == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        (line, col)
    }
    pub fn sp<T>(self, val: T) -> Sp<'i, T> {
        Sp { span: self, val }
    }
}

impl<'i> fmt::Debug for Span<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (line, col) = self.loc();
        write!(f, "{}:{}:{}", self.path, line, col)
    }
}

#[derive(Clone, Copy)]
pub struct Sp<'i, T> {
    pub span: Span<'i>,
    pub val: T,
}

impl<'i, T: fmt::Debug> fmt::Debug for Sp<'i, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {:?}", self.span, self.val)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Shape,
    Fn,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'i> {
    Ident(&'i str),
    Number(&'i str),
    Keyword(Keyword),
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenCurly,
    CloseCurly,
    Comma,
    Period,
    Colon,
    Semicolon,
    Octothorpe,
    At,
    Equals,
    Plus,
    Minus,
    Star,
    Slash,
}

pub fn lex<'i>(file: &'i str, input: &'i str) -> Result<Vec<Sp<'i, Token<'i>>>, char> {
    let mut lexer = Lexer {
        pos: 0,
        path: file,
        input,
        tokens: Vec::new(),
    };
    lexer.go()?;
    Ok(lexer.tokens)
}

struct Lexer<'i> {
    pos: usize,
    path: &'i str,
    input: &'i str,
    tokens: Vec<Sp<'i, Token<'i>>>,
}

impl<'i> Lexer<'i> {
    fn next_char_if(&mut self, f: impl Fn(char) -> bool) -> Option<char> {
        let c = self.input[self.pos..].chars().next()?;
        if !f(c) {
            return None;
        }
        self.pos += c.len_utf8();
        Some(c)
    }
    fn next_char_exact(&mut self, c: char) -> bool {
        self.next_char_if(|c2| c == c2).is_some()
    }
    fn next_char(&mut self) -> Option<char> {
        self.next_char_if(|_| true)
    }
    fn add_token(&mut self, start: usize, token: Token<'i>) {
        let span = Span {
            start,
            end: self.pos,
            path: self.path,
            input: self.input,
        };
        self.tokens.push(span.sp(token));
    }
    fn go(&mut self) -> Result<(), char> {
        loop {
            let start = self.pos;
            let Some(c) = self.next_char() else {
                break;
            };
            match c {
                '(' => self.add_token(start, Token::OpenParen),
                ')' => self.add_token(start, Token::CloseParen),
                '[' => self.add_token(start, Token::OpenBracket),
                ']' => self.add_token(start, Token::CloseBracket),
                '{' => self.add_token(start, Token::OpenCurly),
                '}' => self.add_token(start, Token::CloseCurly),
                ',' => self.add_token(start, Token::Comma),
                '.' => self.add_token(start, Token::Period),
                ':' => self.add_token(start, Token::Colon),
                ';' => self.add_token(start, Token::Semicolon),
                '#' => self.add_token(start, Token::Octothorpe),
                '@' => self.add_token(start, Token::At),
                '=' => self.add_token(start, Token::Equals),
                '+' => self.add_token(start, Token::Plus),
                '-' => self.add_token(start, Token::Minus),
                '*' => self.add_token(start, Token::Star),
                '/' => self.add_token(start, Token::Slash),
                c if is_ident_head(c) => {
                    // Idents and keywords
                    while self.next_char_if(is_ident_tail).is_some() {}
                    let ident = &self.input[start..self.pos];
                    let token = match ident {
                        "shape" => Token::Keyword(Keyword::Shape),
                        "fn" => Token::Keyword(Keyword::Fn),
                        _ => Token::Ident(ident),
                    };
                    self.add_token(start, token);
                }
                c if c.is_ascii_digit() => {
                    // Numbers
                    // Whole part
                    while self.next_char_if(|c| c.is_ascii_digit()).is_some() {}
                    // Fractional part
                    if self.next_char_exact('.') {
                        while self.next_char_if(|c| c.is_ascii_digit()).is_some() {}
                    }
                    // Exponent
                    if self.next_char_if(|c| "eE".contains(c)).is_some() {
                        self.next_char_if(|c| "+-".contains(c));
                        while self.next_char_if(|c| c.is_ascii_digit()).is_some() {}
                    }
                    let number = &self.input[start..self.pos];
                    self.add_token(start, Token::Number(number));
                }
                c if c.is_whitespace() => {}
                c => return Err(c),
            }
        }
        Ok(())
    }
}

fn is_ident_head(c: char) -> bool {
    c.is_alphabetic() || c == '_' || c as u32 > 127
}

fn is_ident_tail(c: char) -> bool {
    is_ident_head(c) || c.is_ascii_digit()
}
