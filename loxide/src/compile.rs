pub fn compile<'a>(src: &'a str) -> Vec<Token<'a>> {
    let mut scanner = Scanner::new(src);

    let mut tokens = vec![];
    loop {
        let tok = scanner.token();
        let kind = tok.kind;
        tokens.push(tok);
        if kind == TokenKind::Eof {
            break;
        }
    }

    tokens
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token<'src> {
    kind: TokenKind,
    msg: &'src str,
    line: u32,
}

pub struct Scanner<'src> {
    src: &'src [u8],
    start: usize,
    current: usize,
    line: usize,
}

impl<'src> Scanner<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src: src.as_bytes(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn advance(&mut self) -> u8 {
        let ret = self.src[self.current];
        self.current += 1;
        ret
    }

    fn peek(&mut self) -> u8 {
        self.src.get(self.current).cloned().unwrap_or(b'\0')
    }

    fn peek_next(&mut self) -> u8 {
        if self.is_at_end() {
            b'\0'
        } else {
            self.src[self.current + 1]
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();

            match c {
                b' ' | b'\r' | b'\t' => {
                    self.advance();
                    break;
                }
                b'\n' => {
                    self.line += 1;
                    self.advance();
                }
                b'/' => {
                    if self.peek_next() == b'/' {
                        while self.peek() != b'\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    pub fn token(&mut self) -> Token<'src> {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            return self.make_token(TokenKind::Eof);
        }

        let c = self.advance();

        if Self::is_alpha(c) {
            return self.identifier();
        }

        if Self::is_digit(c) {
            return self.number();
        }

        match c {
            b'(' => return self.make_token(TokenKind::LeftParen),
            b')' => return self.make_token(TokenKind::RightParen),
            b'{' => return self.make_token(TokenKind::LeftBrace),
            b'}' => return self.make_token(TokenKind::RightBrace),
            b';' => return self.make_token(TokenKind::Semicolon),
            b',' => return self.make_token(TokenKind::Comma),
            b'.' => return self.make_token(TokenKind::Dot),
            b'-' => return self.make_token(TokenKind::Minus),
            b'+' => return self.make_token(TokenKind::Plus),
            b'/' => return self.make_token(TokenKind::Slash),
            b'*' => return self.make_token(TokenKind::Star),
            b'!' => {
                let kind = if self.matches(b'=') {
                    TokenKind::LessEqual
                } else {
                    TokenKind::Less
                };
                return self.make_token(kind);
            }
            b'=' => {
                let kind = if self.matches(b'=') {
                    TokenKind::EqualEqual
                } else {
                    TokenKind::Equal
                };
                return self.make_token(kind);
            }
            b'<' => {
                let kind = if self.matches(b'=') {
                    TokenKind::LessEqual
                } else {
                    TokenKind::Less
                };
                return self.make_token(kind);
            }
            b'>' => {
                let kind = if self.matches(b'=') {
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::Greater
                };
                return self.make_token(kind);
            }
            b'"' => return self.string(),
            _ => (),
        }

        self.error_token("Unexpected character.")
    }

    fn is_alpha(c: u8) -> bool {
        (c >= b'a' && c <= b'z') || (c >= b'A' && c <= b'Z') || c == b'_'
    }

    fn identifier(&mut self) -> Token<'src> {
        while Self::is_alpha(self.peek()) || Self::is_digit(self.peek()) {
            self.advance();
        }

        self.make_token(self.identifier_kind())
    }

    fn identifier_kind(&self) -> TokenKind {
        match self.src[self.start] {
            b'a' => self.check_keyword(1, 2, "nd", TokenKind::And),
            b'c' => self.check_keyword(1, 4, "lass", TokenKind::Class),
            b'e' => self.check_keyword(1, 3, "lse", TokenKind::Else),
            b'f' if self.current as i64 - self.start as i64 > 1 => match self.src[self.start + 1] {
                b'a' => self.check_keyword(2, 3, "lse", TokenKind::False),
                b'o' => self.check_keyword(2, 1, "r", TokenKind::For),
                b'u' => self.check_keyword(2, 1, "n", TokenKind::Fun),
                _ => TokenKind::Identifier,
            },
            b'i' => self.check_keyword(1, 1, "f", TokenKind::If),
            b'n' => self.check_keyword(1, 2, "il", TokenKind::Nil),
            b'o' => self.check_keyword(1, 1, "r", TokenKind::Or),
            b'p' => self.check_keyword(1, 4, "rint", TokenKind::Print),
            b'r' => self.check_keyword(1, 5, "eturn", TokenKind::Return),
            b's' => self.check_keyword(1, 4, "uper", TokenKind::Super),
            b't' if self.current as i64 - self.start as i64 > 1 => match self.src[self.start + 1] {
                b'h' => self.check_keyword(2, 2, "is", TokenKind::This),
                b'r' => self.check_keyword(2, 2, "rue", TokenKind::True),
                _ => TokenKind::Identifier,
            },
            b'v' => self.check_keyword(1, 2, "ar", TokenKind::Var),
            b'w' => self.check_keyword(1, 4, "hile", TokenKind::While),
            _ => TokenKind::Identifier,
        }
    }

    fn check_keyword(&self, start: usize, len: usize, rest: &str, kind: TokenKind) -> TokenKind {
        if self.current - self.start == start + len
            && &self.src[(self.start + start)..len] == rest.as_bytes()
        {
            return kind;
        }

        TokenKind::Identifier
    }

    fn string(&mut self) -> Token<'src> {
        while self.peek() != b'"' && !self.is_at_end() {
            if self.peek() == b'\n' {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            return self.error_token("Unterminated string.");
        }

        // closing quote
        self.advance();
        self.make_token(TokenKind::String)
    }

    fn is_digit(c: u8) -> bool {
        match c {
            b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8' | b'9' => true,
            _ => false,
        }
    }

    fn number(&mut self) -> Token<'src> {
        while Self::is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == b'.' && Self::is_digit(self.peek_next()) {
            // consume the '.'
            self.advance();

            while Self::is_digit(self.peek()) {
                self.advance();
            }
        }

        self.make_token(TokenKind::Number)
    }

    fn matches(&mut self, expected: u8) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.src[self.current] != expected {
            return false;
        }

        self.current += 1;

        true
    }

    fn is_at_end(&self) -> bool {
        self.current == self.src.len()
        // self.src[self.current] == b'\0'
    }

    fn make_token(&self, kind: TokenKind) -> Token<'src> {
        Token {
            kind,
            // Safety:
            // The input is guaranteed to be valid utf8 so this is safe
            msg: unsafe {
                std::str::from_utf8_unchecked(&self.src[self.start as usize..self.current as usize])
            },
            line: self.line as u32,
        }
    }

    fn error_token(&self, err: &'src str) -> Token<'src> {
        Token {
            kind: TokenKind::Error,
            msg: err,
            line: self.line as u32,
        }
    }
}
