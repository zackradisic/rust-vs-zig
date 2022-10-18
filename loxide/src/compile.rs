use std::mem::MaybeUninit;

use crate::{
    chunk::{Chunk, Opcode},
    obj::{Obj, ObjList, ObjString},
    table::Table,
    value::Value,
};

#[derive(Debug, Clone, Copy)]
struct ParseRuleCtx {
    can_assign: bool,
}

type ParseFn<'s> = fn(&mut Compiler<'s>, ParseRuleCtx);

pub struct ParseRule<'s> {
    prefix: Option<ParseFn<'s>>,
    infix: Option<ParseFn<'s>>,
    precedence: Precedence,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Precedence {
    None = 0,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    fn from_u8(val: u8) -> Option<Self> {
        use Precedence::*;
        match val {
            0 => Some(None),
            1 => Some(Assignment),
            2 => Some(Or),
            3 => Some(And),
            4 => Some(Equality),
            5 => Some(Comparison),
            6 => Some(Term),
            7 => Some(Factor),
            8 => Some(Unary),
            9 => Some(Call),
            10 => Some(Primary),
            _ => Option::None,
        }
    }
}

macro_rules! none_prec {
    () => {
        ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        }
    };
}

macro_rules! parse_rule {
    (pre=$prefix:expr, $prec:expr) => {
        ParseRule {
            prefix: Some($prefix),
            infix: None,
            precedence: $prec,
        }
    };
    (inf=$infix:expr, $prec:expr) => {
        ParseRule {
            prefix: None,
            infix: Some($infix),
            precedence: $prec,
        }
    };
    (pre=$prefix:expr, inf=$infix:expr, $prec:expr) => {
        ParseRule {
            prefix: Some($prefix),
            infix: Some($infix),
            precedence: $prec,
        }
    };
}

pub struct Local<'src> {
    name: Token<'src>,
    depth: Option<u32>,
}

pub struct Compiler<'src> {
    pub parser: Parser<'src>,
    pub scanner: Scanner<'src>,
    pub chunk: Chunk,
    pub obj_list: ObjList,
    pub interned_strings: Table,

    locals: [MaybeUninit<Local<'src>>; u8::MAX as usize],
    local_count: usize,
    scope_depth: usize,
}

impl<'src> Compiler<'src> {
    pub const PARSE_RULES: [ParseRule<'src>; 40] = [
        // left paren
        parse_rule!(pre = Compiler::grouping, Precedence::None),
        // right paren
        none_prec!(),
        // left brace
        none_prec!(),
        // right brace
        none_prec!(),
        // comma
        none_prec!(),
        // dot
        none_prec!(),
        // minus
        parse_rule!(
            pre = Compiler::unary,
            inf = Compiler::binary,
            Precedence::Term
        ),
        // plus
        parse_rule!(inf = Compiler::binary, Precedence::Term),
        // semicolon
        none_prec!(),
        // slash
        parse_rule!(inf = Compiler::binary, Precedence::Factor),
        // star
        parse_rule!(inf = Compiler::binary, Precedence::Factor),
        // bang
        parse_rule!(pre = Compiler::unary, Precedence::None),
        // bangequal
        parse_rule!(inf = Compiler::binary, Precedence::Equality),
        // equal
        parse_rule!(inf = Compiler::binary, Precedence::Equality),
        // equalequal
        parse_rule!(inf = Compiler::binary, Precedence::Comparison),
        // greater
        parse_rule!(inf = Compiler::binary, Precedence::Comparison),
        // greaterequal
        parse_rule!(inf = Compiler::binary, Precedence::Comparison),
        // less
        parse_rule!(inf = Compiler::binary, Precedence::Comparison),
        // lessequal
        parse_rule!(inf = Compiler::binary, Precedence::Comparison),
        // identifier
        parse_rule!(pre = Compiler::variable, Precedence::None),
        // string
        parse_rule!(pre = Compiler::string, Precedence::None),
        // number
        parse_rule!(pre = Compiler::number, Precedence::None),
        // and
        none_prec!(),
        // class
        none_prec!(),
        // else
        none_prec!(),
        // false
        parse_rule!(pre = Compiler::literal, Precedence::None),
        // for
        none_prec!(),
        // fun
        none_prec!(),
        // if
        none_prec!(),
        // nil
        parse_rule!(pre = Compiler::literal, Precedence::None),
        // or
        none_prec!(),
        // print
        none_prec!(),
        // return
        none_prec!(),
        // super
        none_prec!(),
        // this
        none_prec!(),
        // true
        parse_rule!(pre = Compiler::literal, Precedence::None),
        // var
        none_prec!(),
        // while
        none_prec!(),
        // error
        none_prec!(),
        // eof
        none_prec!(),
    ];
    const UNINTIALIZED_LOCAL: MaybeUninit<Local<'src>> = MaybeUninit::uninit();

    pub fn new(src: &'src str, chunk: Chunk, strings: Table) -> Self {
        let scanner = Scanner::new(src);
        let parser = Parser::new();

        Self {
            obj_list: Default::default(),
            parser,
            scanner,
            chunk,
            interned_strings: strings,
            locals: [Self::UNINTIALIZED_LOCAL; u8::MAX as usize],
            local_count: 0,
            scope_depth: 0,
        }
    }

    fn get_rule(kind: TokenKind) -> &'src ParseRule<'src> {
        &Self::PARSE_RULES[kind as u8 as usize]
    }

    pub fn compile<'a>(&mut self) -> bool {
        self.advance();

        while !self.match_tok(TokenKind::Eof) {
            self.declaration();
        }

        self.end();
        !self.parser.had_error
    }

    fn synchronize(&mut self) {
        self.parser.panic_mode = false;

        while self.parser.cur().kind != TokenKind::Eof {
            if self.parser.prev().kind == TokenKind::Semicolon {
                return;
            }

            use TokenKind::*;
            match self.parser.cur().kind {
                Class | Fun | Var | For | If | While | Print | Return => return,
                _ => (),
            }

            self.advance()
        }
    }

    fn declaration(&mut self) {
        if self.match_tok(TokenKind::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.parser.panic_mode {
            self.synchronize();
        }
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.match_tok(TokenKind::Equal) {
            self.expression();
        } else {
            self.emit_byte(Opcode::Nil as u8);
        }

        self.consume(
            TokenKind::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(global);
    }

    fn parse_variable(&mut self, err_msg: &str) -> u8 {
        self.consume(TokenKind::Identifier, err_msg);

        self.declare_variable();
        if self.scope_depth > 0 {
            return 0;
        }

        let name = self.parser.prev();

        let mut had_error = false;
        for local in self.locals.iter().take(self.local_count).rev() {
            let local = unsafe { local.assume_init_ref() };
            if local.depth.is_some()
                && unsafe { local.depth.unwrap_unchecked() as usize } < self.scope_depth
            {
                break;
            }

            if name == local.name {
                had_error = true;
            }
        }

        if had_error {
            self.error("Already a variable with this name in this scope.");
        }

        self.identifier_constant(name)
    }

    fn declare_variable(&mut self) {
        if self.scope_depth == 0 {
            return;
        }

        self.add_local(&self.parser.prev());
    }

    fn add_local(&mut self, tok: &Token<'src>) {
        if self.local_count == u8::MAX as usize {
            self.error("Too many local variables in function.");
            return;
        }

        let local = self.locals[self.local_count].as_mut_ptr();
        self.local_count += 1;

        unsafe {
            (*local).name = *tok;
            (*local).depth = None;
        }
    }

    fn define_variable(&mut self, global: u8) {
        if self.scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        self.emit_bytes(Opcode::DefineGlobal as u8, global)
    }

    fn mark_initialized(&mut self) {
        let scope_depth = self.scope_depth;
        unsafe {
            self.locals[self.local_count - 1].assume_init_mut().depth = Some(scope_depth as u32);
        }
    }

    fn identifier_constant(&mut self, name: Token) -> u8 {
        let constant = Value::Obj(ObjString::copy_string(
            &mut self.interned_strings,
            &mut self.obj_list,
            name.msg,
        ) as *mut Obj);
        self.make_constant(constant)
    }

    fn statement(&mut self) {
        if self.match_tok(TokenKind::Print) {
            self.print_statement();
        } else if self.match_tok(TokenKind::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn block(&mut self) {
        while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eof) {
            self.declaration()
        }

        self.consume(TokenKind::RightBrace, "Expect '}' after block.")
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }
    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        while self.local_count > 0
            && unsafe {
                self.locals[self.local_count - 1]
                    .assume_init_ref()
                    .depth
                    .map(|val| val as isize)
                    .unwrap_or(-1)
            } > self.scope_depth as isize
        {
            self.emit_byte(Opcode::Pop as u8);
            self.local_count -= 1;
        }
    }

    fn match_tok(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenKind::Semicolon, "Expect ';' after value.");
        self.emit_byte(Opcode::Print as u8)
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenKind::Semicolon, "Expect ';' after expression.");
        self.emit_byte(Opcode::Pop as u8)
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.parser.cur().kind == kind
    }

    fn end(&mut self) {
        self.emit_return();
        #[cfg(debug_assertions)]
        {
            if !self.parser.had_error {
                println!("{:?}", self.chunk);
            }
        }
    }

    fn emit_return(&mut self) {
        self.emit_byte(Opcode::Return as u8)
    }

    fn emit_byte(&mut self, byte: u8) {
        self.chunk.write(byte, self.parser.prev().line)
    }

    fn emit_bytes(&mut self, a: u8, b: u8) {
        self.emit_byte(a);
        self.emit_byte(b)
    }

    fn emit_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);
        self.emit_bytes(Opcode::Constant as u8, constant);
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        let constant_idx = self.chunk.add_constant(value);
        if constant_idx >= u8::MAX {
            self.error("Too many constants in one chunk");
            return 0;
        }

        constant_idx
    }

    fn consume(&mut self, kind: TokenKind, msg: &str) {
        if self.parser.cur().kind == kind {
            self.advance();
            return;
        }

        self.error_at_current(msg)
    }

    fn advance(&mut self) {
        self.parser.prev = self.parser.cur;

        loop {
            self.parser.cur = MaybeUninit::new(self.scanner.token());
            if self.parser.cur().kind != TokenKind::Error {
                break;
            }

            self.error_at_current(self.parser.cur().msg)
        }
    }

    fn error_at_current(&mut self, msg: &str) {
        self.error_at(self.parser.cur(), msg)
    }

    fn error(&mut self, msg: &str) {
        self.error_at(self.parser.prev(), msg)
    }

    fn error_at(&mut self, token: Token<'src>, msg: &str) {
        if self.parser.panic_mode {
            return;
        }

        self.parser.panic_mode = true;

        eprint!("[line {}] Error", token.line);

        if token.kind == TokenKind::Eof {
            eprint!(" at end")
        } else if token.kind == TokenKind::Error {
        } else {
            eprint!(" at {}", token.msg)
        }

        eprint!(": {}\n", msg);
        self.parser.had_error = true;
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let rule = match Self::get_rule(self.parser.prev().kind).prefix {
            Some(rule) => rule,
            None => {
                self.error("Expect expression");
                return;
            }
        };

        let ctx = ParseRuleCtx {
            can_assign: precedence as u8 <= Precedence::Assignment as u8,
        };
        rule(self, ctx);

        while precedence as u8 <= Self::get_rule(self.parser.cur().kind).precedence as u8 {
            self.advance();
            let infix_rule = match Self::get_rule(self.parser.prev().kind).infix {
                Some(rule) => rule,
                None => panic!(),
            };
            infix_rule(self, ctx);
        }

        if ctx.can_assign && self.match_tok(TokenKind::Equal) {
            self.error("Invalid assignment target.");
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment)
    }

    fn variable(&mut self, ctx: ParseRuleCtx) {
        self.named_variable(self.parser.prev(), ctx);
    }

    fn named_variable(&mut self, name: Token, ctx: ParseRuleCtx) {
        let (mut get_op, mut set_op) = (Opcode::GetGlobal as u8, Opcode::SetGlobal as u8);

        let arg = match self.resolve_local(name) {
            Some(arg) => {
                get_op = Opcode::GetLocal as u8;
                set_op = Opcode::SetLocal as u8;
                arg
            }
            None => self.identifier_constant(name),
        };

        if ctx.can_assign && self.match_tok(TokenKind::Equal) {
            self.expression();
            self.emit_bytes(set_op, arg);
        } else {
            self.emit_bytes(get_op, arg);
        }
    }

    fn resolve_local(&mut self, name: Token) -> Option<u8> {
        for (i, local) in self.locals.iter().enumerate().take(self.local_count).rev() {
            let local = unsafe { local.assume_init_ref() };
            if local.name == name {
                if local.depth.is_none() {
                    self.error("Can't read local variable in its own initializer.");
                }
                return Some(i as u8);
            }
        }

        return None;
    }

    fn number(&mut self, ctx: ParseRuleCtx) {
        let value: f64 = self.parser.prev().msg.parse().unwrap();
        self.emit_constant(value.into())
    }

    fn string(&mut self, ctx: ParseRuleCtx) {
        let string = self.parser.prev().msg;

        // get rid of the quotations
        let obj_str = ObjString::copy_string(
            &mut self.interned_strings,
            &mut self.obj_list,
            &string[1..string.len() - 1],
        ) as *mut Obj;

        self.emit_constant(Value::Obj(obj_str));
    }

    fn literal(&mut self, ctx: ParseRuleCtx) {
        match self.parser.prev().kind {
            TokenKind::True => self.emit_byte(Opcode::True as u8),
            TokenKind::False => self.emit_byte(Opcode::False as u8),
            TokenKind::Nil => self.emit_byte(Opcode::Nil as u8),
            _ => return,
        }
    }

    fn grouping(&mut self, ctx: ParseRuleCtx) {
        self.expression();
        self.consume(TokenKind::RightParen, "Expect ')' after expression.")
    }

    fn unary(&mut self, ctx: ParseRuleCtx) {
        let op_kind = self.parser.prev().kind;

        self.parse_precedence(Precedence::Unary);

        match op_kind {
            TokenKind::Minus => self.emit_byte(Opcode::Negate as u8),
            TokenKind::Bang => self.emit_byte(Opcode::Not as u8),
            _ => (),
        }
    }

    fn binary(&mut self, ctx: ParseRuleCtx) {
        let op_kind = self.parser.prev().kind;
        let rule: &ParseRule<'src> = Self::get_rule(op_kind);
        self.parse_precedence(Precedence::from_u8(rule.precedence as u8 + 1).unwrap());

        match op_kind {
            TokenKind::BangEqual => self.emit_bytes(Opcode::Equal as u8, Opcode::Not as u8),
            TokenKind::EqualEqual => self.emit_byte(Opcode::Equal as u8),
            TokenKind::Greater => self.emit_byte(Opcode::Greater as u8),
            TokenKind::GreaterEqual => self.emit_bytes(Opcode::Less as u8, Opcode::Not as u8),
            TokenKind::Less => self.emit_byte(Opcode::Less as u8),
            TokenKind::LessEqual => self.emit_bytes(Opcode::Greater as u8, Opcode::Not as u8),
            TokenKind::Plus => self.emit_byte(Opcode::Add as u8),
            TokenKind::Minus => self.emit_byte(Opcode::Subtract as u8),
            TokenKind::Star => self.emit_byte(Opcode::Multiply as u8),
            TokenKind::Slash => self.emit_byte(Opcode::Divide as u8),
            other => unreachable!("{:?}", other),
        }
    }
}

pub struct Parser<'src> {
    // probably a bad idea to make maybeuninit but 2 lazy rn
    cur: MaybeUninit<Token<'src>>,
    prev: MaybeUninit<Token<'src>>,

    had_error: bool,
    panic_mode: bool,
}

impl<'src> Parser<'src> {
    pub fn new() -> Self {
        Self {
            cur: MaybeUninit::uninit(),
            prev: MaybeUninit::uninit(),
            had_error: false,
            panic_mode: false,
        }
    }

    #[inline]
    fn cur(&self) -> Token<'src> {
        unsafe { self.cur.assume_init() }
    }

    #[inline]
    fn prev(&self) -> Token<'src> {
        unsafe { self.prev.assume_init() }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // Single-character tokens.
    LeftParen = 0,
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

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Token<'src> {
    kind: TokenKind,
    line: u32,
    msg: &'src str,
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

        let noob = c as char;
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
            && &self.src[(self.start + start)..(self.start + start + len)] == rest.as_bytes()
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
