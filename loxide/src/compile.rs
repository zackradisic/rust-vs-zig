use std::{
    mem::MaybeUninit,
    ptr::{addr_of_mut, null_mut, NonNull},
};

use crate::{
    chunk::{Chunk, Opcode},
    mem::Mem,
    obj::ObjFunction,
    value::Value,
};

#[derive(Debug, Clone, Copy)]
struct ParseRuleCtx {
    can_assign: bool,
}

type ParseFn<'a, 'src> = fn(&mut Parser<'a, 'src>, ParseRuleCtx);

pub struct ParseRule<'a, 'src> {
    prefix: Option<ParseFn<'a, 'src>>,
    infix: Option<ParseFn<'a, 'src>>,
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
            prefix: None as Option<ParseFn<'a, 'src>>,
            infix: None as Option<ParseFn<'a, 'src>>,
            precedence: Precedence::None,
        } as ParseRule<'a, 'src>
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
    is_captured: bool,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FunctionKind {
    Method,
    Function,
    Script,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionKindT<T> {
    Function(T),
    Method(T),
    Script,
}

pub struct Locals<'src> {
    stack: [MaybeUninit<Local<'src>>; u8::MAX as usize],
    count: u8,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Upvalue {
    // callframe-relative index in the stack to where this value is
    pub index: u8,
    // `false` when the upvalue captures another upvalue
    pub is_local: bool,
}

pub struct Compiler<'src> {
    pub function: NonNull<ObjFunction>,
    enclosing: Option<Box<Compiler<'src>>>,
    function_kind: FunctionKind,
    locals: Locals<'src>,
    scope_depth: usize,
    upvalues: [MaybeUninit<Upvalue>; u8::MAX as usize],
}

impl<'src> Compiler<'src> {
    const UNINTIALIZED_LOCAL: MaybeUninit<Local<'src>> = MaybeUninit::uninit();
    const UNINTIALIZED_UPVALUE: MaybeUninit<Upvalue> = MaybeUninit::uninit();

    pub fn new(function_kind: FunctionKindT<Token>, mem: &mut Mem) -> Self {
        let (function_name, function_kind) = match function_kind {
            FunctionKindT::Function(prev_tok) => (
                mem.copy_string(prev_tok.msg).as_ptr(),
                FunctionKind::Function,
            ),
            FunctionKindT::Method(prev_tok) => {
                (mem.copy_string(prev_tok.msg).as_ptr(), FunctionKind::Method)
            }
            FunctionKindT::Script => (null_mut(), FunctionKind::Script),
        };

        let function = mem.alloc_obj(ObjFunction::new(function_name));

        let mut this = Self {
            enclosing: None,
            function,
            function_kind,
            locals: Locals {
                stack: [Self::UNINTIALIZED_LOCAL; u8::MAX as usize],
                count: 0,
            },
            scope_depth: 0,
            upvalues: [Self::UNINTIALIZED_UPVALUE; u8::MAX as usize],
        };

        // Safety:
        // It is UB to create reference to uninitialized memory so we set this
        // value through raw pointer
        unsafe {
            let mut local_ptr = this.locals.stack[0].as_mut_ptr();
            (*local_ptr).is_captured = false;
            (*local_ptr).depth = Some(0);
            (*local_ptr).name = Token {
                kind: TokenKind::Nil,
                line: 0,
                msg: if function_kind != FunctionKind::Function {
                    "this"
                } else {
                    ""
                },
            };
        }
        this.locals.count += 1;

        this
    }

    #[inline]
    pub fn current_chunk_ptr(&self) -> *mut Chunk {
        unsafe { addr_of_mut!((*self.function.as_ptr()).chunk) }
    }

    #[inline]
    fn current_chunk(&self) -> &Chunk {
        unsafe { &(*self.function.as_ptr()).chunk }
    }

    #[inline]
    fn current_chunk_mut(&self) -> &mut Chunk {
        unsafe { &mut (*self.function.as_ptr()).chunk }
    }

    #[inline]
    fn current_fn(&self) -> &ObjFunction {
        unsafe { self.function.as_ref() }
    }

    #[inline]
    fn current_fn_mut(&mut self) -> &mut ObjFunction {
        unsafe { self.function.as_mut() }
    }

    /// Add up value and return index in compiler's upvalue array
    fn add_up_value(&mut self, index: u8, is_local: bool, errors: &mut Vec<&str>) -> u8 {
        unsafe {
            let upvalue_count = self.function.as_ref().upvalue_count;

            // check if it exists already
            for (i, upvalue) in self
                .upvalues
                .iter()
                .enumerate()
                .take(upvalue_count as usize)
                .rev()
            {
                let upvalue = upvalue.assume_init();
                if upvalue.index == index && upvalue.is_local {
                    return i as u8;
                }
            }

            if upvalue_count == u8::MAX {
                errors.push("Too many closure variables in function.");
                return 0;
            }

            let upvalue_ptr = self.upvalues[upvalue_count as usize].as_mut_ptr();

            (*upvalue_ptr).is_local = is_local;
            (*upvalue_ptr).index = index;

            self.function.as_mut().upvalue_count += 1;
            upvalue_count
        }
    }

    /// resolves/creates upvalue by recursively travelling upwards in scope
    /// returns the index of the upvalue in its corresponding Compiler array
    ///
    /// this creates a chain of upvalues from this scope to the outer scope where the variable is
    fn resolve_upvalue(&mut self, name: Token, errors: &mut Vec<&str>) -> Option<u8> {
        let enclosing = match &mut self.enclosing {
            Some(enclosing) => enclosing,
            None => return None,
        };

        match enclosing.resolve_local(name, errors) {
            Some(local) => {
                unsafe {
                    let local = enclosing.locals.stack[local as usize].assume_init_mut();
                    local.is_captured = true;
                }
                Some(self.add_up_value(local, true, errors))
            }
            // recurse
            None => enclosing
                .resolve_upvalue(name, errors)
                .map(|index| self.add_up_value(index, false, errors)),
        }
    }

    fn resolve_local(&mut self, name: Token, errors: &mut Vec<&str>) -> Option<u8> {
        for (i, local) in self
            .locals
            .stack
            .iter()
            .enumerate()
            .take(self.locals.count as usize)
            .rev()
        {
            let local = unsafe { local.assume_init_ref() };
            if local.name.msg == name.msg {
                if local.depth.is_none() {
                    errors.push("Can't read local variable in its own initializer.");
                }
                return Some(i as u8);
            }
        }

        None
    }
}

pub struct Parser<'a, 'src> {
    pub compiler: Box<Compiler<'src>>,
    mem: &'a mut Mem,
    scanner: Scanner<'src>,

    // probably a bad idea to make maybeuninit but 2 lazy rn
    cur: MaybeUninit<Token<'src>>,
    prev: MaybeUninit<Token<'src>>,

    had_error: bool,
    panic_mode: bool,
}

impl<'a, 'src: 'a> Parser<'a, 'src> {
    pub const PARSE_RULES: [ParseRule<'a, 'src>; 40] = [
        // left paren
        parse_rule!(pre = Parser::grouping, inf = Parser::call, Precedence::Call),
        // right paren
        none_prec!(),
        // left brace
        none_prec!(),
        // right brace
        none_prec!(),
        // comma
        none_prec!(),
        // dot
        parse_rule!(inf = Parser::dot, Precedence::Call),
        // minus
        parse_rule!(pre = Parser::unary, inf = Parser::binary, Precedence::Term),
        // plus
        parse_rule!(inf = Parser::binary, Precedence::Term),
        // semicolon
        none_prec!(),
        // slash
        parse_rule!(inf = Parser::binary, Precedence::Factor),
        // star
        parse_rule!(inf = Parser::binary, Precedence::Factor),
        // bang
        parse_rule!(pre = Parser::unary, Precedence::None),
        // bangequal
        parse_rule!(inf = Parser::binary, Precedence::Equality),
        // equal
        parse_rule!(inf = Parser::binary, Precedence::Equality),
        // equalequal
        parse_rule!(inf = Parser::binary, Precedence::Comparison),
        // greater
        parse_rule!(inf = Parser::binary, Precedence::Comparison),
        // greaterequal
        parse_rule!(inf = Parser::binary, Precedence::Comparison),
        // less
        parse_rule!(inf = Parser::binary, Precedence::Comparison),
        // lessequal
        parse_rule!(inf = Parser::binary, Precedence::Comparison),
        // identifier
        parse_rule!(pre = Parser::variable, Precedence::None),
        // string
        parse_rule!(pre = Parser::string, Precedence::None),
        // number
        parse_rule!(pre = Parser::number, Precedence::None),
        // and
        parse_rule!(inf = Parser::and, Precedence::And),
        // class
        none_prec!(),
        // else
        none_prec!(),
        // false
        parse_rule!(pre = Parser::literal, Precedence::None),
        // for
        none_prec!(),
        // fun
        none_prec!(),
        // if
        none_prec!(),
        // nil
        parse_rule!(pre = Parser::literal, Precedence::None),
        // or
        parse_rule!(inf = Parser::or, Precedence::Or),
        // print
        none_prec!(),
        // return
        none_prec!(),
        // super
        none_prec!(),
        // this
        parse_rule!(pre = Parser::this, Precedence::None),
        // true
        parse_rule!(pre = Parser::literal, Precedence::None),
        // var
        none_prec!(),
        // while
        none_prec!(),
        // error
        none_prec!(),
        // eof
        none_prec!(),
    ];

    pub fn new(src: &'src str, mem: &'a mut Mem) -> Self {
        let scanner = Scanner::new(src);
        let compiler = Box::new(Compiler::new(FunctionKindT::Script, mem));

        Self {
            compiler,
            mem,
            scanner,
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

    pub fn compile(&mut self) -> bool {
        self.advance();

        while !self.match_tok(TokenKind::Eof) {
            self.declaration();
        }

        self.end();
        !self.had_error
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while self.cur().kind != TokenKind::Eof {
            if self.prev().kind == TokenKind::Semicolon {
                return;
            }

            use TokenKind::*;
            match self.cur().kind {
                Class | Fun | Var | For | If | While | Print | Return => return,
                _ => (),
            }

            self.advance()
        }
    }

    fn handle_errors(&mut self, mut errors: Vec<&str>) {
        while let Some(err) = errors.pop() {
            self.error(err);
        }
    }

    fn resolve_local(&mut self, name: Token) -> Option<u8> {
        let mut errors = vec![];
        let ret = self.compiler.resolve_local(name, &mut errors);
        self.handle_errors(errors);
        ret
    }

    fn resolve_upvalue(&mut self, name: Token) -> Option<u8> {
        let mut errors = vec![];
        let ret = self.compiler.resolve_upvalue(name, &mut errors);
        self.handle_errors(errors);
        ret
    }

    fn get_rule(kind: TokenKind) -> &'a ParseRule<'a, 'src> {
        &Self::PARSE_RULES[kind as u8 as usize]
    }

    fn named_variable(&mut self, name: Token, ctx: ParseRuleCtx) {
        let (arg, get_op, set_op) = match self.resolve_local(name) {
            Some(arg) => (arg, Opcode::GetLocal as u8, Opcode::SetLocal as u8),
            None => self
                .resolve_upvalue(name)
                .map(|arg| (arg, Opcode::GetUpvalue as u8, Opcode::SetUpvalue as u8))
                .unwrap_or_else(|| {
                    (
                        self.identifier_constant(name),
                        Opcode::GetGlobal as u8,
                        Opcode::SetGlobal as u8,
                    )
                }),
        };

        if ctx.can_assign && self.match_tok(TokenKind::Equal) {
            self.expression();
            self.emit_bytes(set_op, arg);
        } else {
            self.emit_bytes(get_op, arg);
        }
    }

    fn declaration(&mut self) {
        if self.match_tok(TokenKind::Class) {
            self.class_declaration()
        } else if self.match_tok(TokenKind::Fun) {
            self.fn_declaration();
        } else if self.match_tok(TokenKind::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn class_declaration(&mut self) {
        self.consume(TokenKind::Identifier, "Expect class name.");
        let class_name = self.prev();
        let name_constant = self.identifier_constant(self.prev());
        self.declare_variable();

        self.emit_bytes(Opcode::Class as u8, name_constant);
        self.define_variable(name_constant);
        self.named_variable(class_name, ParseRuleCtx { can_assign: false });

        self.consume(TokenKind::LeftBrace, "Expect '{' before class body.");
        while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eof) {
            self.method();
        }
        self.consume(TokenKind::RightBrace, "Expect '}' after class body.");
        self.emit_byte(Opcode::Pop as u8);
    }

    fn method(&mut self) {
        self.consume(TokenKind::Identifier, "Expect method name.");
        let constant = self.identifier_constant(self.prev());

        let kind = FunctionKind::Method;
        self.function(kind);
        self.emit_bytes(Opcode::Method as u8, constant);
    }

    fn fn_declaration(&mut self) {
        let global = self.parse_variable("Expect function name.");
        self.mark_initialized();
        self.function(FunctionKind::Function);
        self.define_variable(global);
    }

    fn and(&mut self, _ctx: ParseRuleCtx) {
        let end_jump = self.emit_jump(Opcode::JumpIfFalse as u8);
        self.emit_byte(Opcode::Pop as u8);
        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
    }

    fn or(&mut self, _ctx: ParseRuleCtx) {
        let else_jump = self.emit_jump(Opcode::Jump as u8);
        let end_jump = self.emit_jump(Opcode::Jump as u8);

        self.patch_jump(else_jump);
        self.emit_byte(Opcode::Pop as u8);

        self.parse_precedence(Precedence::And);

        self.patch_jump(end_jump);
    }

    fn number(&mut self, _ctx: ParseRuleCtx) {
        let value: f64 = self.prev().msg.parse().unwrap();
        self.emit_constant(value.into())
    }

    fn string(&mut self, _ctx: ParseRuleCtx) {
        let string = self.prev().msg;

        // get rid of the quotations
        let obj_str = self.mem.copy_string(&string[1..string.len() - 1]);

        self.emit_constant(Value::Obj(obj_str.cast()));
    }

    fn literal(&mut self, _ctx: ParseRuleCtx) {
        match self.prev().kind {
            TokenKind::True => self.emit_byte(Opcode::True as u8),
            TokenKind::False => self.emit_byte(Opcode::False as u8),
            TokenKind::Nil => self.emit_byte(Opcode::Nil as u8),
            _ => (),
        }
    }

    fn grouping(&mut self, _ctx: ParseRuleCtx) {
        self.expression();
        self.consume(TokenKind::RightParen, "Expect ')' after expression.")
    }

    fn call(&mut self, _ctx: ParseRuleCtx) {
        let arg_count = self.argument_list();
        self.emit_bytes(Opcode::Call as u8, arg_count);
    }

    fn argument_list(&mut self) -> u8 {
        let mut arg_count = 0;
        if !self.check(TokenKind::RightParen) {
            loop {
                self.expression();
                if arg_count == u8::MAX {
                    self.error("Can't have more than 255 arguments");
                }
                arg_count += 1;
                if !self.match_tok(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenKind::RightParen, "Expect ')' after arguments.");

        arg_count
    }

    fn dot(&mut self, ctx: ParseRuleCtx) {
        self.consume(TokenKind::Identifier, "Expect property name after '.'.");
        let name = self.identifier_constant(self.prev());

        if ctx.can_assign && self.match_tok(TokenKind::Equal) {
            self.expression();
            self.emit_bytes(Opcode::SetProperty as u8, name);
        } else {
            self.emit_bytes(Opcode::GetProperty as u8, name);
        }
    }

    fn unary(&mut self, _ctx: ParseRuleCtx) {
        let op_kind = self.prev().kind;

        self.parse_precedence(Precedence::Unary);

        match op_kind {
            TokenKind::Minus => self.emit_byte(Opcode::Negate as u8),
            TokenKind::Bang => self.emit_byte(Opcode::Not as u8),
            _ => (),
        }
    }

    fn binary(&mut self, _ctx: ParseRuleCtx) {
        let op_kind = self.prev().kind;
        let rule = Self::get_rule(op_kind);
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

    fn function(&mut self, kind: FunctionKind) {
        let kindt = match kind {
            FunctionKind::Function => FunctionKindT::Function(self.prev()),
            FunctionKind::Method => FunctionKindT::Method(self.prev()),
            FunctionKind::Script => FunctionKindT::Script,
        };

        let temp_compiler =
            std::mem::replace(&mut self.compiler, Box::new(Compiler::new(kindt, self.mem)));
        self.compiler.enclosing = Some(temp_compiler);

        self.begin_scope();

        self.consume(TokenKind::LeftParen, "Expect '(' after function name.");
        if !self.check(TokenKind::RightParen) {
            loop {
                match self.compiler.current_fn().arity.checked_add(1) {
                    Some(new_arity) => {
                        self.compiler.current_fn_mut().arity = new_arity;
                    }
                    None => {
                        self.error_at_current("Can't have more than 255 parameters");
                    }
                };

                let constant = self.parse_variable("Expect parameter name.");
                self.define_variable(constant);
                if !self.match_tok(TokenKind::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenKind::RightParen, "Expect ')' after parameters.");
        self.consume(TokenKind::LeftBrace, "Expect '{' before function body.");

        self.block();

        self.end();

        let func = self.compiler.function;
        // back to the original compiler
        let temp_compiler = self.compiler.enclosing.take().unwrap();
        let temp_compiler = std::mem::replace(&mut self.compiler, temp_compiler);

        let val = self.make_constant(Value::Obj(func.cast()));
        self.emit_bytes(Opcode::Closure as u8, val);

        let upvalue_count = unsafe { func.as_ref().upvalue_count };
        let _func_name = unsafe {
            func.as_ref()
                .name
                .as_ref()
                .map(|obj| obj.as_str())
                .unwrap_or("top_level")
        };
        for i in 0..upvalue_count {
            let upvalue = unsafe { temp_compiler.upvalues[i as usize].assume_init() };
            self.emit_byte(if upvalue.is_local { 1 } else { 0 });
            self.emit_byte(upvalue.index);
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
        if self.compiler.scope_depth > 0 {
            return 0;
        }

        let name = self.prev();

        let mut had_error = false;
        for local in self
            .compiler
            .locals
            .stack
            .iter()
            .take(self.compiler.locals.count as usize)
            .rev()
        {
            let local = unsafe { local.assume_init_ref() };
            if local.depth.is_some()
                && unsafe { local.depth.unwrap_unchecked() as usize } < self.compiler.scope_depth
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
        if self.compiler.scope_depth == 0 {
            return;
        }

        self.add_local(&self.prev());
    }

    fn add_local(&mut self, tok: &Token<'src>) {
        if self.compiler.locals.count == u8::MAX {
            self.error("Too many local variables in function.");
            return;
        }

        let local = self.compiler.locals.stack[self.compiler.locals.count as usize].as_mut_ptr();
        self.compiler.locals.count += 1;

        unsafe {
            (*local).name = *tok;
            (*local).depth = None;
            (*local).is_captured = false;
        }
    }

    fn define_variable(&mut self, global: u8) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        self.emit_bytes(Opcode::DefineGlobal as u8, global)
    }

    fn mark_initialized(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }
        let scope_depth = self.compiler.scope_depth;
        unsafe {
            self.compiler.locals.stack[self.compiler.locals.count as usize - 1]
                .assume_init_mut()
                .depth = Some(scope_depth as u32);
        }
    }

    fn identifier_constant(&mut self, name: Token) -> u8 {
        let constant = Value::Obj(self.mem.copy_string(name.msg).cast());
        self.make_constant(constant)
    }

    fn statement(&mut self) {
        if self.match_tok(TokenKind::Print) {
            self.print_statement();
        } else if self.match_tok(TokenKind::For) {
            self.for_statement();
        } else if self.match_tok(TokenKind::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else if self.match_tok(TokenKind::If) {
            self.if_statement();
        } else if self.match_tok(TokenKind::Return) {
            self.return_statement();
        } else if self.match_tok(TokenKind::While) {
            self.while_statement();
        } else {
            self.expression_statement();
        }
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(Opcode::Loop as u8);

        let offset = self.compiler.current_chunk().len() - loop_start + 2;
        if offset > u16::MAX as usize {
            self.error("Loop body too large.");
        }

        self.emit_byte(((offset as u16) >> 8) as u8);
        self.emit_byte(offset as u8);
    }

    fn for_statement(&mut self) {
        self.begin_scope();

        self.consume(TokenKind::LeftParen, "Expect '(' after 'for'.");

        // Handle the initializer caluse
        if self.match_tok(TokenKind::Semicolon) {
            // No initializer
        } else if self.match_tok(TokenKind::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.compiler.current_chunk().len();

        // Handle the loop condition
        let exit_jump = if !self.match_tok(TokenKind::Semicolon) {
            self.expression();
            self.consume(TokenKind::Semicolon, "Expect ';' after loop condition.");

            let exit_jump = self.emit_jump(Opcode::JumpIfFalse as u8);
            self.emit_byte(Opcode::Pop as u8);
            Some(exit_jump)
        } else {
            None
        };

        // Increment clause:
        // single-pass compilation so we have to jump over incremeent,
        // run body, then jump back
        if !self.match_tok(TokenKind::RightParen) {
            let body_jump = self.emit_jump(Opcode::Jump as u8);
            let increment_start = self.compiler.current_chunk().len();

            self.expression();
            // discard value from increment caluse
            self.emit_byte(Opcode::Pop as u8);

            self.consume(TokenKind::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        self.emit_loop(loop_start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.emit_byte(Opcode::Pop as u8);
        }

        self.end_scope();
    }

    fn while_statement(&mut self) {
        let loop_start = self.compiler.current_chunk().len();

        self.consume(TokenKind::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenKind::RightParen, "Expect ')' after condition.");

        let exit_jump = self.emit_jump(Opcode::JumpIfFalse as u8);
        self.emit_byte(Opcode::Pop as u8);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_byte(Opcode::Pop as u8);
    }

    fn return_statement(&mut self) {
        if self.compiler.function_kind == FunctionKind::Script {
            self.error("Can't return from top-level code.");
        }

        if self.match_tok(TokenKind::Semicolon) {
            self.emit_return();
        } else {
            self.expression();
            self.consume(TokenKind::Semicolon, "Expect ';' after return value.");
            self.emit_byte(Opcode::Return as u8);
        }
    }

    fn if_statement(&mut self) {
        self.consume(TokenKind::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenKind::RightParen, "Expect ')' after condition.");

        // then_jump -> pop -> then stmt -> else_jump -> pop -> else
        let then_jump = self.emit_jump(Opcode::JumpIfFalse as u8);
        self.emit_byte(Opcode::Pop as u8);
        self.statement();

        let else_jump = self.emit_jump(Opcode::Jump as u8);

        self.patch_jump(then_jump);
        self.emit_byte(Opcode::Pop as u8);

        if self.match_tok(TokenKind::Else) {
            self.statement();
        }

        self.patch_jump(else_jump);
    }

    fn emit_jump(&mut self, instr: u8) -> u32 {
        self.emit_byte(instr);

        // using 2 bytes as a 16 bit int for the offset to jump
        // this will be patched later
        self.emit_byte(0xff);
        self.emit_byte(0xff);

        // returns index of the first byte of the jump offset
        self.compiler.current_chunk().len() as u32 - 2
    }

    fn patch_jump(&mut self, offset: u32) {
        // -2 to adjust for the 2 bytes for the jump offset
        // this will be the index just before the next instruction
        let jump = self.compiler.current_chunk().len() as u32 - offset - 2;

        if jump > u16::MAX as u32 {
            self.error("Too much code to jump over.");
        }

        self.compiler.current_chunk_mut().code[offset as usize] = (jump >> 8) as u8;
        self.compiler.current_chunk_mut().code[offset as usize + 1] = jump as u8;
    }

    fn block(&mut self) {
        while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eof) {
            self.declaration()
        }

        self.consume(TokenKind::RightBrace, "Expect '}' after block.")
    }

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }
    fn end_scope(&mut self) {
        self.compiler.scope_depth -= 1;

        while self.compiler.locals.count > 0
            && unsafe {
                self.compiler.locals.stack[self.compiler.locals.count as usize - 1]
                    .assume_init_ref()
                    .depth
                    .map(|val| val as isize)
                    .unwrap_or(-1)
            } > self.compiler.scope_depth as isize
        {
            let is_captured = unsafe {
                self.compiler.locals.stack[self.compiler.locals.count as usize - 1]
                    .assume_init_ref()
                    .is_captured
            };
            self.emit_byte(if is_captured {
                Opcode::CloseUpvalue as u8
            } else {
                Opcode::Pop as u8
            });
            self.compiler.locals.count -= 1;
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
        self.cur().kind == kind
    }

    fn end(&mut self) {
        self.emit_return();
        #[cfg(debug_assertions)]
        {
            if !self.had_error {
                unsafe {
                    let name = self
                        .compiler
                        .function
                        .as_ref()
                        .name
                        .as_ref()
                        .map(|s| s.as_str())
                        .unwrap_or("script");
                    println!(
                        "{} :: {:#?} :: upvalues :: {:#?}",
                        name,
                        self.compiler.current_chunk(),
                        self.compiler
                            .upvalues
                            .iter()
                            .take(self.compiler.current_fn().upvalue_count as usize)
                            .map(|val| val.assume_init())
                            .collect::<Vec<_>>()
                    );
                }
            }
        }
    }

    /// This is badly named. This function is called if a return statement has no expression,
    /// we emit nil to make the function implicitly return nil
    ///
    /// If the function has a return statement, that is handled in `self.return_statement()` function
    fn emit_return(&mut self) {
        self.emit_byte(Opcode::Nil as u8);
        self.emit_byte(Opcode::Return as u8)
    }

    fn emit_byte(&mut self, byte: u8) {
        self.compiler
            .current_chunk_mut()
            .write(byte, self.prev().line)
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
        let constant_idx = self.compiler.current_chunk_mut().add_constant(value);
        if constant_idx >= u8::MAX {
            self.error("Too many constants in one chunk");
            return 0;
        }

        constant_idx
    }

    fn consume(&mut self, kind: TokenKind, msg: &str) {
        if self.cur().kind == kind {
            self.advance();
            return;
        }

        self.error_at_current(msg)
    }

    fn advance(&mut self) {
        self.prev = self.cur;

        loop {
            self.cur = MaybeUninit::new(self.scanner.token());
            if self.cur().kind != TokenKind::Error {
                break;
            }

            self.error_at_current(self.cur().msg)
        }
    }

    fn error_at_current(&mut self, msg: &str) {
        self.error_at(self.cur(), msg)
    }

    fn error(&mut self, msg: &str) {
        self.error_at(self.prev(), msg)
    }

    fn error_at(&mut self, token: Token<'src>, msg: &str) {
        if self.panic_mode {
            return;
        }

        self.panic_mode = true;

        eprint!("[line {}] Error", token.line);

        if token.kind == TokenKind::Eof {
            eprint!(" at end")
        } else if token.kind == TokenKind::Error {
        } else {
            eprint!(" at {}", token.msg)
        }

        eprintln!(": {msg}");
        self.had_error = true;
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let rule = match Self::get_rule(self.prev().kind).prefix {
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

        while precedence as u8 <= Self::get_rule(self.cur().kind).precedence as u8 {
            self.advance();
            let infix_rule = match Self::get_rule(self.prev().kind).infix {
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
        self.named_variable(self.prev(), ctx);
    }

    fn this(&mut self, _ctx: ParseRuleCtx) {
        self.variable(ParseRuleCtx { can_assign: false })
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

        self.error_token("Unexpected character.")
    }

    fn is_alpha(c: u8) -> bool {
        (b'a'..=b'z').contains(&c) || (b'A'..=b'Z').contains(&c) || c == b'_'
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
            msg: unsafe { std::str::from_utf8_unchecked(&self.src[self.start..self.current]) },
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
