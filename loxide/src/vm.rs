use std::{
    alloc::{self, handle_alloc_error, Layout},
    borrow::Cow,
    mem::MaybeUninit,
    ptr::{self, addr_of_mut, null_mut, NonNull},
};

use crate::{
    chunk::{InstructionDebug, Opcode},
    mem::{Greystack, Mem},
    native_fn::NativeFnKind,
    obj::{
        Obj, ObjBoundMethod, ObjClass, ObjClosure, ObjFunction, ObjInstance, ObjKind, ObjNative,
        ObjPunnable, ObjString, ObjUpvalue,
    },
    table::ObjHash,
    value::Value,
};

const GC_HEAP_GROW_FACTOR: usize = 2;

pub type InterpretResult<T> = Result<T, InterpretError>;

#[derive(Debug)]
pub enum InterpretError {
    RuntimeError,
    CompileError,
}

#[derive(Debug, Copy, Clone)]
pub struct CallFrame {
    /// PERF: Instruction pointer is faster to dereference than index
    /// instruction index of caller, this is used for returns, function will jump back to this index
    /// and resume from there
    pub instr_offset: u32,

    /// PERF: pointer is faster to dereference than index
    ///
    /// index into VM's value stack at the first slot this function can use
    pub slots_offset: u32,

    pub closure: NonNull<ObjClosure>,
}

impl CallFrame {
    #[inline]
    fn function(&self) -> &ObjFunction {
        unsafe { self.closure.as_ref().function.as_ref() }
    }
    #[inline]
    fn closure(&self) -> &ObjClosure {
        unsafe { self.closure.as_ref() }
    }
}

pub const U8_COUNT: usize = (u8::MAX) as usize + 1; // 256
const FRAMES_MAX: usize = 64;
pub const STACK_MAX: usize = 64 * U8_COUNT;
pub static mut STACK: [MaybeUninit<Value>; STACK_MAX] = [MaybeUninit::uninit(); STACK_MAX];
pub type ValueStack = [MaybeUninit<Value>; STACK_MAX];

pub struct VM<'b> {
    pub stack: &'b mut [MaybeUninit<Value>; STACK_MAX],
    marker: std::marker::PhantomData<&'b ()>,
    pub stack_top: u32,
    /// linked list of open upvalues for the top-most stack,
    /// this points to the top-most open upvalue
    /// (so last in this list is the first open upvalue on the stack)
    pub open_upvalues: *mut ObjUpvalue,

    pub call_frames: [MaybeUninit<CallFrame>; FRAMES_MAX],
    pub call_frame_count: u32,

    pub mem: Mem,
    pub grey_stack: Greystack,
}

impl<'b> VM<'b> {
    pub fn new(
        stack: &'b mut [MaybeUninit<Value>; STACK_MAX],
        mut mem: Mem,
        function: NonNull<ObjFunction>,
    ) -> Self {
        let closure = mem.alloc_obj(ObjClosure::new(function));

        let mut call_frames = [MaybeUninit::uninit(); FRAMES_MAX];
        call_frames[0] = MaybeUninit::new(CallFrame {
            instr_offset: 0,
            slots_offset: 0,
            closure,
        });

        stack[0] = MaybeUninit::new(Value::Obj(closure.cast()));

        let mut this = Self {
            mem,
            open_upvalues: null_mut(),
            stack,
            marker: Default::default(),
            stack_top: 1,
            call_frames,
            call_frame_count: 1,
            grey_stack: vec![],
        };

        this.define_native("clock", NativeFnKind::Clock);
        this.define_native("__dummy", NativeFnKind::Dummy);

        this
    }

    fn iter_stack(
        &self,
    ) -> std::iter::Map<
        std::iter::Take<std::slice::Iter<MaybeUninit<Value>>>,
        fn(&MaybeUninit<Value>) -> Value,
    > {
        self.stack
            .iter()
            .take(self.stack_top as usize)
            .map(|val| unsafe { val.assume_init_read() })
    }

    fn iter_frames(
        &self,
    ) -> std::iter::Map<
        std::iter::Take<std::slice::Iter<MaybeUninit<CallFrame>>>,
        fn(&MaybeUninit<CallFrame>) -> CallFrame,
    > {
        self.call_frames
            .iter()
            .take(self.call_frame_count as usize)
            .map(|val| unsafe { val.assume_init_read() })
    }

    fn trace_references(&mut self, greystack: &mut Greystack) {
        loop {
            if greystack.is_empty() {
                break;
            }

            // Safety:
            // We checked that the greystack is non-empty
            let obj = unsafe { greystack.pop().unwrap_unchecked() };
            unsafe { Obj::blacken(obj, greystack) };
        }
    }

    fn sweep(&mut self) {
        // Clear references to unmarked strings
        self.mem.interned_strings.remove_white();

        // Now free all unmarked objects
        let mut i = 0;
        loop {
            let mut obj_ptr = match self.mem.obj_list.get(i) {
                Some(obj) => *obj,
                None => break,
            };

            if unsafe { obj_ptr.as_ref().is_marked } {
                unsafe { obj_ptr.as_mut().is_marked = false };
                i += 1;
                continue;
            }

            self.mem.obj_list.remove(i);
            Obj::free(obj_ptr)
        }
    }

    fn mark_roots(&mut self, greystack: &mut Greystack) {
        for val in self.iter_stack() {
            val.mark(greystack);
        }

        for frame in self.iter_frames() {
            unsafe { Obj::mark(frame.closure.cast().as_ptr(), greystack) }
        }

        let mut upvalue = self.open_upvalues;
        while !upvalue.is_null() {
            unsafe {
                Obj::mark(upvalue.cast(), greystack);
                upvalue = (*upvalue).next;
            }
        }

        self.mem.globals.mark(greystack);
    }

    fn collect_garbage(&mut self) {
        #[cfg(feature = "debug_gc")]
        println!("-- gc begin");
        #[cfg(feature = "debug_gc")]
        let before = self.mem.bytes_allocated();

        let mut greystack = std::mem::take(&mut self.grey_stack);

        self.mark_roots(&mut greystack);
        self.trace_references(&mut greystack);
        self.sweep();

        self.mem.next_gc = self.mem.bytes_allocated() * GC_HEAP_GROW_FACTOR;

        #[cfg(feature = "debug_gc")]
        {
            println!("-- gc end");
            println!(
                "   collected {} bytes (from {} to {}) next at {}",
                before - self.mem.bytes_allocated(),
                before,
                self.mem.bytes_allocated(),
                self.mem.next_gc
            );
        }
    }

    fn alloc_obj<T: ObjPunnable>(&mut self, obj: T) -> NonNull<T> {
        if self.mem.should_run_gc::<T>() {
            #[cfg(feature = "debug_gc")]
            println!("Allocated a {:?}, now collecting garbage", obj.kind());
            self.collect_garbage();
        }

        self.mem.alloc_obj(obj)
    }

    fn alloc_obj_string(&mut self, obj_string: ObjString) -> NonNull<ObjString> {
        let ptr = self.alloc_obj(obj_string);
        self.mem.intern_string(ptr);
        ptr
    }

    fn take_string(&mut self, chars: NonNull<u8>, len: u32) -> NonNull<ObjString> {
        let hash = ObjHash::hash_string(unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(chars.as_ptr(), len as usize))
        });

        match self.mem.interned_strings.find_string(
            unsafe {
                std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                    chars.as_ptr(),
                    len as usize,
                ))
            },
            hash,
        ) {
            Some(interned) => {
                return interned;
            }
            None => (),
        }

        let obj_string = ObjString::new(chars, len, hash);
        self.alloc_obj_string(obj_string)
    }

    #[cfg(debug_assertions)]
    /// Only to be used for debugging purposes
    pub fn get_string(&mut self, string: &str) -> NonNull<ObjString> {
        self.mem.copy_string(string)
    }

    #[inline]
    fn push(&mut self, val: Value) {
        unsafe {
            *self.stack.as_mut_ptr().add(self.stack_top as usize) = MaybeUninit::new(val);
        }
        // self.stack[self.stack_top as usize] = MaybeUninit::new(val);
        self.stack_top += 1;
    }

    #[inline]
    fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        unsafe { MaybeUninit::assume_init(self.stack[self.stack_top as usize]) }
    }

    #[inline]
    fn binary_op<F: FnOnce(Value, Value) -> Value>(&mut self, f: F) -> InterpretResult<()> {
        if !matches!(self.peek(0), Value::Number(_)) || !matches!(self.peek(1), Value::Number(_)) {
            self.runtime_error("Operands must be two numbers or two strings.".into());
            return Err(InterpretError::RuntimeError);
        }

        let b = self.pop();
        let a = self.pop();
        self.push(f(a, b));

        Ok(())
    }

    #[inline]
    fn reset_stack(&mut self) {
        self.stack_top = 0;
        self.call_frame_count = 0;
        self.open_upvalues = null_mut();
    }

    fn runtime_error<'a>(&mut self, err: Cow<'a, str>) {
        eprintln!("{err}");

        let frame = self.top_call_frame();
        let instr_idx = frame.instr_offset;
        let line = frame.function().chunk.lines[instr_idx as usize];

        eprintln!("[line {line}] in script");

        for frame in self
            .call_frames
            .iter()
            .take(self.call_frame_count as usize - 1)
            .rev()
        {
            unsafe {
                let frame = frame.assume_init();
                let function = frame.function();
                let instruction = frame.instr_offset;
                eprintln!(
                    "[line {}] in {}",
                    function.chunk.lines[instruction as usize],
                    match function.name.as_ref() {
                        Some(name) => name.as_str(),
                        None => "script",
                    }
                )
            }
        }

        self.reset_stack();
    }

    fn peek(&self, distance: u32) -> Value {
        unsafe { self.stack[self.stack_top as usize - 1 - distance as usize].assume_init() }
    }

    fn concatenate(&mut self) {
        let b = self.pop();
        let a = self.pop();

        let b = b.as_obj_str().unwrap();
        let a = a.as_obj_str().unwrap();

        let new_len = a.len + b.len;

        let obj_str = if new_len == 0 {
            self.alloc_obj_string(ObjString::new(
                NonNull::dangling(),
                0,
                ObjHash::EMPTY_STR_HASH,
            ))
        } else {
            let layout = Layout::array::<u8>(new_len as usize).unwrap();
            let chars = unsafe {
                match NonNull::new(alloc::alloc(layout)) {
                    Some(ptr) => ptr,
                    None => handle_alloc_error(layout),
                }
            };

            unsafe {
                ptr::copy_nonoverlapping(a.chars.as_ptr(), chars.as_ptr(), a.len as usize);
                ptr::copy_nonoverlapping(
                    b.chars.as_ptr(),
                    chars.as_ptr().offset(a.len as isize),
                    b.len as usize,
                );
            }

            self.take_string(chars, new_len)
        };

        self.push(Value::Obj(obj_str.cast()))
    }

    fn call(&mut self, closure: NonNull<ObjClosure>, arg_count: u8) -> bool {
        let arity = unsafe { closure.as_ref().function.as_ref().arity };
        if arg_count != arity {
            self.runtime_error(format!("Expected {arg_count} arguments but got {arity}.").into());
            return false;
        }

        if self.call_frame_count as usize == FRAMES_MAX {
            self.runtime_error("Stack overflow.".into());
            return false;
        }

        self.next_call_frame(closure, arg_count);

        true
    }

    fn define_native(&mut self, name: &str, native_fn_kind: NativeFnKind) {
        // We don't want/need to trigger GC here so directly call allocation
        // functions on `self.mem`

        let name = Value::Obj(self.mem.copy_string(name).cast());

        let native_fn = Value::Obj(self.mem.alloc_obj(ObjNative::new(native_fn_kind)).cast());

        self.push(name);
        self.push(native_fn);

        unsafe {
            self.mem.globals.set(
                self.stack[self.stack_top as usize - 2]
                    .assume_init()
                    .as_obj_str_ptr()
                    .unwrap(),
                self.stack[self.stack_top as usize - 1].assume_init(),
            );
        }

        self.pop();
        self.pop();
    }

    fn call_value(&mut self, callee: Value, arg_count: u8) -> bool {
        match callee {
            Value::Obj(obj) => {
                let kind = unsafe { obj.as_ref().kind };
                match kind {
                    ObjKind::Class => {
                        let class: NonNull<ObjClass> = obj.cast();
                        let instance = self.alloc_obj(ObjInstance::new(class));
                        self.stack[self.stack_top as usize - arg_count as usize - 1] =
                            MaybeUninit::new(Value::Obj(instance.cast()));
                        return true;
                    }
                    ObjKind::Closure => return self.call(obj.cast(), arg_count),
                    ObjKind::Native => {
                        let native: NonNull<ObjNative> = obj.cast();
                        let stack_begin = self.stack_top as usize - arg_count as usize;
                        let values = &self.stack[stack_begin..];
                        let result =
                            unsafe { native.as_ref().function.call(std::mem::transmute(values)) };

                        self.stack_top -= arg_count as u32 + 1;
                        self.push(result);
                        return true;
                    }
                    ObjKind::BoundMethod => {
                        let bound: NonNull<ObjBoundMethod> = obj.cast();
                        self.stack[self.stack_top as usize - arg_count as usize - 1] =
                            MaybeUninit::new(unsafe { bound.as_ref() }.receiver);
                        return self.call(unsafe { bound.as_ref() }.method, arg_count);
                    }
                    _ => (),
                }
            }
            _ => {}
        }

        self.runtime_error("Can only call functions and classes.".into());
        false
    }

    fn capture_upvalue(&mut self, offset: u8) -> NonNull<ObjUpvalue> {
        let slot = self.top_call_frame().slots_offset;

        let local = NonNull::new(self.stack[slot as usize + offset as usize].as_mut_ptr()).unwrap();

        unsafe {
            let mut prev_upvalue: *mut ObjUpvalue = null_mut();
            let mut upvalue: *mut ObjUpvalue = self.open_upvalues;

            // the list is sorted in stack order (first upvalue location points to top-most local)
            // so we keep iterating skipping upvalues whose location is above our desired, hence the
            // `(*upvalue).location > local`
            while !upvalue.is_null() && (*upvalue).location > local {
                prev_upvalue = upvalue;
                upvalue = (*upvalue).next;
            }

            match NonNull::new(upvalue) {
                // found a captured upvalue, reuse it
                Some(upvalue_nonnull) if (*upvalue).location == local => return upvalue_nonnull,
                _ => (),
            }

            let created_upvalue = ObjUpvalue::new(local, upvalue);
            // TODO: FIX
            // let created_upvalue = self.mem.alloc_obj(created_upvalue);
            let created_upvalue = self.alloc_obj(created_upvalue);
            if prev_upvalue.is_null() {
                self.open_upvalues = created_upvalue.as_ptr();
            } else {
                (*prev_upvalue).next = created_upvalue.as_ptr();
            }

            created_upvalue
        }
    }

    fn new_closure(&mut self, function: NonNull<ObjFunction>) {
        let closure = self.alloc_obj(ObjClosure::new(function));
        self.push(Value::Obj(closure.cast()));
        unsafe {
            for i in 0..(*closure.as_ptr()).upvalue_count {
                let byte = self.read_byte();
                let is_local = byte == 1;
                let index = self.read_byte();

                let upvalue = if is_local {
                    self.capture_upvalue(index).as_ptr()
                } else {
                    // at this point we haven't switched call frame to closure yet, so we
                    // read from current call frame which is actually closure's surrounding call frame
                    *(self
                        .top_call_frame()
                        .closure
                        .as_ref()
                        .upvalues
                        .as_ptr()
                        .offset(index as isize))
                };

                (*(*closure.as_ptr()).upvalues.as_ptr().offset(i as isize)) = upvalue;
            }
        }
    }

    /// Closes upvalues of a scope
    fn close_upvalues(&mut self, last_stack_offset: u32) {
        unsafe {
            let last = NonNull::new(self.stack[last_stack_offset as usize].as_mut_ptr()).unwrap();
            while let Some(upvalue) = NonNull::new(self.open_upvalues) {
                if (*upvalue.as_ptr()).location < last {
                    return;
                }

                let upvalue_ptr = upvalue.as_ptr();
                (*upvalue_ptr).closed = *((*upvalue_ptr).location.as_ptr());
                (*upvalue_ptr).location =
                    NonNull::new(addr_of_mut!((*upvalue_ptr).closed)).unwrap();
                self.open_upvalues = (*upvalue_ptr).next;
            }
        }
    }

    fn define_method(&mut self, name: NonNull<ObjString>) {
        let method = self.peek(0);
        let mut class_value = self.peek(1);
        let class = class_value.as_class_mut().unwrap();
        class.methods.set(name, method);
        self.pop();
    }

    fn bind_method(&mut self, class: NonNull<ObjClass>, name: NonNull<ObjString>) -> bool {
        let method = match unsafe { (*class.as_ptr()).methods.get(name) } {
            Some(method) => method,
            None => {
                self.runtime_error(
                    format!("Undefined property {}", unsafe { name.as_ref() }.as_str()).into(),
                );

                return false;
            }
        };

        let receiver = self.peek(0);
        let bound = ObjBoundMethod::new(receiver, method.as_closure_ptr().unwrap());
        let bound = self.alloc_obj(bound);

        self.pop();
        self.push(Value::Obj(bound.cast()));

        true
    }

    pub fn run(&mut self) -> InterpretResult<()> {
        loop {
            #[cfg(debug_assertions)]
            {
                // Debug stack
                for slot in self.stack.iter().take(self.stack_top as usize) {
                    let value: &Value = unsafe { slot.assume_init_ref() };
                    println!("          [ {value:?} ]");
                }

                // Debug instruction
                let frame = self.top_call_frame();
                let mut duplicate_instruction_index = frame.instr_offset as usize;
                let line = frame.function().chunk.lines[duplicate_instruction_index];
                let inner = frame
                    .function()
                    .chunk
                    .disassemble_instruction(&mut duplicate_instruction_index);
                println!("{:?}", inner.map(|inner| InstructionDebug { line, inner }));
            }

            let byte = self.read_byte();

            match Opcode::from_u8(byte) {
                Some(Opcode::Method) => {
                    let obj_str = self.read_constant().as_obj_str_ptr().unwrap();
                    self.define_method(obj_str)
                }
                Some(Opcode::GetProperty) => {
                    let top = self.peek(0);
                    let instance = match top.as_instance_ptr() {
                        Some(instance) => instance,
                        None => {
                            self.runtime_error("Only instances have properties.".into());
                            return Err(InterpretError::RuntimeError);
                        }
                    };

                    let name = self
                        .read_constant()
                        .as_obj_str_ptr()
                        .expect("Expect to read a string constant.");

                    match unsafe { &*instance.as_ptr() }.fields.get(name) {
                        Some(val) => {
                            self.pop();
                            self.push(val);
                        }
                        None => {
                            if !self.bind_method(unsafe { &*instance.as_ptr() }.class, name) {
                                return Err(InterpretError::RuntimeError);
                            }
                        }
                    }
                }
                Some(Opcode::SetProperty) => {
                    let mut top = self.peek(1);
                    let instance = match top.as_instance_fn_mut() {
                        Some(instance) => instance,
                        None => {
                            self.runtime_error("Only instances have fields.".into());
                            return Err(InterpretError::RuntimeError);
                        }
                    };

                    let field_name = self
                        .read_constant()
                        .as_obj_str_ptr()
                        .expect("Expect to string constant");

                    instance.fields.set(field_name, self.peek(0));

                    let value = self.pop();
                    self.pop();
                    self.push(value);
                }
                Some(Opcode::Class) => {
                    let name = self
                        .read_constant()
                        .as_obj_str_ptr()
                        .expect("Opcode::Class instruction should be followed by string constant");

                    let class = ObjClass::new(name);
                    let class = self.alloc_obj(class);

                    self.push(Value::Obj(class.cast()));
                }
                Some(Opcode::CloseUpvalue) => {
                    self.close_upvalues(self.stack_top - 1);
                    self.pop();
                }
                Some(Opcode::GetUpvalue) => {
                    let slot = self.read_byte();
                    let val = unsafe {
                        *(self
                            .top_call_frame()
                            .closure
                            .as_ref()
                            .upvalue_at_slot(slot as usize)
                            .unwrap()
                            .as_ref()
                            .location
                            .as_ptr())
                    };

                    self.push(val);
                }
                Some(Opcode::SetUpvalue) => {
                    let slot = self.read_byte();
                    let val = self.peek(0);
                    unsafe {
                        let loc_ptr = self
                            .top_call_frame()
                            .closure()
                            .upvalue_at_slot(slot as usize)
                            .unwrap()
                            .as_mut()
                            .location
                            .as_ptr();

                        (*loc_ptr) = val;
                    }
                }
                Some(Opcode::Closure) => {
                    let function = self.read_constant().as_fn_ptr().unwrap();
                    self.new_closure(function);
                    // TODO: investigate
                    // let closure = self.alloc_obj();
                    // let noob = Value::Obj(closure.cast());
                    // println!("did the closure thing");
                    // self.push(noob);
                }
                Some(Opcode::Call) => {
                    let arg_count = self.read_byte();
                    if !self.call_value(self.peek(arg_count as u32), arg_count) {
                        return Err(InterpretError::RuntimeError);
                    }
                }
                Some(Opcode::Loop) => {
                    let offset = self.read_u16();
                    self.top_call_frame_mut().instr_offset -= offset as u32;
                }
                Some(Opcode::Jump) => {
                    let offset = self.read_u16();
                    self.top_call_frame_mut().instr_offset += offset as u32;
                }
                Some(Opcode::JumpIfFalse) => {
                    let offset = self.read_u16();
                    if self.peek(0).is_falsey() {
                        self.top_call_frame_mut().instr_offset += offset as u32;
                    }
                }
                Some(Opcode::GetLocal) => {
                    let slot = self.read_byte();
                    let slot_offset = self.top_call_frame().slots_offset;
                    let val =
                        unsafe { self.stack[slot as usize + slot_offset as usize].assume_init() };

                    self.push(val);
                }
                Some(Opcode::SetLocal) => {
                    let slot = self.read_byte();
                    let slot_offset = self.top_call_frame().slots_offset;
                    self.stack[slot as usize + slot_offset as usize] =
                        MaybeUninit::new(self.peek(0));
                }
                Some(Opcode::SetGlobal) => {
                    let name = self
                        .read_constant()
                        .as_obj_str_ptr()
                        .expect("Expect string constant for global variable name.");

                    let new_val = self.peek(0);
                    // println!("{} = {:?}", unsafe { name.as_ref() }.as_str(), self.peek(0));

                    if self.mem.globals.set(name, new_val) {
                        self.mem.globals.delete(name);
                        self.runtime_error(
                            format!("Undefined variable: {}", unsafe { name.as_ref().as_str() })
                                .into(),
                        );

                        return Err(InterpretError::RuntimeError);
                    }
                }
                Some(Opcode::GetGlobal) => {
                    let name = self
                        .read_constant()
                        .as_obj_str_ptr()
                        .expect("Expect string constant for global variable name.");

                    let val = match self.mem.globals.get(name) {
                        Some(global) => global,
                        None => {
                            self.runtime_error(
                                format!("Undefined variable: {}", unsafe {
                                    name.as_ref().as_str()
                                })
                                .into(),
                            );

                            return Err(InterpretError::RuntimeError);
                        }
                    };

                    self.push(val);
                }
                Some(Opcode::DefineGlobal) => {
                    let name = self
                        .read_constant()
                        .as_obj_str_ptr()
                        .expect("Expect string constant for global variable name.");

                    self.mem.globals.set(name, self.peek(0));
                    self.pop();
                }
                Some(Opcode::Nil) => {
                    self.push(Value::Nil);
                }
                Some(Opcode::Pop) => {
                    self.pop();
                }
                Some(Opcode::Print) => {
                    let value = self.pop();
                    println!("{value:?}");
                }
                Some(Opcode::Equal) => {
                    let b = self.pop();
                    let a = self.pop();

                    self.push(Value::Bool(a == b))
                }
                Some(Opcode::Not) => {
                    let top = self.pop();
                    self.push(Value::Bool(top.is_falsey()))
                }
                Some(Opcode::Negate) => {
                    if !matches!(self.peek(0), Value::Bool(_) | Value::Number(_)) {
                        self.runtime_error("Operand must be a number.".into());
                        return Err(InterpretError::RuntimeError);
                    }

                    let negated = -self.pop();
                    self.push(negated)
                }
                Some(Opcode::Return) => {
                    if self.call_frame_count == 1 {
                        self.pop();
                        return Ok(());
                    }

                    let result = self.pop();
                    let slots_offset = self.top_call_frame().slots_offset;
                    self.close_upvalues(slots_offset);

                    self.call_frame_count -= 1;

                    self.stack_top = self.top_call_frame().slots_offset;
                    self.push(result);
                }
                Some(Opcode::Constant) => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                Some(Opcode::Subtract) => self.binary_op(std::ops::Sub::sub)?,
                Some(Opcode::Multiply) => self.binary_op(std::ops::Mul::mul)?,
                Some(Opcode::Divide) => self.binary_op(std::ops::Div::div)?,
                Some(Opcode::Greater) => self.binary_op(Value::gt_owned)?,
                Some(Opcode::Less) => self.binary_op(Value::lt_owned)?,
                Some(Opcode::Add) => {
                    if self.peek(0).is_str() && self.peek(1).is_str() {
                        self.concatenate();
                    } else {
                        self.binary_op(std::ops::Add::add)?
                    }
                }
                otherwise => panic!("Unknown opcode {otherwise:?}"),
            }
        }
    }

    #[inline]
    fn read_byte(&mut self) -> u8 {
        let frame =
            unsafe { self.call_frames[self.call_frame_count as usize - 1].assume_init_mut() };

        let ret = frame.function().chunk.code[frame.instr_offset as usize];
        frame.instr_offset += 1;

        ret
    }

    #[inline]
    fn next_call_frame(&mut self, closure: NonNull<ObjClosure>, arg_count: u8) {
        let call_frame = self.call_frames[self.call_frame_count as usize].as_mut_ptr();
        self.call_frame_count += 1;
        unsafe {
            (*call_frame).instr_offset = 0;
            (*call_frame).slots_offset = self.stack_top - arg_count as u32 - 1;
            (*call_frame).closure = closure;
        }
    }

    #[inline]
    fn top_call_frame_ptr(&mut self) -> NonNull<CallFrame> {
        unsafe {
            NonNull::new_unchecked(
                self.call_frames[self.call_frame_count as usize - 1].as_mut_ptr(),
            )
        }
    }
    #[inline]
    fn top_call_frame_mut(&mut self) -> &mut CallFrame {
        unsafe { self.call_frames[self.call_frame_count as usize - 1].assume_init_mut() }
    }
    #[inline]
    fn top_call_frame(&self) -> &CallFrame {
        unsafe { self.call_frames[self.call_frame_count as usize - 1].assume_init_ref() }
    }

    #[inline]
    fn read_u16(&mut self) -> u16 {
        let frame = self.top_call_frame_mut();

        let (byte1, byte2) = (
            frame.function().chunk[frame.instr_offset as usize],
            frame.function().chunk[frame.instr_offset as usize + 1],
        );

        frame.instr_offset += 2;

        ((byte1 as u16) << 8) | (byte2 as u16)
    }

    #[inline]
    fn read_constant(&mut self) -> Value {
        let idx = self.read_byte();
        self.top_call_frame().function().chunk.constants[idx as usize]
    }
}
