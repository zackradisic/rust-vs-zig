use std::{
    alloc::{self, Layout},
    collections::LinkedList,
    mem,
    ptr::{self, addr_of_mut, null_mut, NonNull},
    slice,
};

use crate::{
    chunk::Chunk,
    native_fn::NativeFnKind,
    table::{ObjHash, Table},
    value::Value,
};

pub type ObjList = LinkedList<NonNull<Obj>>;

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ObjKind {
    Str,
    Fn,
    Native,
    Closure,
    Upvalue,
}

#[repr(C)]
pub struct Obj {
    pub kind: ObjKind,
}

pub struct ObjPtrWrapper(pub *mut Obj);

#[repr(C)]
pub struct ObjNative {
    pub obj: Obj,
    pub function: NativeFnKind,
}

#[repr(C)]
pub struct ObjUpvalue {
    pub obj: Obj,
    pub location: NonNull<Value>,
    // open upvalues are in a linkedlist so we can traverse that to reuse upvalues
    pub next: *mut ObjUpvalue,
    pub closed: Value,
}

#[repr(C)]
pub struct ObjClosure {
    pub obj: Obj,
    pub function: NonNull<ObjFunction>,
    pub upvalues: NonNull<*mut ObjUpvalue>,
    pub upvalue_count: u8,
}

#[repr(C)]
pub struct ObjFunction {
    pub obj: Obj,
    pub arity: u8,
    pub chunk: Chunk,
    pub name: *mut ObjString,
    pub upvalue_count: u8,
}

#[repr(C)]
pub struct ObjString {
    pub obj: Obj,
    pub len: u32,
    pub hash: ObjHash,
    pub chars: NonNull<u8>,
}

impl Obj {
    /// Safety:
    /// T must be type-punnable from T <-> Obj
    unsafe fn alloc<T>(obj_list: &mut ObjList, kind: ObjKind) -> NonNull<T> {
        let layout = Layout::from_size_align(mem::size_of::<T>(), mem::align_of::<T>()).unwrap();
        let obj = NonNull::new(alloc::alloc(layout) as *mut Obj).expect("Heap allocation failed.");

        (*obj.as_ptr()).kind = kind;

        obj_list.push_front(obj);

        obj.cast()
    }

    unsafe fn dealloc<T>(obj: *mut Obj) {
        let layout = Layout::from_size_align(mem::size_of::<T>(), mem::align_of::<T>()).unwrap();
        alloc::dealloc(obj as *mut u8, layout)
    }

    pub fn free(obj: NonNull<Obj>) {
        unsafe {
            let obj = obj.as_ptr();
            let kind = (*obj).kind;

            match kind {
                ObjKind::Str => {
                    let obj_str = obj as *mut ObjString;
                    let len = (*obj_str).len;
                    if len != 0 {
                        let layout = {
                            let bytes =
                                slice::from_raw_parts((*obj_str).chars.as_ptr(), len as usize);
                            Layout::for_value(bytes)
                        };
                        alloc::dealloc((*obj_str).chars.as_ptr(), layout);
                    }

                    Self::dealloc::<ObjString>(obj)
                }
                ObjKind::Fn => {
                    // `name` will be freed by string table
                    // just need to free `chunk`
                    let _chunk = ptr::read(addr_of_mut!((*obj.cast::<ObjFunction>()).chunk));

                    Self::dealloc::<ObjFunction>(obj)
                }
                ObjKind::Native => Self::dealloc::<ObjNative>(obj),
                ObjKind::Closure => Self::dealloc::<ObjClosure>(obj),
                ObjKind::Upvalue => {
                    let upvalues = (*obj.cast::<ObjClosure>()).upvalues;
                    let upvalues_count = (*obj.cast::<ObjClosure>()).upvalue_count;

                    if upvalues_count != 0 {
                        // drop upvalues
                        let _upvalues = Vec::from_raw_parts(
                            upvalues.as_ptr(),
                            upvalues_count as usize,
                            upvalues_count as usize,
                        );
                    }

                    Self::dealloc::<ObjUpvalue>(obj);
                }
            }
        }
    }
}

impl std::fmt::Debug for ObjPtrWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ptr = match NonNull::new(self.0) {
            Some(ptr) => ptr,
            None => return write!(f, "{:?}", None as Option<i32>),
        };

        let kind = unsafe { ptr.as_ref().kind };
        match kind {
            ObjKind::Str => {
                let obj_str = unsafe { ptr.cast::<ObjString>().as_ref() };
                write!(f, "{:?}", obj_str.as_str())
            }
            ObjKind::Fn => unsafe {
                let obj_fn = ptr.cast::<ObjFunction>().as_ref();
                f.debug_struct("Function")
                    .field("name", &ObjPtrWrapper(obj_fn.name as *mut Obj))
                    .field("arity", &obj_fn.arity)
                    // .field("chunk", &obj_fn.chunk)
                    .finish()
            },
            ObjKind::Native => {
                let function = unsafe { ptr.cast::<ObjNative>().as_ref().function };
                write!(f, "{:?}", function)
            }
            ObjKind::Closure => unsafe {
                let ptr: NonNull<ObjClosure> = ptr.cast();
                let upvalues = std::slice::from_raw_parts(
                    ptr.as_ref().upvalues.as_ptr(),
                    ptr.as_ref().upvalue_count as usize,
                );
                f.debug_struct("Closure")
                    .field(
                        "function",
                        &ObjPtrWrapper(ptr.as_ref().function.as_ptr() as *mut Obj),
                    )
                    .field("upvalues", &upvalues)
                    .finish()
            },
            ObjKind::Upvalue => unsafe {
                let ptr: &ObjUpvalue = ptr.cast().as_ref();
                let loc_ptr = ptr.location;
                let loc = *ptr.location.as_ptr();

                f.debug_struct("ObjUpvalue")
                    .field("location", &(loc_ptr, loc))
                    .finish()
            },
        }
    }
}

impl ObjNative {
    pub fn new(obj_list: &mut ObjList, kind: NativeFnKind) -> NonNull<ObjNative> {
        let obj_native: NonNull<ObjNative> = unsafe { Obj::alloc(obj_list, ObjKind::Native) };

        unsafe {
            (*obj_native.as_ptr()).function = kind;
        }

        obj_native
    }
}

impl ObjUpvalue {
    pub fn new(obj_list: &mut ObjList, location: NonNull<Value>) -> NonNull<ObjUpvalue> {
        let obj_upvalue: NonNull<ObjUpvalue> = unsafe { Obj::alloc(obj_list, ObjKind::Upvalue) };
        unsafe {
            let ptr = obj_upvalue.as_ptr();
            (*ptr).location = location;
            (*ptr).next = null_mut();
            (*ptr).closed = Value::Nil;
        }

        obj_upvalue
    }

    // pub fn upvalue_at_slot(&self, slot: usize) -> Option<NonNull<ObjUpvalue>> {
    //     if self.upv
    // }
}

impl ObjClosure {
    pub fn new(obj_list: &mut ObjList, function: NonNull<ObjFunction>) -> NonNull<ObjClosure> {
        unsafe {
            let obj_closure: NonNull<ObjClosure> = Obj::alloc(obj_list, ObjKind::Closure);

            let upvalue_count = (*function.as_ptr()).upvalue_count;
            let upvalues = if upvalue_count == 0 {
                NonNull::dangling()
            } else {
                let layout = Layout::array::<*mut ObjUpvalue>(upvalue_count as usize).unwrap();
                NonNull::new(alloc::alloc_zeroed(layout)).unwrap().cast()
            };

            (*obj_closure.as_ptr()).function = function;
            (*obj_closure.as_ptr()).upvalue_count = upvalue_count;
            (*obj_closure.as_ptr()).upvalues = upvalues;

            obj_closure
        }
    }

    pub fn upvalue_at_slot(&self, slot: usize) -> Option<NonNull<ObjUpvalue>> {
        if self.upvalue_count == 0 {
            return None;
        }

        unsafe {
            let upvalues =
                slice::from_raw_parts(self.upvalues.as_ptr(), self.upvalue_count as usize);
            upvalues.get(slot).and_then(|upval| NonNull::new(*upval))
        }
    }
}

impl ObjFunction {
    pub fn new(obj_list: &mut ObjList, name: *mut ObjString) -> NonNull<ObjFunction> {
        let obj_fn = unsafe { Obj::alloc::<ObjFunction>(obj_list, ObjKind::Fn) };

        unsafe {
            (*obj_fn.as_ptr()).arity = 0;
            (*obj_fn.as_ptr()).name = name;
            (*obj_fn.as_ptr()).upvalue_count = 0;
            addr_of_mut!((*obj_fn.as_ptr()).chunk).write(Chunk::new());
        }

        obj_fn
    }
}

impl ObjString {
    pub fn as_str(&self) -> &str {
        unsafe {
            let bytes = slice::from_raw_parts(self.chars.as_ptr(), self.len as usize);
            std::str::from_utf8_unchecked(bytes)
        }
    }

    pub fn take_string(
        interned_strings: &mut Table,
        obj_list: &mut ObjList,
        chars: *mut u8,
        len: u32,
    ) -> NonNull<ObjString> {
        let hash = ObjHash::hash_string(unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(chars, len as usize))
        });

        match interned_strings.find_string(
            unsafe {
                std::str::from_utf8_unchecked(std::slice::from_raw_parts(chars, len as usize))
            },
            hash,
        ) {
            Some(interned) => {
                return interned;
            }
            None => (),
        }

        Self::alloc_str(
            interned_strings,
            obj_list,
            unsafe { NonNull::new_unchecked(chars) },
            len,
            hash,
        )
    }

    pub fn copy_string(
        interned_strings: &mut Table,
        obj_list: &mut ObjList,
        string: &str,
    ) -> NonNull<ObjString> {
        let hash = ObjHash::hash_string(string);
        match interned_strings.find_string(string, hash) {
            Some(interned) => return interned,
            None => (),
        };

        // Allocating layout for zero length data is not allowed
        if string.is_empty() {
            return Self::alloc_str(interned_strings, obj_list, NonNull::dangling(), 0, hash);
        }

        let layout = Layout::for_value(string.as_bytes());
        let chars = unsafe { alloc::alloc(layout) };
        unsafe {
            ptr::copy_nonoverlapping(string.as_ptr(), chars, string.len());
        }

        Self::alloc_str(
            interned_strings,
            obj_list,
            unsafe { NonNull::new_unchecked(chars) },
            string.len() as u32,
            hash,
        )
    }

    pub fn alloc_str(
        interned_strings: &mut Table,
        obj_list: &mut ObjList,
        chars: NonNull<u8>,
        len: u32,
        hash: ObjHash,
    ) -> NonNull<ObjString> {
        // Safety:
        // This is safe because ObjString <-> Obj
        let ptr = unsafe { Obj::alloc::<ObjString>(obj_list, ObjKind::Str) };

        unsafe {
            let ptr = ptr.as_ptr();
            (*ptr).len = len;
            (*ptr).chars = chars;
            (*ptr).hash = hash;
        }

        interned_strings.set(ptr, Value::Nil);

        ptr
    }
}
