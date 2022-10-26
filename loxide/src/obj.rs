use std::{
    alloc::{self, Layout},
    collections::VecDeque,
    ptr::NonNull,
    slice,
};

use crate::{chunk::Chunk, mem::Greystack, native_fn::NativeFnKind, table::ObjHash, value::Value};

pub type ObjList = VecDeque<NonNull<Obj>>;

/// This is to enable type-safe functions generic over types that are type punnable to Obj
pub trait ObjPunnable: Sized {
    fn kind(&self) -> ObjKind;
}

impl ObjPunnable for Obj {
    fn kind(&self) -> ObjKind {
        self.kind
    }
}
impl ObjPunnable for ObjNative {
    fn kind(&self) -> ObjKind {
        ObjKind::Native
    }
}
impl ObjPunnable for ObjUpvalue {
    fn kind(&self) -> ObjKind {
        ObjKind::Upvalue
    }
}
impl ObjPunnable for ObjFunction {
    fn kind(&self) -> ObjKind {
        ObjKind::Fn
    }
}
impl ObjPunnable for ObjClosure {
    fn kind(&self) -> ObjKind {
        ObjKind::Closure
    }
}
impl ObjPunnable for ObjString {
    fn kind(&self) -> ObjKind {
        ObjKind::Str
    }
}

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
    pub is_marked: bool,
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
    pub unsafe fn blacken(obj: NonNull<Obj>, greystack: &mut Greystack) {
        #[cfg(feature = "debug_gc")]
        println!("{:?} blacken {:?}", obj.as_ptr(), Value::Obj(obj));

        let kind = obj.as_ref().kind;
        match kind {
            ObjKind::Fn => {
                let function = obj.cast::<ObjFunction>().as_ref();
                Obj::mark(function.name.cast(), greystack);
                for val in function.chunk.constants.iter() {
                    val.mark(greystack)
                }
            }
            ObjKind::Closure => {
                let closure = obj.cast::<ObjClosure>().as_ref();
                Obj::mark(closure.function.as_ptr().cast(), greystack);
                let upvalue_slice = std::slice::from_raw_parts(
                    closure.upvalues.as_ptr(),
                    closure.upvalue_count as usize,
                );
                for obj in upvalue_slice {
                    Obj::mark(obj.cast(), greystack);
                }
            }
            ObjKind::Upvalue => obj.cast::<ObjUpvalue>().as_ref().closed.mark(greystack),
            ObjKind::Native | ObjKind::Str => (),
        }
    }

    pub unsafe fn mark(obj: *mut Obj, greystack: &mut Greystack) {
        if obj.is_null() {
            return;
        }

        if obj.as_ref().unwrap_unchecked().is_marked {
            return;
        }

        #[cfg(feature = "debug_gc")]
        println!(
            "{:?} mark {:?}",
            obj,
            Value::Obj(NonNull::new(obj).unwrap())
        );

        // let kind = (*obj).kind;
        (*obj).is_marked = true;

        // Safety:
        // We checked that obj is non null above
        greystack.push(unsafe { NonNull::new_unchecked(obj.cast()) });
    }

    pub fn free(obj_nonnull: NonNull<Obj>) {
        unsafe {
            let obj = obj_nonnull.as_ptr();
            let kind = (*obj).kind;

            #[cfg(feature = "debug_gc")]
            println!(
                "{:?} free type {:?} ({:?})",
                obj,
                kind,
                Value::Obj(obj_nonnull)
            );

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

                    let _ = Box::from_raw(obj as *mut ObjString);
                }
                ObjKind::Fn => {
                    let _ = Box::from_raw(obj as *mut ObjFunction);
                }
                ObjKind::Native => {
                    let _ = Box::from_raw(obj as *mut ObjNative);
                }
                ObjKind::Closure => {
                    let upvalues = (*obj.cast::<ObjClosure>()).upvalues;
                    let upvalues_count = (*obj.cast::<ObjClosure>()).upvalue_count;

                    if upvalues_count != 0 {
                        // drop upvalues array
                        let _upvalues = Vec::from_raw_parts(
                            upvalues.as_ptr(),
                            upvalues_count as usize,
                            upvalues_count as usize,
                        );
                    }

                    let _ = Box::from_raw(obj as *mut ObjClosure);
                }
                ObjKind::Upvalue => {
                    let _ = Box::from_raw(obj as *mut ObjUpvalue);
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
                write!(f, "{function:?}")
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
    pub fn new(kind: NativeFnKind) -> Self {
        Self {
            obj: Obj {
                kind: ObjKind::Native,
                is_marked: false,
            },
            function: kind,
        }
    }
}

impl ObjUpvalue {
    pub fn new(location: NonNull<Value>, next: *mut ObjUpvalue) -> Self {
        Self {
            obj: Obj {
                kind: ObjKind::Upvalue,
                is_marked: false,
            },
            location,
            next,
            closed: Value::Nil,
        }
    }
}

impl ObjClosure {
    pub fn new(function: NonNull<ObjFunction>) -> Self {
        let (upvalues, upvalue_count) = unsafe {
            let upvalue_count = (*function.as_ptr()).upvalue_count;
            let upvalues = if upvalue_count == 0 {
                NonNull::dangling()
            } else {
                let layout = Layout::array::<*mut ObjUpvalue>(upvalue_count as usize).unwrap();
                NonNull::new(alloc::alloc_zeroed(layout)).unwrap().cast()
            };

            (upvalues, upvalue_count)
        };

        Self {
            obj: Obj {
                kind: ObjKind::Closure,
                is_marked: false,
            },
            function,
            upvalues,
            upvalue_count,
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
    pub fn new(name: *mut ObjString) -> Self {
        Self {
            obj: Obj {
                kind: ObjKind::Fn,
                is_marked: false,
            },
            arity: 0,
            chunk: Chunk::new(),
            name,
            upvalue_count: 0,
        }
    }
}

impl ObjString {
    pub fn as_str(&self) -> &str {
        unsafe {
            let bytes = slice::from_raw_parts(self.chars.as_ptr(), self.len as usize);
            std::str::from_utf8_unchecked(bytes)
        }
    }

    pub fn new(chars: NonNull<u8>, len: u32, hash: ObjHash) -> ObjString {
        Self {
            obj: Obj {
                kind: ObjKind::Str,
                is_marked: false,
            },
            len,
            hash,
            chars,
        }
    }
}
