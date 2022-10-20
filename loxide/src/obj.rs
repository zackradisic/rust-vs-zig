use std::{
    alloc::{self, Layout},
    collections::LinkedList,
    mem::{self},
    ptr::{self, addr_of_mut, NonNull},
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
pub struct ObjFunction {
    pub obj: Obj,
    pub arity: u8,
    pub chunk: Chunk,
    pub name: *mut ObjString,
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

impl ObjFunction {
    pub fn new(obj_list: &mut ObjList, name: *mut ObjString) -> NonNull<ObjFunction> {
        let obj_fn = unsafe { Obj::alloc::<ObjFunction>(obj_list, ObjKind::Fn) };

        unsafe {
            (*obj_fn.as_ptr()).arity = 0;
            addr_of_mut!((*obj_fn.as_ptr()).chunk).write(Chunk::new());
            (*obj_fn.as_ptr()).name = name;
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
