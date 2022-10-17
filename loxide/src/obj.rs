use std::{
    alloc::{self, Layout},
    collections::{hash_map::DefaultHasher, LinkedList},
    hash::{BuildHasher, Hash, Hasher},
    mem::{self},
    ptr::{self, NonNull},
    slice,
};

use fnv::FnvHasher;

use crate::{
    table::{LoxHash, Table},
    value::Value,
};

pub type ObjList = LinkedList<*mut Obj>;

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ObjKind {
    Str,
}

#[repr(C)]
pub struct Obj {
    pub kind: ObjKind,
}

#[repr(C)]
pub struct ObjString {
    pub obj: Obj,
    pub len: u32,
    pub hash: LoxHash,
    pub chars: NonNull<u8>,
}

impl Obj {
    /// Safety:
    /// T must be type-punnable from T <-> Obj
    unsafe fn alloc<T>(obj_list: &mut ObjList, kind: ObjKind) -> *mut T {
        let layout = Layout::from_size_align(mem::size_of::<T>(), mem::align_of::<T>()).unwrap();
        let dst = alloc::alloc(layout);
        let obj: *mut Obj = dst as *mut Obj;

        (*obj).kind = kind;

        obj_list.push_front(obj);

        obj as *mut T
    }

    unsafe fn dealloc<T>(obj: *mut Obj) {
        let layout = Layout::from_size_align(mem::size_of::<T>(), mem::align_of::<T>()).unwrap();
        alloc::dealloc(obj as *mut u8, layout)
    }

    pub fn free(obj: *mut Obj) {
        unsafe {
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
            }
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

    pub fn take_string(
        interned_strings: &mut Table,
        obj_list: &mut ObjList,
        chars: *mut u8,
        len: u32,
    ) -> *mut ObjString {
        let hash = LoxHash::hash_string(unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(chars, len as usize))
        });

        match interned_strings.find_string(
            unsafe {
                std::str::from_utf8_unchecked(std::slice::from_raw_parts(chars, len as usize))
            },
            hash,
        ) {
            Some(interned) => {
                return interned.as_ptr();
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
    ) -> *mut ObjString {
        let hash = LoxHash::hash_string(string);
        match interned_strings.find_string(string, hash) {
            Some(interned) => return interned.as_ptr(),
            None => (),
        };

        // Allocating layout for zero length data is not allowed
        if string.len() == 0 {
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
        hash: LoxHash,
    ) -> *mut ObjString {
        // Safety:
        // This is safe because ObjString <-> Obj
        let ptr = unsafe { Obj::alloc::<ObjString>(obj_list, ObjKind::Str) };

        unsafe {
            (*ptr).len = len;
            (*ptr).chars = chars;
            (*ptr).hash = hash;
        }

        interned_strings.set(ptr, Value::Nil);

        ptr
    }
}
