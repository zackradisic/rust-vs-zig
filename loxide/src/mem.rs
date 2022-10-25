use std::{
    alloc::{self, handle_alloc_error, Allocator, Global, GlobalAlloc, Layout, System},
    ptr::{self, NonNull},
    sync::atomic::AtomicUsize,
};

use crate::{
    obj::{Obj, ObjList, ObjPunnable, ObjString},
    table::{ObjHash, Table},
    value::Value,
};

pub type Greystack = Vec<NonNull<Obj>>;

// Borrowed from https://github.com/ceronman/loxido/blob/a605c17e4d35bc75022e65387c200201704ec37c/src/gc.rs#L286
// pub struct GlobalAllocator {
//     bytes_allocated: AtomicUsize,
// }

// impl GlobalAllocator {
//     pub fn bytes_allocated(&self) -> usize {
//         self.bytes_allocated
//             .load(std::sync::atomic::Ordering::Relaxed)
//     }
// }

// unsafe impl alloc::GlobalAlloc for GlobalAllocator {
//     unsafe fn alloc(&self, layout: alloc::Layout) -> *mut u8 {
//         self.bytes_allocated
//             .fetch_add(layout.size(), std::sync::atomic::Ordering::Relaxed);
//         System.allocate(layout).unwrap().as_non_null_ptr().as_ptr()
//         // mimalloc::MiMalloc.alloc(layout)
//     }

//     unsafe fn dealloc(&self, ptr: *mut u8, layout: alloc::Layout) {
//         System.deallocate(NonNull::new(ptr).unwrap(), layout);
//         // mimalloc::MiMalloc.dealloc(ptr, layout);
//         self.bytes_allocated
//             .fetch_sub(layout.size(), std::sync::atomic::Ordering::Relaxed);
//     }
// }

// #[global_allocator]
// pub static GLOBAL: GlobalAllocator = GlobalAllocator {
//     bytes_allocated: AtomicUsize::new(0),
// };

pub struct Mem {
    pub obj_list: ObjList,
    pub globals: Table,
    pub interned_strings: Table,
    pub next_gc: usize,
    pub bytes_allocated: usize,
}

impl Mem {
    pub fn new() -> Self {
        Self {
            obj_list: Default::default(),
            globals: Table::new(),
            interned_strings: Table::new(),
            next_gc: 1024 * 1024,
            bytes_allocated: 0,
        }
    }

    #[inline]
    pub fn bytes_allocated(&self) -> usize {
        self.bytes_allocated
    }

    #[inline]
    pub fn alloc_obj_string(&mut self, obj_string: ObjString) -> NonNull<ObjString> {
        let obj_string = self.alloc_obj(obj_string);
        self.intern_string(obj_string);
        obj_string
    }

    #[inline]
    pub fn alloc_obj<T: ObjPunnable>(&mut self, obj: T) -> NonNull<T> {
        let val = unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(obj))) };
        self.obj_list.push_front(val.cast());

        self.bytes_allocated += std::mem::size_of::<T>();

        #[cfg(feature = "debug_gc")]
        println!(
            "{:?}: allocating {:?} bytes for {:?}",
            val,
            std::mem::size_of::<T>(),
            unsafe { (*val.as_ptr()).kind() }
        );

        val
    }

    #[inline]
    pub fn intern_string(&mut self, obj_string: NonNull<ObjString>) {
        self.interned_strings.set(obj_string, Value::Nil);
    }

    pub fn copy_string(&mut self, string: &str) -> NonNull<ObjString> {
        let hash = ObjHash::hash_string(string);
        match self.interned_strings.find_string(string, hash) {
            Some(interned) => return interned,
            None => (),
        };

        // Allocating layout for zero length data is not allowed,
        // documentation of `std::slice::from_raw_parts` says NonNull::dangling() is allowed
        // for zero-length data
        if string.is_empty() {
            let empty_string = ObjString::new(NonNull::dangling(), 0, hash);
            return self.alloc_obj_string(empty_string);
        }

        let layout = Layout::for_value(string.as_bytes());
        let chars = unsafe { alloc::alloc(layout) };
        unsafe {
            ptr::copy_nonoverlapping(string.as_ptr(), chars, string.len());
        }
        let chars = match NonNull::new(chars) {
            Some(ptr) => ptr,
            None => handle_alloc_error(layout),
        };

        let obj_str = ObjString::new(chars, string.len() as u32, hash);
        self.alloc_obj_string(obj_str)
    }
}

impl Drop for Mem {
    fn drop(&mut self) {
        // free obj list
        for obj in self.obj_list.iter_mut() {
            Obj::free(*obj)
        }

        Table::free(&mut self.interned_strings);
        Table::free(&mut self.globals);
    }
}
