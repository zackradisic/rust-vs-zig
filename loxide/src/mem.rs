use std::{
    alloc::{self, handle_alloc_error, Layout},
    ops::{Deref, DerefMut},
    ptr::{self, NonNull},
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

    #[cfg(feature = "always_gc")]
    #[inline]
    pub fn should_run_gc<T: Sized>(&self) -> bool {
        true
    }

    #[cfg(not(feature = "always_gc"))]
    #[inline]
    pub fn should_run_gc<T: Sized>(&self) -> bool {
        self.bytes_allocated() + std::mem::size_of::<T>() > self.next_gc
    }

    #[inline]
    pub fn bytes_allocated(&self) -> usize {
        self.bytes_allocated
    }

    #[inline]
    pub fn alloc_obj_string(&mut self, obj_string: ObjString) -> Gc<ObjString> {
        let obj_string = self.alloc_obj(obj_string);
        self.intern_string(obj_string.as_non_null_ptr());
        obj_string
    }

    #[inline]
    pub fn alloc_obj<T: ObjPunnable>(&mut self, obj: T) -> Gc<T> {
        let val = Gc::new(unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(obj))) });
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

    pub fn copy_string(&mut self, string: &str) -> Gc<ObjString> {
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
            Obj::free(obj.as_non_null_ptr())
        }

        Table::free(&mut self.interned_strings);
        Table::free(&mut self.globals);
    }
}

#[repr(transparent)]
pub struct Gc<T> {
    inner: NonNull<T>,
}

impl<T> Gc<T> {
    pub fn new(inner: NonNull<T>) -> Self {
        Self { inner }
    }

    #[inline]
    pub fn as_ptr(self) -> *mut T {
        self.inner.as_ptr()
    }

    #[inline]
    pub fn as_non_null_ptr(self) -> NonNull<T> {
        self.inner
    }

    #[inline]
    pub fn cast<K>(self) -> Gc<K> {
        Gc::new(self.inner.cast())
    }
}

impl<T> std::fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Gc").field("inner", &self.inner).finish()
    }
}
impl<T> Copy for Gc<T> {}
impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl AsRef<str> for Gc<ObjString> {
    fn as_ref(&self) -> &str {
        unsafe { self.inner.as_ref().as_str() }
    }
}
impl<T> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        unsafe { self.inner.as_ref() }
    }
}
impl<T> AsMut<T> for Gc<T> {
    fn as_mut(&mut self) -> &mut T {
        unsafe { self.inner.as_mut() }
    }
}
impl<T> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.inner.as_ref() }
    }
}

impl<T> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.inner.as_mut() }
    }
}
