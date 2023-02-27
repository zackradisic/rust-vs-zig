use std::{
    alloc::{self, handle_alloc_error, Layout},
    collections::VecDeque,
    ops::{Deref, DerefMut},
    ptr::{self, NonNull},
};

use generational_arena::{Arena, Index};

use crate::{
    obj::{
        Obj, ObjBoundMethod, ObjClass, ObjClosure, ObjFunction, ObjInstance, ObjKind, ObjList,
        ObjNative, ObjPunnable, ObjString, ObjUpvalue,
    },
    obj2::Obj as Obj2,
    obj2::ObjIdsEnum as ObjIdsEnum2,
    obj2::{
        ObjBoundMethod as ObjBoundMethod2, ObjClass as ObjClass2, ObjClosure as ObjClosure2,
        ObjFunction as ObjFunction2, ObjInstance as ObjInstance2, ObjNative as ObjNative2,
        ObjString as ObjString2, ObjUpvalue as ObjUpvalue2,
    },
    obj_id::{
        cast_magic_obj_id, cast_magic_obj_id_index, cast_obj_id, ObjId, ObjIdAny, ObjStringId,
    },
    table::{ObjHash, Table},
    value::Value,
    value2::Value as Value2,
};

pub type Greystack = Vec<NonNull<Obj>>;
pub type Greystack2 = Vec<ObjIdAny>;
pub type ObjList2 = VecDeque<ObjIdAny>;

pub struct Mem {
    pub obj_list: ObjList,
    pub obj_list2: ObjList2,
    pub globals: Table,
    pub interned_strings: Table,
    pub interned_strings2: Table,
    pub next_gc: usize,
    pub bytes_allocated: usize,

    pub obj_string_arena: Arena<ObjString2, ObjId<{ ObjKind::STR }>>,
    pub obj_fn_arena: Arena<ObjFunction2, ObjId<{ ObjKind::FN }>>,
    pub obj_native_arena: Arena<ObjNative2, ObjId<{ ObjKind::NATIVE }>>,
    pub obj_closure_arena: Arena<ObjClosure2, ObjId<{ ObjKind::CLOSURE }>>,
    pub obj_upvalue_arena: Arena<ObjUpvalue2, ObjId<{ ObjKind::UPVALUE }>>,
    pub obj_class_arena: Arena<ObjClass2, ObjId<{ ObjKind::CLASS }>>,
    pub obj_instance_arena: Arena<ObjInstance2, ObjId<{ ObjKind::INSTANCE }>>,
    pub obj_bound_method_arena: Arena<ObjBoundMethod2, ObjId<{ ObjKind::BOUND_METHOD }>>,
}

impl Mem {
    pub fn new() -> Self {
        Self {
            obj_list: Default::default(),
            obj_list2: Default::default(),
            globals: Table::new(),
            interned_strings: Table::new(),
            interned_strings2: Table::new(),
            next_gc: 1024 * 1024,
            bytes_allocated: 0,

            obj_string_arena: Arena::new(),
            obj_fn_arena: Arena::new(),
            obj_native_arena: Arena::new(),
            obj_closure_arena: Arena::new(),
            obj_upvalue_arena: Arena::new(),
            obj_class_arena: Arena::new(),
            obj_instance_arena: Arena::new(),
            obj_bound_method_arena: Arena::new(),
        }
    }

    pub fn free(&mut self, id: ObjIdAny) {
        let _ = match id.generation().ty() {
            ObjKind::Str => {
                let id = cast_obj_id(id);
                let _ = self.obj_string_arena.remove(id).unwrap();
            }
            ObjKind::Fn => {
                let id = cast_obj_id(id);
                let _ = self.obj_fn_arena.remove(id).unwrap();
            }
            ObjKind::Native => {
                let id = cast_obj_id(id);
                let _ = self.obj_native_arena.remove(id).unwrap();
            }
            ObjKind::Closure => {
                let id = cast_obj_id(id);
                let _ = self.obj_closure_arena.remove(id).unwrap();
            }
            ObjKind::Upvalue => {
                let id = cast_obj_id(id);
                let _ = self.obj_upvalue_arena.remove(id).unwrap();
            }
            ObjKind::Class => {
                let id = cast_obj_id(id);
                let mut obj = self.obj_class_arena.remove(id).unwrap();
                Table::free(&mut obj.methods);
            }
            ObjKind::Instance => {
                let id = cast_obj_id(id);
                let mut obj = self.obj_instance_arena.remove(id).unwrap();
                Table::free(&mut obj.fields);
            }
            ObjKind::BoundMethod => {
                let id = cast_obj_id(id);
                let _ = self.obj_bound_method_arena.remove(id).unwrap();
            }
        };

        // obj_ptr.as_mut().is_marked = false;
    }

    pub fn unmark(&self, id: ObjIdAny) {
        let _ = match id.generation().ty() {
            ObjKind::Str => {
                let id = cast_obj_id(id);
                let obj = self.obj_string_arena.get(id).unwrap();
                let obj = self
                    .obj_string_arena
                    .update_generation_data(id, id.generation().set_marked(false));
            }
            ObjKind::Fn => {
                let id = cast_obj_id(id);
                let obj = self.obj_fn_arena.get(id).unwrap();
                let obj = self
                    .obj_fn_arena
                    .update_generation_data(id, id.generation().set_marked(false));
            }
            ObjKind::Native => {
                let id = cast_obj_id(id);
                let obj = self.obj_native_arena.get(id).unwrap();
                let obj = self
                    .obj_native_arena
                    .update_generation_data(id, id.generation().set_marked(false));
            }
            ObjKind::Closure => {
                let id = cast_obj_id(id);
                let obj = self.obj_closure_arena.get(id).unwrap();
                let obj = self
                    .obj_closure_arena
                    .update_generation_data(id, id.generation().set_marked(false));
            }
            ObjKind::Upvalue => {
                let id = cast_obj_id(id);
                let obj = self.obj_upvalue_arena.get(id).unwrap();
                let obj = self
                    .obj_upvalue_arena
                    .update_generation_data(id, id.generation().set_marked(false));
            }
            ObjKind::Class => {
                let id = cast_obj_id(id);
                let obj = self.obj_class_arena.get(id).unwrap();
                let obj = self
                    .obj_class_arena
                    .update_generation_data(id, id.generation().set_marked(false));
            }
            ObjKind::Instance => {
                let id = cast_obj_id(id);
                let obj = self.obj_instance_arena.get(id).unwrap();
                let obj = self
                    .obj_instance_arena
                    .update_generation_data(id, id.generation().set_marked(false));
            }
            ObjKind::BoundMethod => {
                let id = cast_obj_id(id);
                let obj = self.obj_bound_method_arena.get(id).unwrap();
                let obj = self
                    .obj_bound_method_arena
                    .update_generation_data(id, id.generation().set_marked(false));
            }
        };

        // obj_ptr.as_mut().is_marked = false;
    }

    pub fn mark(&self, greystack: &mut Greystack2, val: Value2) {
        let id = match val {
            Value2::Nil | Value2::Number(_) | Value2::Bool(_) => return,
            Value2::ObjString(id) => {
                let new_generation = id.generation().set_marked(true);
                self.obj_string_arena
                    .update_generation_data(id, new_generation);
                cast_magic_obj_id_index(id)
            }
            Value2::ObjFn(id) => {
                let new_generation = id.generation().set_marked(true);
                self.obj_fn_arena.update_generation_data(id, new_generation);
                cast_magic_obj_id_index(id)
            }
            Value2::ObjNative(id) => {
                let new_generation = id.generation().set_marked(true);
                self.obj_native_arena
                    .update_generation_data(id, new_generation);
                cast_magic_obj_id_index(id)
            }
            Value2::ObjClosure(id) => {
                let new_generation = id.generation().set_marked(true);
                self.obj_closure_arena
                    .update_generation_data(id, new_generation);
                cast_magic_obj_id_index(id)
            }
            Value2::ObjUpvalue(id) => {
                let new_generation = id.generation().set_marked(true);
                self.obj_upvalue_arena
                    .update_generation_data(id, new_generation);
                cast_magic_obj_id_index(id)
            }
            Value2::ObjClass(id) => {
                let new_generation = id.generation().set_marked(true);
                self.obj_class_arena
                    .update_generation_data(id, new_generation);
                cast_magic_obj_id_index(id)
            }
            Value2::ObjInstance(id) => {
                let new_generation = id.generation().set_marked(true);
                self.obj_instance_arena
                    .update_generation_data(id, new_generation);
                cast_magic_obj_id_index(id)
            }
            Value2::ObjBoundMethod(id) => {
                let new_generation = id.generation().set_marked(true);
                self.obj_bound_method_arena
                    .update_generation_data(id, new_generation);
                cast_magic_obj_id_index(id)
            }
        };

        let patched_id = id.set_generation(id.generation().set_marked(true));
        greystack.push(patched_id);
    }

    pub fn blacken(&mut self, greystack: &mut Greystack2, val: Value2) {
        match val {
            Value2::ObjString(_)
            | Value2::ObjNative(_)
            | Value2::Nil
            | Value2::Number(_)
            | Value2::Bool(_) => (),

            Value2::ObjFn(id) => {
                let function = self.obj_fn_arena.get(id).unwrap();
                {
                    self.mark(greystack, Value2::ObjString(function.name));
                }
                let function = self.obj_fn_arena.get(id).unwrap();
                for val in function.chunk.constants.iter() {
                    // self.mark(greystack, *val);
                    todo!()
                }
            }
            Value2::ObjClosure(id) => {
                let closure = self.obj_closure_arena.get(id).unwrap();
                self.mark(greystack, Value2::ObjFn(closure.function));
                for upvalue in closure.upvalues.iter().copied() {
                    self.mark(greystack, Value2::ObjUpvalue(upvalue))
                }
            }
            Value2::ObjUpvalue(_) => todo!(),
            Value2::ObjClass(id) => {
                let class = self.obj_class_arena.get(id).unwrap();
                self.mark(greystack, Value2::ObjString(class.name));
                todo!("mark methods")
            }
            Value2::ObjInstance(id) => {
                let instance = self.obj_instance_arena.get(id).unwrap();
                self.mark(greystack, Value2::ObjClass(instance.class));
                todo!("mark methods")
            }
            Value2::ObjBoundMethod(id) => {
                let bound = self.obj_bound_method_arena.get(id).unwrap();
                self.mark(greystack, Value2::ObjClosure(bound.method));
                self.mark(greystack, bound.receiver);
            }
        }
    }

    pub fn sweep(&mut self) {
        // Clear references to unmarked strings
        self.interned_strings.remove_white();

        // Now free all unmarked objects
        let mut i = 0;
        loop {
            let mut id = match self.obj_list2.get(i) {
                Some(obj) => *obj,
                None => break,
            };

            if id.generation().is_marked() {
                self.unmark(id);
                i += 1;
                continue;
            }

            self.obj_list.remove(i);
            self.free(id);
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

    pub fn alloc_obj_string2(&mut self, obj_string: ObjString2) -> ObjStringId {
        let obj_string = self.alloc_obj2(obj_string);
        self.intern_string(obj_string.as_non_null_ptr());
        obj_string
    }

    pub fn alloc_obj2<const N: u8>(&mut self, obj: Obj2) -> Index<ObjId<N>> {
        let id = match obj {
            Obj2::String(str) => cast_magic_obj_id_index(self.obj_string_arena.insert(str)),
            Obj2::Fn(f) => cast_magic_obj_id_index(self.obj_fn_arena.insert(f)),
            Obj2::Native(n) => cast_magic_obj_id_index(self.obj_native_arena.insert(n)),
            Obj2::Closure(c) => cast_magic_obj_id_index(self.obj_closure_arena.insert(c)),
            Obj2::Upvalue(u) => cast_magic_obj_id_index(self.obj_upvalue_arena.insert(u)),
            Obj2::Class(c) => cast_magic_obj_id_index(self.obj_class_arena.insert(c)),
            Obj2::Instance(i) => cast_magic_obj_id_index(self.obj_instance_arena.insert(i)),
            Obj2::BoundMethod(b) => cast_magic_obj_id_index(self.obj_bound_method_arena.insert(b)),
        };

        self.obj_list2.push_front(id);

        cast_obj_id(id)
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

    #[inline]
    pub fn intern_string2(&mut self, obj_string: ObjString) {
        self.interned_strings2.set(obj_string, Value::Nil);
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
