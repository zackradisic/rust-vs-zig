use std::ptr::{null_mut, NonNull};

use crate::{
    mem::{Gc, Greystack},
    obj::{Obj, ObjString},
    value::Value,
};

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ObjHash(pub u32);

impl ObjHash {
    pub const EMPTY_STR_HASH: Self = ObjHash(2166136261u32);

    pub fn hash_string(string: &str) -> ObjHash {
        if string.is_empty() {
            return Self::EMPTY_STR_HASH;
        }

        let string_bytes = string.as_bytes();
        let mut hash = 2166136261u32;

        for i in 0..string.len() {
            hash ^= string_bytes[i] as u32;
            hash = hash.overflowing_mul(16777619).0;
            // hash *= ;
        }

        ObjHash(hash)
    }
}

pub struct TableIter<'a> {
    table: &'a Table,
    index: usize,
}

impl<'a> Iterator for TableIter<'a> {
    type Item = &'a Entry;

    fn next(&mut self) -> Option<Self::Item> {
        if self.table.len == 0 {
            return None;
        }

        loop {
            if self.index >= self.table.cap as usize {
                return None;
            }

            let entry = unsafe { self.table.entries.add(self.index).as_ref().unwrap() };

            self.index += 1;

            // Skip uninitialized and tombstone entries
            if !entry.key.is_null() {
                return Some(entry);
            }
        }
    }
}

pub struct TableIterMut<'a> {
    table: &'a mut Table,
    index: usize,
}

impl<'a> Iterator for TableIterMut<'a> {
    type Item = &'a mut Entry;

    fn next(&mut self) -> Option<Self::Item> {
        if self.table.len == 0 {
            return None;
        }

        loop {
            if self.index >= self.table.cap as usize {
                return None;
            }

            let entry = unsafe { self.table.entries.add(self.index).as_mut().unwrap() };

            self.index += 1;

            // Skip uninitialized and tombstone entries
            if !entry.key.is_null() {
                return Some(entry);
            }
        }
    }
}

#[derive(Clone)]
pub struct Table {
    pub len: u32,
    pub cap: u32,
    pub entries: *mut Entry,
}

#[derive(Copy, Clone)]
pub struct Entry {
    pub key: *mut ObjString,
    pub value: Value,
}

impl std::fmt::Debug for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Table")
            .field("len", &self.len)
            .field("cap", &self.cap)
            .field("entries", &self.entries)
            .finish()
    }
}

impl std::fmt::Debug for Entry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            f.debug_struct("Entry")
                .field(
                    "key",
                    &match self.key.as_ref() {
                        Some(obj_str) => obj_str.as_str(),
                        None => "null_key",
                    },
                )
                .field("(key_ptr)", &self.key)
                .field("value", &self.value)
                .finish()
        }
    }
}

impl Table {
    pub const TABLE_MAX_LOAD: f32 = 0.75;

    pub fn new() -> Self {
        Self {
            len: 0,
            cap: 0,
            entries: null_mut(),
        }
    }

    pub fn iter(&self) -> TableIter {
        TableIter {
            table: self,
            index: 0,
        }
    }

    pub fn iter_mut(&mut self) -> TableIterMut {
        TableIterMut {
            table: self,
            index: 0,
        }
    }

    fn entries_slice(&self) -> Option<&[Entry]> {
        // Safety:
        // `self.entries` layout is an array of `self.cap` length so its okay to create a slice of it
        if self.entries.is_null() {
            None
        } else {
            Some(unsafe { std::slice::from_raw_parts(self.entries, self.cap as usize) })
        }
    }

    pub fn add_all(&self, to: &mut Self) {
        match self.entries_slice() {
            Some(entries) => {
                for entry in entries.iter() {
                    match NonNull::new(entry.key) {
                        Some(key) => {
                            to.set(key, entry.value);
                        }
                        None => (),
                    }
                }
            }
            None => (),
        }
    }

    pub fn adjust_capacity(&mut self, new_cap: u32) {
        let mut entries = vec![Entry::uninitialized(); new_cap as usize];

        if !self.entries.is_null() {
            let mut new_len = 0;

            // Drop old
            // Safety:
            // We don't need to call drop on each Entry so Vec deallocating is just fine
            let old_entries =
                unsafe { Vec::from_raw_parts(self.entries, self.cap as usize, self.cap as usize) };

            for entry in old_entries.iter() {
                unsafe {
                    let key = match NonNull::new(entry.key) {
                        Some(key) => key,
                        None => continue,
                    };

                    let dest = Self::find_entry_from_ptr(entries.as_mut_ptr(), new_cap, key);
                    (*dest).key = key.as_ptr();
                    (*dest).value = entry.value;
                }
                new_len += 1;
            }

            self.len = new_len;
        }

        self.entries = entries.as_mut_ptr();
        self.cap = new_cap;

        let _ = entries.leak();
    }

    pub fn set(&mut self, key: NonNull<ObjString>, val: Value) -> bool {
        if self.len as f32 + 1.0 > self.cap as f32 * Self::TABLE_MAX_LOAD {
            let new_cap = if self.cap < 8 { 8 } else { self.cap * 2 };
            self.adjust_capacity(new_cap);
        }

        let entry = self.find_entry_mut(key);

        let is_new_key = entry.is_uninitialized();
        let should_increment_len = entry.is_uninitialized();

        entry.key = key.as_ptr();
        entry.value = val;

        if should_increment_len {
            self.len += 1;
        }

        is_new_key
    }

    pub fn delete(&mut self, key: NonNull<ObjString>) -> bool {
        if self.len == 0 {
            return false;
        }

        let entry = self.find_entry_mut(key);
        if entry.key.is_null() {
            return false;
        }

        // place tombstone
        entry.delete();
        true
    }

    pub fn get(&self, key: NonNull<ObjString>) -> Option<Value> {
        if self.len == 0 {
            return None;
        }

        let entry = self.find_entry(key);
        if entry.key.is_null() {
            return None;
        }

        Some(entry.value)
    }

    pub fn find_entry(&self, key: NonNull<ObjString>) -> &Entry {
        unsafe {
            Self::find_entry_from_ptr(self.entries, self.cap, key)
                .as_ref()
                .unwrap()
        }
    }

    pub fn find_entry_mut(&mut self, key: NonNull<ObjString>) -> &mut Entry {
        unsafe {
            Self::find_entry_from_ptr(self.entries, self.cap, key)
                .as_mut()
                .unwrap()
        }
    }

    pub fn find_string(&self, string: &str, hash: ObjHash) -> Option<Gc<ObjString>> {
        if self.len == 0 {
            return None;
        }

        let entries = match self.entries_slice() {
            Some(entries) => entries,
            None => return None,
        };

        let mut index = hash.0 & (self.cap - 1);

        unsafe {
            loop {
                let entry = entries[index as usize];
                if entry.key.is_null() {
                    if matches!(entry.value, Value::Nil) {
                        return None;
                    }
                } else if (*entry.key).len == string.len() as u32
                    && (*entry.key).hash == hash
                    && (*entry.key).as_str() == string
                {
                    return Some(Gc::new(NonNull::new_unchecked(entry.key)));
                }

                index = (index + 1) & (self.cap - 1);
            }
        }
    }

    fn find_entry_from_ptr(entries: *mut Entry, cap: u32, key: NonNull<ObjString>) -> *mut Entry {
        let mut index = unsafe { (*key.as_ptr()).hash.0 } & (cap - 1);
        let mut tombstone: *mut Entry = null_mut();

        loop {
            unsafe {
                let entry = entries.offset(index as isize);
                if (*entry).key.is_null() {
                    // It's a tombstone
                    if matches!((*entry).value, Value::Nil) {
                        return if !tombstone.is_null() {
                            tombstone
                        } else {
                            entry
                        };
                    }
                    tombstone = entry;
                } else if (*entry).key == key.as_ptr() {
                    return entry;
                }

                index = (index + 1) & (cap - 1);
            }
        }
    }

    pub fn free(table: &mut Table) {
        if table.entries.is_null() {
            return;
        }

        // free the buckets
        let _entries =
            unsafe { Vec::from_raw_parts(table.entries, table.len as usize, table.cap as usize) };

        table.len = 0;
        table.cap = 0;
        table.entries = null_mut();
    }

    pub fn mark(&self, greystack: &mut Greystack) {
        for entry in self.iter() {
            Obj::mark(entry.key.cast(), greystack);
            entry.value.mark(greystack);
        }
    }

    pub fn remove_white(&mut self) {
        for entry in self.iter_mut() {
            let is_marked = unsafe {
                // Safety:
                // TableIterMut skips over entries with null keys (unintialized and tombstoned entries)
                !entry
                    .key
                    .cast::<Obj>()
                    .as_ref()
                    .unwrap_unchecked()
                    .is_marked
            };

            if is_marked {
                entry.delete();
            }
        }
    }
}

impl Entry {
    fn delete(&mut self) {
        self.key = null_mut();
        // place tombstone
        self.value = Value::Bool(true);
    }

    fn uninitialized() -> Self {
        Self {
            key: null_mut(),
            value: Value::Nil,
        }
    }

    fn is_uninitialized(&self) -> bool {
        self.key.is_null() && self.value.is_nil()
    }

    fn is_tombstone(&self) -> bool {
        self.key.is_null() && matches!(self.value, Value::Bool(true))
    }
}
