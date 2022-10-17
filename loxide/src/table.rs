use std::{
    alloc::{self, Layout},
    hash::Hash,
    ops::{Deref, DerefMut},
    ptr::{null_mut, NonNull},
};

use fnv::FnvHashMap;

use crate::{obj::ObjString, value::Value};

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct LoxHash(pub u32);

impl LoxHash {
    pub fn hash_string(string: &str) -> LoxHash {
        let string_bytes = string.as_bytes();
        let mut hash = 2166136261u32;

        for i in 0..string.len() {
            hash ^= string_bytes[i] as u32;
            hash = hash.overflowing_mul(16777619).0;
            // hash *= ;
        }

        LoxHash(hash)
    }
}

pub struct Table {
    pub len: u32,
    pub cap: u32,
    pub entries: *mut Entry,
}

#[derive(Copy, Clone, Debug)]
pub struct Entry {
    pub key: *mut ObjString,
    pub value: Value,
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

    fn entries_slice(&self) -> Option<&[Entry]> {
        // Safety:
        // `self.entries` layout is an array of `self.cap` length so its okay to create a slice of it
        if self.entries.is_null() {
            return None;
        } else {
            Some(unsafe { std::slice::from_raw_parts(self.entries, self.cap as usize) })
        }
    }

    pub fn add_all(&self, to: &mut Self) {
        match self.entries_slice() {
            Some(entries) => {
                for entry in entries.iter() {
                    if !entry.key.is_null() {
                        to.set(entry.key, entry.value);
                    }
                }
            }
            None => (),
        }
    }

    pub fn adjust_capacity(&mut self, new_cap: u32) {
        let mut entries = vec![
            Entry {
                key: null_mut(),
                value: Value::Nil
            };
            new_cap as usize
        ];

        if !self.entries.is_null() {
            let mut new_len = 0;

            // Drop old
            // Safety:
            // We don't need to call drop on each Entry so Vec deallocating is just fine
            let old_entries =
                unsafe { Vec::from_raw_parts(self.entries, self.len as usize, self.cap as usize) };

            for entry in old_entries.iter() {
                unsafe {
                    if entry.key.is_null() {
                        continue;
                    }

                    let dest =
                        Self::find_entry_from_ptr(entries.as_mut_ptr(), new_cap, (*entry).key);
                    (*dest).key = entry.key;
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

    pub fn set(&mut self, key: *mut ObjString, val: Value) -> bool {
        if self.len as f32 + 1.0 > self.cap as f32 * Self::TABLE_MAX_LOAD {
            let new_cap = if self.cap < 8 { 8 } else { self.cap * 2 };
            self.adjust_capacity(new_cap);
        }

        let entry = self.find_entry_mut(key);

        let is_new_key = entry.key.is_null();
        let should_increment_len = is_new_key && matches!(entry.value, Value::Nil);

        entry.key = key;
        entry.value = val;

        if should_increment_len {
            self.len += 1;
        }

        is_new_key
    }

    pub fn delete(&mut self, key: *mut ObjString) -> bool {
        if self.len == 0 {
            return false;
        }

        let entry = self.find_entry_mut(key);
        if entry.key.is_null() {
            return false;
        }

        // place tombstone
        entry.key = null_mut();
        entry.value = Value::Bool(true);
        true
    }

    pub fn get(&self, key: *mut ObjString) -> Option<Value> {
        if self.len == 0 {
            return None;
        }

        let entry = self.find_entry(key);
        if entry.key.is_null() {
            return None;
        }

        Some(entry.value)
    }

    pub fn find_entry(&self, key: *mut ObjString) -> &Entry {
        unsafe {
            Self::find_entry_from_ptr(self.entries, self.cap, key)
                .as_ref()
                .unwrap()
        }
    }

    pub fn find_entry_mut(&mut self, key: *mut ObjString) -> &mut Entry {
        unsafe {
            Self::find_entry_from_ptr(self.entries, self.cap, key)
                .as_mut()
                .unwrap()
        }
    }

    pub fn find_string(&self, string: &str, hash: LoxHash) -> Option<NonNull<ObjString>> {
        if self.len == 0 {
            return None;
        }

        let entries = match self.entries_slice() {
            Some(entries) => entries,
            None => return None,
        };

        let mut index = hash.0 % self.cap;

        unsafe {
            loop {
                let entry = entries[index as usize];
                if entry.key.is_null() {
                    if matches!(entry.value, Value::Nil) {
                        return None;
                    }
                } else if (*entry.key).len == string.len() as u32
                    && (*entry.key).hash == hash
                    && (&*entry.key).as_str() == string
                {
                    return Some(NonNull::new_unchecked(entry.key));
                }

                index = (index + 1) % self.cap;
            }
        }
    }

    fn find_entry_from_ptr(entries: *mut Entry, cap: u32, key: *mut ObjString) -> *mut Entry {
        let mut index = unsafe { (*key).hash.0 } % cap;
        let mut tombstone: *mut Entry = null_mut();

        loop {
            unsafe {
                let entry = entries.offset(index as isize);
                if (*entry).key.is_null() {
                    if matches!((*entry).value, Value::Nil) {
                        return if !tombstone.is_null() {
                            tombstone
                        } else {
                            entry
                        };
                    } else {
                        tombstone = entry;
                    }
                } else if (*entry).key == key {
                    return entry;
                }

                index = (index + 1) % cap;
            }
        }
    }

    pub fn free(table: &mut Table) {
        if table.entries.is_null() {
            return;
        }

        let _v =
            unsafe { Vec::from_raw_parts(table.entries, table.len as usize, table.cap as usize) };
        table.len = 0;
        table.cap = 0;
        table.entries = null_mut();
    }
}
