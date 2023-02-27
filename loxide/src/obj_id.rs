use std::num::NonZeroU32;

use generational_arena::{GenerationCounter, Index};

use crate::obj::ObjKind;

pub type ObjStringId = Index<ObjId<{ ObjKind::STR }>>;
pub type ObjFnId = Index<ObjId<{ ObjKind::FN }>>;
pub type ObjNativeId = Index<ObjId<{ ObjKind::NATIVE }>>;
pub type ObjClosureId = Index<ObjId<{ ObjKind::CLOSURE }>>;
pub type ObjUpvalueId = Index<ObjId<{ ObjKind::UPVALUE }>>;
pub type ObjClassId = Index<ObjId<{ ObjKind::CLASS }>>;
pub type ObjInstanceId = Index<ObjId<{ ObjKind::INSTANCE }>>;
pub type ObjBoundMethodId = Index<ObjId<{ ObjKind::BOUND_METHOD }>>;

/// ObjId that could be any type (using 255 as a magic number to pass the type system)
pub type ObjIdAny = Index<ObjId<255>>;

#[inline]
pub fn cast_magic_obj_id_index<const N: u8>(val: Index<ObjId<N>>) -> ObjIdAny {
    unsafe { std::mem::transmute(val) }
}
#[inline]
pub fn cast_magic_obj_id<const N: u8>(val: ObjId<N>) -> ObjId<255> {
    unsafe { std::mem::transmute(val) }
}
#[inline]
pub fn cast_obj_id<const N: u8>(val: Index<ObjId<255>>) -> Index<ObjId<N>> {
    unsafe { std::mem::transmute(val) }
}

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct ObjId<const N: u8>(NonZeroU32);

const MAX_GENERATION_NUM: u32 = 2_u32.pow(23) - 1;
impl<const N: u8> ObjId<N> {
    pub fn from_raw_parts(is_marked: bool, ty: ObjKind, generation: u32) -> Self {
        // can have 8,388,607 generations which is probably enough
        debug_assert!(generation <= MAX_GENERATION_NUM);

        let marked_bit = if is_marked { 1 << 31 } else { 0 };
        let ty_bits = ((ty as u8) as u32) << 23;

        let value = marked_bit | ty_bits | generation;

        #[cfg(debug_assertions)]
        return Self(NonZeroU32::new(value).unwrap());
        #[cfg(not(debug_assertions))]
        return Self(unsafe { NonZeroU32::new_unchecked(value) });
    }

    #[inline]
    pub fn into_raw_parts(self) -> (bool, ObjKind, u32) {
        let is_marked = self.is_marked();
        let ty = self.ty();
        let generation = self.generation();

        (is_marked, ty, generation)
    }

    #[inline]
    pub fn generation(self) -> u32 {
        // use 0x7FFFFF to only look at the first 23 bits
        self.0.get() & 0x7FFFFF
    }

    #[inline]
    pub fn is_marked(self) -> bool {
        (self.0.get() & (1 << 31)) != 0
    }

    #[inline]
    pub fn ty(self) -> ObjKind {
        // shift back and then use 0x7FFFFFFF to extract out is_marked bit
        let ty = ((self.0.get() >> 23) & 0x7FFFFFFF) as u8;

        ty.into()
    }

    pub fn increment_generation(self) -> Self {
        debug_assert!(self.generation() <= MAX_GENERATION_NUM);
        let val = self.generation() + 1;

        #[cfg(not(debug_assertions))]
        return Self(unsafe { NonZeroU32::new_unchecked(self.0.get() | val) });
        #[cfg(debug_assertions)]
        return Self(NonZeroU32::new(self.0.get() | val).unwrap());
    }

    pub fn set_marked(self, marked: bool) -> Self {
        let val: u32 = if marked { 1 << 31 } else { 0 };

        #[cfg(not(debug_assertions))]
        return Self(unsafe { NonZeroU32::new_unchecked(self.0.get() | val) });
        #[cfg(debug_assertions)]
        return Self(NonZeroU32::new(self.0.get() | val).unwrap());
    }
}

impl<const N: u8> core::fmt::Debug for ObjId<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ObjId").field(&self.0).finish()
    }
}

impl<const N: u8> PartialOrd for ObjId<N> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.generation().cmp(&other.generation()))
    }
}

impl<const N: u8> PartialEq for ObjId<N> {
    fn eq(&self, other: &Self) -> bool {
        self.generation().eq(&other.generation())
    }
}

impl<const N: u8> GenerationCounter for ObjId<N> {
    fn to_u32(self) -> u32 {
        self.generation()
    }

    fn from_u32(val: u32) -> Self {
        #[cfg(debug_assertions)]
        return Self(NonZeroU32::new(val).unwrap());
        #[cfg(not(debug_assertions))]
        return Self(unsafe { NonZeroU32::new_unchecked(val) });
    }

    fn increment(self) -> Self {
        self.increment_generation()
    }

    fn default() -> Self {
        Self::from_raw_parts(false, N.into(), 1)
    }
}

#[cfg(test)]
mod test {

    use crate::obj_id::MAX_GENERATION_NUM;

    use super::{ObjId, ObjKind};

    #[test]
    fn make_obj_id() {
        let is_marked = false;
        let kind = ObjKind::Str;
        let generation = MAX_GENERATION_NUM;

        let objid = ObjId::<{ ObjKind::STR }>::from_raw_parts(is_marked, kind, generation);

        let raw_parts = objid.into_raw_parts();

        assert_eq!((is_marked, kind, generation), raw_parts);
    }

    #[test]
    fn size() {
        type Objid = ObjId<0>;
        assert_eq!(std::mem::size_of::<Objid>(), 4);
        assert_eq!(std::mem::size_of::<Option<Objid>>(), 4);
    }
}
