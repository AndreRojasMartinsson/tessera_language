use std::hash::Hash;

mod top_level;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HirId(u128);
