#![feature(cold_path)]
#![feature(macro_metavar_expr_concat)]
#![allow(unused)]

extern crate string_cache;

#[macro_use]
mod atoms {
    include!(concat!(env!("OUT_DIR"), "/kw_atom.rs"));
}

#[macro_use]
mod compiler;

pub mod tests;
pub use compiler::*;
