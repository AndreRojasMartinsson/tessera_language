use std::{env, path::Path};

extern crate string_cache_codegen;

fn main() {
    string_cache_codegen::AtomType::new("atoms::KwAtom", "atom!")
        .atoms(&[
            "fn", "as", "const", "continue", "else", "break", "extern", "for", "if", "in", "match",
            "pub", "ref", "while", "crate", "super", "self", "Self", "mut", "isz", "usz", "i8",
            "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "void", "u128", "char", "bool",
            "true", "false", "infer", "str", "float", "double", "using",
        ])
        .write_to_file(&Path::new(&env::var("OUT_DIR").unwrap()).join("kw_atom.rs"))
        .unwrap();
}
