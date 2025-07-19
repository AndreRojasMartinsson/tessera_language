use bumpalo::Bump;
use codespan_reporting::files::SimpleFiles;
use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;
use tessera_language::{lexer::Lexer, parser};

const SOURCE: &str = black_box(
    r#"
extern fn void exit(i32 code);
extern const fn void exit_const(i32 code);

fn i32 main(&self, i32 anyways) {
  infer a := 3;

  infer result := a:b:c;



  io:println("Hello, world!");


  std:<T, U>:io:<P>:println;

  a.ohno.parse.bludclut;

  ok;


  while bludclut >= 10 {
    infer b := 7.;
  };

  match a {
    'a' | 'b' | 3 if a = 3 -> {},
    _ -> {}
  }

  |> 4;
}

"#,
);

fn compile(source: &str) {
    let lexer = Lexer::new(source);
    let mut files = SimpleFiles::new();

    let file_id = files.add("example.tes", source);
    let arena = Bump::new();

    let mut parser = parser::Parser::new(&arena, source, lexer, files, file_id);

    let _ = parser.parse().unwrap();
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("compile program", |b| b.iter(|| compile(SOURCE)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
