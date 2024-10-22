use std::hint::black_box;

use codspeed_criterion_compat::{
    BenchmarkId, Criterion, Throughput, criterion_group, criterion_main,
};
use mitki_yellow::GreenNode;

#[salsa::input]
struct Input {
    code: &'static str,
}

#[salsa::tracked]
fn parse(db: &dyn salsa::Database, input: Input) -> GreenNode<'_> {
    mitki_parse::module(db, input.code(db))
}

fn benchmark_parser(c: &mut Criterion) {
    let db = salsa::DatabaseImpl::new();
    let inputs = vec![
        (
            "Simple",
            Input::new(
                &db,
                r#"
            fun foo() {
                42
            }
            "#,
            ),
        ),
        (
            "Medium",
            Input::new(
                &db,
                r#"
            fun foo() { 
                if true {}
                if true {} else {}
                if true {} else if false {} else {}
            }

            fun bar() {
                loop {}
            }
            "#,
            ),
        ),
    ];

    let mut group = c.benchmark_group("Parser Benchmark");

    for (name, input) in inputs {
        let code_length = input.code(&db).len() as u64;
        group.throughput(Throughput::Bytes(code_length));
        group.bench_with_input(BenchmarkId::new("parse_code", name), &input, |b, &code| {
            b.iter(|| {
                let module = parse(&db, code);
                black_box(module);
            });
        });
    }

    group.finish();
}

criterion_group!(benches, benchmark_parser);
criterion_main!(benches);
