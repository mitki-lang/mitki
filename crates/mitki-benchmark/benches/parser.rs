use std::hint::black_box;

use codspeed_criterion_compat::{
    BenchmarkId, Criterion, Throughput, criterion_group, criterion_main,
};
use mitki_inputs::File;
use mitki_parse::FileParse as _;

fn benchmark_parser(c: &mut Criterion) {
    let db = salsa::DatabaseImpl::new();
    let files = vec![
        File::new(
            &db,
            "Simple".into(),
            r#"
            fun foo() {
                42
            }
            "#
            .to_string(),
        ),
        File::new(
            &db,
            "Medium".into(),
            r#"
            fun foo() { 
                if true {}
                if true {} else {}
                if true {} else if false {} else {}
            }

            fun bar() {
                loop {}
            }
            "#
            .to_string(),
        ),
    ];

    let mut group = c.benchmark_group("Parser Benchmark");

    for file in files {
        let code_length = file.text(&db).len() as u64;
        group.throughput(Throughput::Bytes(code_length));
        group.bench_with_input(
            BenchmarkId::new("parse_code", file.path(&db)),
            &file,
            |b, &file| {
                b.iter(|| {
                    let module = file.parse(&db);
                    black_box(module);
                });
            },
        );
    }

    group.finish();
}

criterion_group!(benches, benchmark_parser);
criterion_main!(benches);
