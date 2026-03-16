use std::hint::black_box;

use codspeed_criterion_compat::{Criterion, criterion_group, criterion_main};
use mitki_analysis::{ResolveIntent, Semantics};
use mitki_ide::{Analysis, FilePosition, extract_cursor_offset};
use mitki_inputs::File;
use mitki_parse::FileParse as _;
use mitki_yellow::SyntaxKind;

fn benchmark_goto_definition(c: &mut Criterion) {
    let analysis = Analysis::default();

    let fixture = r#"
        fun init() {

        }

        fun helper_one(x: i32): i32 {
            return x + 1;
        }

        fun helper_two(x: i32): i32 {
            return x * 2;
        }

        fun noise_a() { }
        fun noise_b() { }
        fun noise_c() { }
        fun noise_d() { }
        fun noise_e() { }
        fun noise_f() { }
        fun noise_g() { }
        fun noise_h() { }
        fun noise_i() { }
        fun noise_j() { }

        fun compute_complex_value(y: i32) -> i32 {
            let a = helper_one(y);
            let b = helper_two(y);
            let result = a + b;
            return result;
        }

        fun extra1() { }
        fun extra2() { }
        fun extra3() { }
        fun extra4() { }
        fun extra5() { }
        fun extra6() { }
        fun extra7() { }
        fun extra8() { }
        fun extra9() { }
        fun extra10() { }

        fun main() {
            init();
            let value = compute_complex_value$0(42);
            println!("Computed: {}", value);
        }
    "#;
    let (offset, fixture_text) = extract_cursor_offset(fixture);

    let file = File::new(analysis.db(), "goto_complex_test".into(), fixture_text.clone());
    let file_position = FilePosition { file, offset };

    c.bench_function("goto_definition_complex", |b| {
        b.iter(|| {
            if let Some((_def, focus)) = analysis.goto_definition(file_position) {
                black_box(focus);
            } else {
                panic!("goto_definition returned an error");
            }
        })
    });
}

fn benchmark_binding_at(c: &mut Criterion) {
    let analysis = Analysis::default();
    let fixture = r#"
        fun helper(x: i32): i32 {
            return x + 1;
        }

        fun compute(y: i32): i32 {
            let alpha = helper(y);
            let beta = alpha * 2;
            let gamma = beta + alpha;
            let delta = gamma + beta;
            let epsilon = delta + gamma;
            let zeta = epsilon + delta;
            let eta = zeta + epsilon;
            let theta = eta + zeta;
            let result = theta + e$0ta;
            return result;
        }
    "#;
    let (offset, fixture_text) = extract_cursor_offset(fixture);
    let file = File::new(analysis.db(), "binding_at_test".into(), fixture_text);
    let semantics = Semantics::new(analysis.db(), file);
    let node = name_node_at(file, analysis.db(), offset);

    c.bench_function("binding_at_large_body", |b| {
        b.iter(|| {
            let binding = semantics.binding_at(analysis.db(), &node, ResolveIntent::Any);
            black_box(binding);
        })
    });
}

fn benchmark_enum_variant_lookup(c: &mut Criterion) {
    let analysis = Analysis::default();
    let fixture = r#"
        enum Color {
            Red,
            Green,
            Blue,
        }

        enum Shape {
            Red,
        }

        fun main() {
            Color.$0Red
        }
    "#;
    let (offset, fixture_text) = extract_cursor_offset(fixture);
    let file = File::new(analysis.db(), "enum_variant_test".into(), fixture_text);
    let semantics = Semantics::new(analysis.db(), file);
    let node = name_node_at(file, analysis.db(), offset);

    c.bench_function("binding_at_enum_variant", |b| {
        b.iter(|| {
            let binding = semantics.binding_at(analysis.db(), &node, ResolveIntent::Any);
            black_box(binding);
        })
    });
}

fn name_node_at<'db>(
    file: File,
    db: &'db dyn salsa::Database,
    offset: text_size::TextSize,
) -> mitki_yellow::SyntaxNode<'db> {
    let root = file.parse(db).syntax_node();
    let token = root
        .token_at_offset(offset)
        .filter(|token| !token.is_trivia())
        .max_by_key(|token| usize::from(token.kind() == SyntaxKind::NAME))
        .expect("name token");
    token.parent()
}

criterion_group!(
    benches,
    benchmark_goto_definition,
    benchmark_binding_at,
    benchmark_enum_variant_lookup
);
criterion_main!(benches);
