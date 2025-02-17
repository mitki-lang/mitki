use std::hint::black_box;

use codspeed_criterion_compat::{Criterion, criterion_group, criterion_main};
use mitki_ide::{Analysis, FilePosition};
use mitki_inputs::File;
use text_size::TextSize;

fn extract_offset(text: &str) -> (TextSize, String) {
    let marker = "$0";
    let pos = text.find(marker).expect("Cursor marker not found");
    let new_text = format!("{}{}", &text[..pos], &text[pos + marker.len()..]);
    (TextSize::from(pos as u32), new_text)
}

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
    let (offset, fixture_text) = extract_offset(fixture);

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

criterion_group!(benches, benchmark_goto_definition);
criterion_main!(benches);
