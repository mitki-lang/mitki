#[test]
fn id_mixups_fail_to_compile() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/trybuild/*.rs");
}
