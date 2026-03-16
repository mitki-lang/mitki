use mitki_db::{RootDatabase, check_file};
use mitki_errors::Diagnostic;
use mitki_inputs::File;

#[derive(Debug, PartialEq, Eq)]
struct ExpectedDiag {
    line: usize,
    message: String,
}

#[derive(Debug, PartialEq, Eq)]
struct ActualDiag {
    line: usize,
    message: String,
}

fn parse_expectations(fixture: &str) -> Vec<ExpectedDiag> {
    let mut expected = Vec::new();

    for (idx, line) in fixture.lines().enumerate() {
        let Some((_, comment)) = line.split_once("//~") else {
            continue;
        };
        let comment = comment.trim();
        let comment = comment.strip_prefix("ERROR").unwrap_or(comment).trim();
        if comment.is_empty() {
            continue;
        }
        expected.push(ExpectedDiag { line: idx + 1, message: comment.to_owned() });
    }

    expected
}

fn collect_actual(db: &RootDatabase, file: File, diagnostics: &[Diagnostic]) -> Vec<ActualDiag> {
    let line_index = file.line_index(db);
    let mut actual = diagnostics
        .iter()
        .map(|diag| {
            let line = line_index.line_col(diag.range().start()).line as usize + 1;
            ActualDiag { line, message: diag.message().to_owned() }
        })
        .collect::<Vec<_>>();
    actual.sort_by_key(|diag| (diag.line, diag.message.clone()));
    actual
}

#[track_caller]
fn check(fixture: &str) {
    let db = RootDatabase::default();
    let file = File::new(&db, "typeck.mitki".into(), fixture.to_owned());

    let diagnostics = check_file(&db, file);
    let mut actual = collect_actual(&db, file, diagnostics);
    let mut expected = parse_expectations(fixture);

    expected.sort_by_key(|diag| (diag.line, diag.message.clone()));

    assert_eq!(
        expected.len(),
        actual.len(),
        "expected {} diagnostic(s), got {}\nexpected: {expected:#?}\nactual: {actual:#?}",
        expected.len(),
        actual.len(),
    );

    for expected_diag in expected {
        let Some(pos) = actual.iter().position(|diag| {
            diag.line == expected_diag.line && diag.message.contains(&expected_diag.message)
        }) else {
            panic!(
                "missing diagnostic on line {} containing `{}`\nactual: {actual:#?}",
                expected_diag.line, expected_diag.message
            );
        };
        actual.remove(pos);
    }

    assert!(actual.is_empty(), "unexpected diagnostics:\n{actual:#?}");
}

#[test]
fn unresolved_identifier() {
    check(
        r#"
fun main() {
    x //~ ERROR Unresolved identifier
}
"#,
    );
}

#[test]
fn unknown_type_annotation() {
    check(
        r#"
fun main() {
    val x: Nope = 1 //~ ERROR Unknown type `Nope`
}
"#,
    );
}

#[test]
fn unknown_type_is_error() {
    check(
        r#"
fun main() {
    val f = { x in x + x } //~ ERROR cannot infer type
}
"#,
    );
}

#[test]
fn missing_parameter_type_is_type_error() {
    check(
        r#"
fun main(x) { //~ ERROR Parameter type annotation is required
}
"#,
    );
}

#[test]
fn missing_parameter_type_still_allows_inference_from_usage() {
    check(
        r#"
fun main(x) { //~ ERROR Inferred `int`
    val y: int = x
}
"#,
    );
}

#[test]
fn missing_parameter_type_in_callee_signature_still_infers_from_call() {
    check(
        r#"
fun id(x) { //~ ERROR Parameter type annotation is required
}

fun main() {
    id(42)
}
"#,
    );
}

#[test]
fn mismatched_annotation() {
    check(
        r#"
fun main() {
    val x: int = true //~ ERROR expected `int`, found `bool`
}
"#,
    );
}

#[test]
fn if_condition_must_be_bool() {
    check(
        r#"
fun main() {
    if 1 { //~ ERROR expected `bool`, found `int`
    }
}
"#,
    );
}

#[test]
fn type_used_as_value() {
    check(
        r#"
fun main() {
    int //~ ERROR expected value, found type `int`
}
"#,
    );
}

#[test]
fn reserved_compiler_intrinsic_name_is_error() {
    check(
        r#"
fun comptime() { //~ ERROR reserved compiler intrinsic name
}
"#,
    );
}

#[test]
fn reflection_intrinsics_require_comptime_fun() {
    check(
        r#"
fun main(): str {
    type_name(int) //~ ERROR only allowed inside `comptime fun`
}
"#,
    );
}

#[test]
fn comptime_requires_comptime_function_target() {
    check(
        r#"
fun helper(): int {
    42
}

fun main(): int {
    comptime(helper()) //~ ERROR direct call to a top-level `comptime fun`
}
"#,
    );
}

#[test]
fn comptime_requires_zero_arg_target() {
    check(
        r#"
comptime fun helper(value: int): int {
    value
}

fun main(): int {
    comptime(helper(42)) //~ ERROR target function to have no parameters
}
"#,
    );
}

#[test]
fn comptime_rejects_generic_targets() {
    check(
        r#"
comptime fun helper[T](): int {
    42
}

fun main(): int {
    comptime(helper()) //~ ERROR comptime does not support generic functions
}
"#,
    );
}

#[test]
fn binary_operator_type_mismatch() {
    check(
        r#"
fun main() {
    1 + true //~ ERROR cannot apply `+` to `int` and `bool`
}
"#,
    );
}

#[test]
fn if_branch_type_mismatch() {
    check(
        r#"
fun main() {
    if true {
        1 //~ ERROR expected `()`, found `int`
    } else {
        false //~ ERROR expected `()`, found `bool`
    }
}
"#,
    );
}

#[test]
fn if_missing_else_in_value_position() {
    check(
        r#"
fun main() {
    val x: int = if true { //~ ERROR missing `else` branch
        1
    }
}
"#,
    );
}

#[test]
fn break_outside_loop_is_error() {
    check(
        r#"
fun main() {
    break //~ ERROR `break` is only allowed inside `loop`
}
"#,
    );
}

#[test]
fn continue_outside_loop_is_error() {
    check(
        r#"
fun main() {
    continue //~ ERROR `continue` is only allowed inside `loop`
}
"#,
    );
}

#[test]
fn extern_struct_cannot_declare_destructor() {
    check(
        r#"
extern struct Handle {
    drop(var self) { //~ ERROR `extern struct` cannot declare a destructor
    }
}
"#,
    );
}

#[test]
fn destructor_must_be_var_self() {
    check(
        r#"
struct Vec {
    drop(self: Vec) { //~ ERROR destructor parameter type is implicit
    }
}
"#,
    );
}

#[test]
fn duplicate_destructor_is_error() {
    check(
        r#"
struct Vec {
    drop(var self) {},
    drop(var self) {} //~ ERROR duplicate destructor declaration
}
"#,
    );
}

#[test]
fn typed_wasm_boundary_rejects_non_copy_types() {
    check(
        r#"
struct Vec {
    value: int,

    drop(var self) {}
}

export fun main(value: Vec) { //~ ERROR typed Wasm imports/exports do not allow non-copy types
}
"#,
    );
}

#[test]
fn comptime_rejects_non_copy_return_types() {
    check(
        r#"
struct Vec {
    value: int,

    drop(var self) {}
}

comptime fun build(): Vec {
    Vec { value: 1 }
}

fun main() {
    comptime(build()) //~ ERROR return a runtime-lowerable value
}
"#,
    );
}

#[test]
fn loop_body_is_checked_as_unit() {
    check(
        r#"
fun main() {
    loop {
        1 //~ ERROR expected `()`, found `int`
    }
}
"#,
    );
}

#[test]
fn loop_body_resolves_params_and_locals() {
    check(
        r#"
struct Counter {
    value: int,
}

fun get(counter: Counter): int {
    counter.value
}

fun read(counter: Counter): int {
    var result: int = 0
    loop {
        val current: int = counter.get()
        result = current
        break
    }
    result
}
"#,
    );
}

#[test]
fn call_arity_mismatch() {
    check(
        r#"
fun add(x: int) {}

fun main() {
    add(1, 2) //~ ERROR expected 1 argument(s), found 2
}
"#,
    );
}

#[test]
fn prefix_operator_type_mismatch() {
    check(
        r#"
fun main() {
    -true //~ ERROR cannot apply `-` to `bool`
}
"#,
    );
}

#[test]
fn postfix_operator_type_mismatch() {
    check(
        r#"
fun main() {
    1! //~ ERROR cannot apply postfix `!` to `int`
}
"#,
    );
}

#[test]
fn call_non_function() {
    check(
        r#"
fun main() {
    1() //~ ERROR expected function, found `int`
}
"#,
    );
}

#[test]
fn val_without_initializer() {
    check(
        r#"
fun main() {
    val x //~ ERROR missing initializer
}
"#,
    );
}

#[test]
fn tuple_arity_mismatch() {
    check(
        r#"
fun main() {
    val x: (int, bool) = (1,) //~ ERROR expected 2 element(s), found 1
}
"#,
    );
}

#[test]
fn call_arity_too_few() {
    check(
        r#"
fun add(x: int, y: int) {}

fun main() {
    add(1) //~ ERROR expected 2 argument(s), found 1
}
"#,
    );
}

#[test]
fn prefix_operator_type_mismatch_string() {
    check(
        r#"
fun main() {
    -"a" //~ ERROR cannot apply `-` to `str`
}
"#,
    );
}

#[test]
fn postfix_operator_type_mismatch_bool() {
    check(
        r#"
fun main() {
    true! //~ ERROR cannot apply postfix `!` to `bool`
}
"#,
    );
}

// === Well-typed programs (no diagnostics expected) ===

#[test]
fn no_error_int_arithmetic() {
    check(
        r#"
fun main() {
    1 + 2;
    3 - 1;
    2 * 4;
    6 / 3;
    7 % 2;
}
"#,
    );
}

#[test]
fn no_error_float_arithmetic() {
    check(
        r#"
fun main() {
    1.0 + 2.0;
    3.0 - 1.0;
    2.0 * 4.0;
    6.0 / 3.0;
}
"#,
    );
}

#[test]
fn no_error_comparison() {
    check(
        r#"
fun main() {
    1 == 2;
    1 != 2;
    1 < 2;
    1 > 2;
    1 <= 2;
    1 >= 2;
}
"#,
    );
}

#[test]
fn no_error_logical() {
    check(
        r#"
fun main() {
    true && false;
    true || false;
}
"#,
    );
}

#[test]
fn no_error_prefix_negate() {
    check(
        r#"
fun main() {
    -1;
    -1.0;
    !true;
}
"#,
    );
}

#[test]
fn no_error_if_else_matching_branches() {
    check(
        r#"
fun main(): int {
    if true { 1 } else { 2 }
}
"#,
    );
}

#[test]
fn no_error_function_call() {
    check(
        r#"
fun add(x: int, y: int): int { x + y }

fun main() {
    add(1, 2);
}
"#,
    );
}

#[test]
fn no_error_val_binding() {
    check(
        r#"
fun main() {
    val x = 1
    val y: int = 2
    x + y;
}
"#,
    );
}

#[test]
fn no_error_closure_literal() {
    check(
        r#"
fun main() {
    val f = { x in x }
}
"#,
    );
}

#[test]
fn no_error_empty_array_with_annotation() {
    check(
        r#"
fun main(): [int] {
    []
}
"#,
    );
}

#[test]
fn no_error_nested_if() {
    check(
        r#"
fun main(): int {
    if true {
        if false { 1 } else { 2 }
    } else {
        3
    }
}
"#,
    );
}

#[test]
fn no_error_empty_function() {
    check(
        r#"
fun noop() {}
"#,
    );
}

// === Additional error tests ===

#[test]
fn binary_float_int_mismatch() {
    check(
        r#"
fun main() {
    1.0 + 1 //~ ERROR cannot apply `+` to `float` and `int`
}
"#,
    );
}

#[test]
fn binary_logical_non_bool() {
    check(
        r#"
fun main() {
    1 && 2 //~ ERROR cannot apply `&&` to `int` and `int`
}
"#,
    );
}

#[test]
fn prefix_negate_string() {
    check(
        r#"
fun main() {
    !"hello" //~ ERROR cannot apply `!` to `str`
}
"#,
    );
}

#[test]
fn return_type_mismatch() {
    check(
        r#"
fun foo(): int {
    true //~ ERROR expected `int`, found `bool`
}
"#,
    );
}

#[test]
fn multiple_errors() {
    check(
        r#"
fun main() {
    x; //~ ERROR Unresolved identifier
    y; //~ ERROR Unresolved identifier
}
"#,
    );
}

#[test]
fn call_wrong_arg_type() {
    check(
        r#"
fun foo(x: int) {}

fun main() {
    foo(true) //~ ERROR expected `int`, found `bool`
}
"#,
    );
}

#[test]
fn annotation_bool_given_int() {
    check(
        r#"
fun main() {
    val x: bool = 1 //~ ERROR expected `bool`, found `int`
}
"#,
    );
}

#[test]
fn comparison_type_mismatch() {
    check(
        r#"
fun main() {
    1 == true //~ ERROR cannot apply `==` to `int` and `bool`
}
"#,
    );
}

#[test]
fn array_literal_item_type_mismatch() {
    check(
        r#"
fun main(): [int] {
    [1, true] //~ ERROR expected `int`, found `bool`
}
"#,
    );
}

#[test]
fn array_repeat_length_must_be_int() {
    check(
        r#"
fun main(): [int] {
    [1; true] //~ ERROR expected `int`, found `bool`
}
"#,
    );
}

#[test]
fn array_ordering_is_rejected() {
    check(
        r#"
fun main() {
    [1, 2] < [1, 3] //~ ERROR cannot apply `<` to `[int]` and `[int]`
}
"#,
    );
}

#[test]
fn if_condition_string() {
    check(
        r#"
fun main() {
    if "hello" {} //~ ERROR expected `bool`, found `str`
}
"#,
    );
}

#[test]
fn call_too_many_args_zero_params() {
    check(
        r#"
fun noop() {}

fun main() {
    noop(1) //~ ERROR expected 0 argument(s), found 1
}
"#,
    );
}

#[test]
fn call_wrong_arg_count_two_params() {
    check(
        r#"
fun add(x: int, y: int): int { x + y }

fun main() {
    add(1, 2, 3); //~ ERROR expected 2 argument(s), found 3
}
"#,
    );
}

#[test]
fn no_error_generic_identity() {
    check(
        r#"
fun id[T](x: T): T { x }

fun main() {
    id(42);
    id("hello");
}
"#,
    );
}

#[test]
fn no_error_generic_two_params() {
    check(
        r#"
fun pair[A, B](a: A, b: B): (A, B) { (a, b) }

fun main() {
    pair(1, true);
}
"#,
    );
}

#[test]
fn no_error_let_polymorphism_identity() {
    check(
        r#"
fun main() {
    val id = { x in x }
    val a: int = id(1)
    val b: bool = id(true)
}
"#,
    );
}

#[test]
fn let_polymorphism_does_not_leak_outer_var() {
    check(
        r#"
fun main() {
    val bad = { x in
        val g = { y in x }
        (g(1), g(true))
    }
    val t: (int, bool) = bad(0) //~ ERROR expected `(int, bool)`, found `(int, int)`
}
"#,
    );
}

#[test]
fn generic_wrong_return_type() {
    check(
        r#"
fun id[T](x: T): T { x }

fun main() {
    val x: int = id(true) //~ ERROR expected `int`, found `bool`
}
"#,
    );
}

#[test]
fn generic_struct_type_application_substitutes_fields() {
    check(
        r#"
struct Vec[T] {
    items: [T],
}

fun main() {
    val xs: Vec[int] = Vec { items: [1, 2, 3] }
    val items: [int] = xs.items
}
"#,
    );
}

// === Struct and enum tests ===

#[test]
fn struct_type_used_as_type_annotation() {
    check(
        r#"
struct Point {
    x: int,
    y: int,
}

fun origin(): Point { origin() }
"#,
    );
}

#[test]
fn struct_type_used_as_value_is_error() {
    check(
        r#"
struct Point {
    x: int,
    y: int,
}

fun main() {
    Point //~ ERROR expected value, found type `Point`
}
"#,
    );
}

#[test]
fn enum_type_used_as_type_annotation() {
    check(
        r#"
enum Color {
    Red,
    Green,
    Blue,
}

fun paint(): Color { paint() }
"#,
    );
}

#[test]
fn enum_type_used_as_value_is_error() {
    check(
        r#"
enum Color {
    Red,
    Green,
    Blue,
}

fun main() {
    Color //~ ERROR expected value, found type `Color`
}
"#,
    );
}

#[test]
fn struct_type_mismatch() {
    check(
        r#"
struct Point {
    x: int,
    y: int,
}

fun make_point(): Point { make_point() }

fun main() {
    val x: int = make_point() //~ ERROR expected `int`, found `Point`
}
"#,
    );
}

#[test]
fn enum_type_mismatch() {
    check(
        r#"
enum Color {
    Red,
    Green,
    Blue,
}

fun make_color(): Color { make_color() }

fun main() {
    val x: int = make_color() //~ ERROR expected `int`, found `Color`
}
"#,
    );
}

// === Struct expression tests ===

#[test]
fn no_error_struct_expr() {
    check(
        r#"
struct Point {
    x: int,
    y: int,
}

fun main(): Point {
    Point { x: 1, y: 2 }
}
"#,
    );
}

#[test]
fn struct_expr_field_type_mismatch() {
    check(
        r#"
struct Point {
    x: int,
    y: int,
}

fun main(): Point {
    Point { x: true, y: 2 } //~ ERROR expected `int`, found `bool`
}
"#,
    );
}

#[test]
fn struct_expr_missing_field() {
    check(
        r#"
struct Point {
    x: int,
    y: int,
}

fun main(): Point {
    Point { x: 1 } //~ ERROR missing field `y`
}
"#,
    );
}

#[test]
fn struct_expr_unknown_field() {
    check(
        r#"
struct Point {
    x: int,
    y: int,
}

fun main(): Point {
    Point { x: 1, y: 2, z: 3 } //~ ERROR unknown field `z`
}
"#,
    );
}

#[test]
fn struct_expr_not_a_struct() {
    check(
        r#"
enum Color {
    Red,
    Green,
    Blue,
}

fun main() {
    Color { x: 1 } //~ ERROR `Color` is not a struct
}
"#,
    );
}

#[test]
fn no_error_struct_field_projection() {
    check(
        r#"
struct Point {
    x: int,
    y: int,
}

fun main() {
    val p = Point { x: 1, y: 2 };
    val x: int = p.x;
}
"#,
    );
}

#[test]
fn struct_field_projection_unknown_field() {
    check(
        r#"
struct Point {
    x: int,
    y: int,
}

fun main() {
    val p = Point { x: 1, y: 2 };
    p.z //~ ERROR unknown field `z`
}
"#,
    );
}

#[test]
fn struct_field_projection_not_a_struct() {
    check(
        r#"
fun main() {
    true.x //~ ERROR `bool` is not a struct
}
"#,
    );
}

#[test]
fn enum_variant_access_as_value() {
    check(
        r#"
enum Color {
    Red,
    Green,
}

fun takes_color(color: Color) {}

fun main() {
    val c = Color.Red;
    takes_color(c);
}
"#,
    );
}

#[test]
fn enum_variant_constructor_call() {
    check(
        r#"
enum Option {
    Some(int),
    None,
}

fun takes_option(value: Option) {}

fun main() {
    val x = Option.Some(1);
    val y = Option.None;
    takes_option(x);
    takes_option(y);
}
"#,
    );
}

#[test]
fn enum_unknown_variant_is_error() {
    check(
        r#"
enum Color {
    Red,
}

fun main() {
    Color.Blue //~ ERROR Unresolved identifier
}
"#,
    );
}

#[test]
fn enum_variant_access_without_type_prefix() {
    check(
        r#"
enum Color {
    Red,
}

fun takes_color(color: Color) {}

fun main() {
    val n = .Red;
    takes_color(n);
}
"#,
    );
}

#[test]
fn enum_variant_access_without_type_prefix_ambiguous_is_error() {
    check(
        r#"
enum Color {
    Red,
}

enum Light {
    Red,
}

fun main() {
    .Red //~ ERROR Unresolved identifier
}
"#,
    );
}

#[test]
fn enum_variant_constructor_without_type_prefix() {
    check(
        r#"
enum Option {
    Some(int),
    None,
}

fun takes_option(value: Option) {}

fun main() {
    val x = .Some(1);
    val y = .None;
    takes_option(x);
    takes_option(y);
}
"#,
    );
}

#[test]
fn enum_variant_without_type_prefix_not_emitted_too_early() {
    check(
        r#"
enum Color {
    Red,
}

fun id[T](x: T): T { x }

fun main() {
    val c: Color = id(.Red);
}
"#,
    );
}

#[test]
fn enum_variant_without_type_prefix_disambiguated_by_annotation() {
    check(
        r#"
enum Color {
    Red,
}

enum Light {
    Red,
}

fun takes_color(color: Color) {}

fun main() {
    val c: Color = .Red;
    takes_color(c);
}
"#,
    );
}

#[test]
fn enum_variant_access_without_type_prefix_without_context_is_error() {
    check(
        r#"
enum Color {
    Red,
}

fun main() {
    .Red //~ ERROR Unresolved identifier
}
"#,
    );
}

#[test]
fn enum_variant_constructor_without_type_prefix_without_context_is_error() {
    check(
        r#"
enum Option {
    Some(int),
    None,
}

fun main() {
    .Some(1) //~ ERROR Unresolved identifier
}
"#,
    );
}

#[test]
fn enum_variant_without_type_prefix_conflicting_uses_reports_error() {
    check(
        r#"
enum Color {
    Red,
}

fun main() {
    val n = .Red;
    val b: Color = n;
    val q: int = n; //~ ERROR expected `int`, found `Color`
}
"#,
    );
}

#[test]
fn enum_variant_without_type_prefix_conflicting_uses_order_independent() {
    check(
        r#"
enum Color {
    Red,
}

fun main() {
    val n = .Red;
    val q: int = n; //~ ERROR expected `int`, found `Color`
    val b: Color = n;
}
"#,
    );
}

#[test]
fn enum_variant_constructor_without_type_prefix_conflicting_uses_reports_error() {
    check(
        r#"
enum Option {
    Some(int),
    None,
}

fun main() {
    val n = .Some(1);
    val ok: Option = n;
    val bad: int = n; //~ ERROR expected `int`, found `Option`
}
"#,
    );
}

#[test]
fn enum_variant_without_type_prefix_with_wrong_expected_type_is_error() {
    check(
        r#"
enum Color {
    Red,
}

fun main() {
    val q: int = .Red //~ ERROR Unresolved identifier
}
"#,
    );
}

#[test]
fn malformed_function_body_does_not_panic_diagnostic_mapping() {
    let db = RootDatabase::default();
    let file = File::new(
        &db,
        "typeck.mitki".into(),
        r#"
fun main() {
    fun nested() {}
}
"#
        .to_owned(),
    );

    let diagnostics = check_file(&db, file);
    assert!(
        !diagnostics.is_empty(),
        "expected at least one diagnostic for malformed function body"
    );
}

#[test]
fn malformed_field_expr_does_not_panic() {
    let db = RootDatabase::default();
    let file = File::new(
        &db,
        "typeck.mitki".into(),
        r#"
fun main() {
    val x = .
}
"#
        .to_owned(),
    );

    let diagnostics = check_file(&db, file);
    assert!(
        !diagnostics.is_empty(),
        "expected at least one diagnostic for malformed field expression"
    );
}

#[test]
fn no_error_anonymous_record_literal_and_field_access() {
    check(
        r#"
fun main() {
    val obj = { x: 42, y: true };
    val x: int = obj.x;
    val y: bool = obj.y;
}
"#,
    );
}

#[test]
fn anonymous_record_unknown_field_is_error() {
    check(
        r#"
fun main() {
    val obj = { x: 42 };
    obj.y //~ ERROR unknown field `y`
}
"#,
    );
}

#[test]
fn no_error_if_with_anonymous_records() {
    check(
        r#"
fun main() {
    val a = { x: 1, y: true };
    val b = { x: 2, y: false };
    val c = if true { a } else { b };
    val x: int = c.x;
}
"#,
    );
}

#[test]
fn self_recursive_enum_typechecks() {
    check(
        r#"
enum List {
    Nil,
    Cons(int, List),
}

fun main() {
    val list = List.Cons(1, List.Nil);
}
"#,
    );
}

#[test]
fn mutually_recursive_enums_typecheck() {
    check(
        r#"
enum Even {
    Zero,
    Succ(Odd),
}

enum Odd {
    Succ(Even),
}

fun main() {
    val even = Even.Succ(Odd.Succ(Even.Zero));
}
"#,
    );
}

#[test]
fn wasm_import_without_body_is_allowed() {
    check(
        r#"
import "env" fun host(x: int): int;

fun main(): int {
    0
}
"#,
    );
}

#[test]
fn wasm_import_body_is_error() {
    check(
        r#"
import "env" fun host(): int { //~ ERROR Imported Wasm functions cannot have a body
    0
}
"#,
    );
}

#[test]
fn missing_function_body_is_error() {
    check(
        r#"
export fun answer(): int; //~ ERROR Function body is required
"#,
    );
}

#[test]
fn generic_wasm_export_is_error() {
    check(
        r#"
export fun answer[T](): int { //~ ERROR Wasm imports and exports do not support generic functions
    0
}
"#,
    );
}

#[test]
fn generic_wasm_import_origin_is_allowed_with_instance() {
    check(
        r#"
import "env" fun id[T](value: T): T;
import instance id[int];

fun main(): int {
    id(0)
}
"#,
    );
}

#[test]
fn boundary_instance_target_must_exist() {
    check(
        r#"
export instance id[int]; //~ ERROR unknown boundary instance target `id`
"#,
    );
}

#[test]
fn boundary_instance_target_must_be_generic() {
    check(
        r#"
fun id(value: int): int {
    value
}

export instance id[int]; //~ ERROR boundary instance target `id` must be a generic function
"#,
    );
}

#[test]
fn import_instance_requires_imported_generic_function() {
    check(
        r#"
fun id[T](value: T): T {
    value
}

import instance id[int]; //~ ERROR import instance target `id` must be an imported generic function
"#,
    );
}

#[test]
fn export_instance_requires_non_import_generic_function() {
    check(
        r#"
import "env" fun id[T](value: T): T;

export instance id[int]; //~ ERROR export instance target `id` must be a non-import generic function
"#,
    );
}

#[test]
fn boundary_instance_type_arg_arity_is_checked() {
    check(
        r#"
fun id[T](value: T): T {
    value
}

export instance id[int, bool]; //~ ERROR boundary instance target `id` expects 1 type argument(s), found 2
"#,
    );
}

#[test]
fn duplicate_boundary_instances_are_rejected() {
    check(
        r#"
fun id[T](value: T): T {
    value
}

export instance id[int];
export instance id[int]; //~ ERROR duplicate boundary instance declaration for `id`
"#,
    );
}

#[test]
fn runtime_name_conflict_is_allowed() {
    check(
        r#"
fun print_str(value: str) {
}

fun main() {
    print_str("ok")
}
"#,
    );
}

#[test]
fn non_exhaustive_match_is_error() {
    check(
        r#"
enum Option {
    Some(int),
    None,
}

fun main(value: Option): int {
    match value { //~ ERROR non-exhaustive match
        .Some(x) => x
    }
}
"#,
    );
}

#[test]
fn unreachable_match_arm_is_error() {
    check(
        r#"
fun main(value: bool): int {
    match value {
        _ => 1,
        false => 0 //~ ERROR unreachable match arm
    }
}
"#,
    );
}

#[test]
fn union_match_literal_patterns_typecheck() {
    check(
        r#"
fun main(value: int | bool): int {
    match value {
        true => 1,
        false => 0,
        _ => 2,
    }
}
"#,
    );
}

#[test]
fn union_match_typed_binding_patterns_typecheck() {
    check(
        r#"
fun takes_int(value: int): str {
    "int"
}

fun main(value: int | str): str {
    match value {
        s: str => s,
        n: int => takes_int(n),
    }
}
"#,
    );
}

#[test]
fn union_match_typed_wildcard_pattern_typechecks() {
    check(
        r#"
fun main(value: int | str): str {
    match value {
        _: int => "int",
        s: str => s,
    }
}
"#,
    );
}

#[test]
fn union_match_tuple_member_typechecks() {
    check(
        r#"
fun main(value: int | (bool, int)): int {
    match value {
        (flag, n) => if flag { n } else { 0 },
        _ => 1,
    }
}
"#,
    );
}

#[test]
fn union_match_tuple_member_with_typed_subpatterns_typechecks() {
    check(
        r#"
fun main(value: int | (int, str)): int {
    match value {
        (n: int, _: str) => n,
        _ => 0,
    }
}
"#,
    );
}

#[test]
fn union_match_struct_member_typechecks() {
    check(
        r#"
struct Box {
    value: int,
}

fun main(value: int | Box): int {
    match value {
        Box { value } => value,
        _ => 0,
    }
}
"#,
    );
}

#[test]
fn union_match_struct_member_with_typed_subpatterns_typechecks() {
    check(
        r#"
struct Box {
    value: int,
}

fun main(value: int | Box): int {
    match value {
        Box { value: n: int } => n,
        _ => 0,
    }
}
"#,
    );
}

#[test]
fn union_match_enum_member_typechecks() {
    check(
        r#"
enum Choice {
    Some(int),
    None,
}

fun main(value: int | Choice): int {
    match value {
        .Some(n) => n,
        .None => 0,
        _ => 1,
    }
}
"#,
    );
}

#[test]
fn union_match_enum_member_with_typed_subpatterns_typechecks() {
    check(
        r#"
enum Choice {
    Some(int),
    None,
}

fun main(value: int | Choice): int {
    match value {
        .Some(n: int) => n,
        .None => 0,
        _ => 1,
    }
}
"#,
    );
}

#[test]
fn ambiguous_union_pattern_is_error() {
    check(
        r#"
fun main(value: (int, int) | (bool, bool)): int {
    match value {
        (x, y) => 0 //~ ERROR pattern matches multiple members of union
    }
}
"#,
    );
}

#[test]
fn ambiguous_typed_union_pattern_is_error() {
    check(
        r#"
struct Box {
    value: int,
}

fun main(value: Box | { value: int }): int {
    match value {
        v: { value: int } => 0 //~ ERROR pattern matches multiple members of union
    }
}
"#,
    );
}

#[test]
fn literal_pattern_mismatch_points_at_pattern() {
    check(
        r#"
fun main(value: int): int {
    match value {
        true => //~ ERROR expected `bool`, found `int`
            1,
        _ => 0,
    }
}
"#,
    );
}

#[test]
fn typed_pattern_mismatch_points_at_pattern() {
    check(
        r#"
fun main(value: int): int {
    match value {
        _: str => 0, //~ ERROR expected `str`, found `int`
        _ => 1,
    }
}
"#,
    );
}

#[test]
fn unknown_type_in_typed_pattern_is_error() {
    check(
        r#"
fun main(value: int | str): int {
    match value {
        x: Missing => 0, //~ ERROR Unknown type `Missing`
        _ => 1,
    }
}
"#,
    );
}

#[test]
fn refutable_val_pattern_is_error() {
    check(
        r#"
fun main() { //~ ERROR refutable patterns are only allowed in `match` arms
    val 0 = 1
}
"#,
    );
}

#[test]
fn refutable_param_pattern_is_error() {
    check(
        r#"
fun main(0: int) { //~ ERROR refutable patterns are only allowed in `match` arms
}
"#,
    );
}

#[test]
fn duplicate_pattern_binding_is_error() {
    check(
        r#"
fun main() {
    val (x, x) = (1, 2) //~ ERROR duplicate binding in pattern
}
"#,
    );
}

#[test]
fn struct_pattern_missing_field_is_error() {
    check(
        r#"
struct Point {
    x: int,
    y: int,
}

fun main(point: Point): int {
    match point {
        Point { x } => x //~ ERROR missing field `y`
    }
}
"#,
    );
}

#[test]
fn struct_pattern_unknown_field_is_error() {
    check(
        r#"
struct Point {
    x: int,
    y: int,
}

fun main(point: Point): int {
    match point {
        Point { x, y, z } => x //~ ERROR unknown field `z`
    }
}
"#,
    );
}

#[test]
fn variant_pattern_unknown_variant_is_error() {
    check(
        r#"
enum Option {
    Some(int),
    None,
}

fun main(value: Option): int {
    match value {
        .Other => 0 //~ ERROR Unresolved identifier
    }
}
"#,
    );
}

#[test]
fn float_pattern_is_not_supported() {
    check(
        r#"
fun main(value: float): int {
    match value {
        1.0 => 1, //~ ERROR float patterns are not supported
        _ => 0,
    }
}
"#,
    );
}
