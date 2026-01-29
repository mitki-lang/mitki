use mitki_hir::hir::*;
use mitki_span::Symbol;

#[salsa::db]
#[derive(Clone, Default)]
struct TestDb {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for TestDb {}

fn sym<'db>(db: &'db TestDb, text: &str) -> Symbol<'db> {
    Symbol::new(db, text)
}

#[test]
fn roundtrip_nodes() {
    let db = TestDb::default();
    let mut store = NodeStore::default();

    let name_sym = sym(&db, "x");
    let name = store.alloc_name(name_sym);
    assert_eq!(store.name(name), name_sym);

    let ty_sym = sym(&db, "T");
    let ty = store.alloc_type_ref(ty_sym);
    assert_eq!(store.type_ref(ty), ty_sym);

    let int_sym = sym(&db, "123");
    let int = store.alloc_int(Some(int_sym));
    assert_eq!(store.int(int), Some(int_sym));
    let missing_int = store.alloc_int(None);
    assert_eq!(store.int(missing_int), None);

    let float_sym = sym(&db, "1.25");
    let float = store.alloc_float(Some(float_sym));
    assert_eq!(store.float(float), Some(float_sym));

    let string_sym = sym(&db, "hello");
    let string = store.alloc_string(Some(string_sym));
    assert_eq!(store.string(string), Some(string_sym));

    let char_sym = sym(&db, "'a'");
    let ch = store.alloc_char(Some(char_sym));
    assert_eq!(store.char(ch), Some(char_sym));

    let _true = store.alloc_true();
    let _false = store.alloc_false();
    let _error = store.alloc_error();

    let tuple = store.alloc_tuple(vec![name.into(), int.into()]);
    let tuple_items: Vec<_> = store.tuple(tuple).iter().collect();
    assert_eq!(tuple_items, vec![name.into(), int.into()]);

    let call = store.alloc_call(name.into(), vec![int.into(), float.into()]);
    let (call_callee, call_args) = store.call(call);
    let call_args: Vec<_> = call_args.iter().collect();
    assert_eq!(call_callee, name.into());
    assert_eq!(call_args, vec![int.into(), float.into()]);

    let binary = store.alloc_binary(name.into(), int.into(), float.into());
    let binary = store.binary(binary);
    assert_eq!(binary.lhs, name.into());
    assert_eq!(binary.op, int.into());
    assert_eq!(binary.rhs, float.into());

    let if_expr = store.alloc_if(name.into(), int.into(), float.into());
    let if_expr = store.if_expr(if_expr);
    assert_eq!(if_expr.cond, name.into());
    assert_eq!(if_expr.then_branch, int.into());
    assert_eq!(if_expr.else_branch, float.into());

    let local = store.alloc_local_var(name, ty.into(), int.into());
    let local = store.local_var(local);
    assert_eq!(local.name, name);
    assert_eq!(local.initializer, int.into());

    let stmt_expr: StmtId = ExprId::from(name).into();
    let stmt_local: StmtId = store.alloc_local_var(name, ty.into(), int.into()).into();
    let tail = float.into();
    let block = store.alloc_block(vec![stmt_local, stmt_expr], tail);
    let (stmts, block_tail) = store.block_stmts(block);
    let stmts: Vec<_> = stmts.iter().collect();
    assert_eq!(stmts, vec![stmt_local, stmt_expr]);
    assert_eq!(block_tail, tail);

    let param = store.alloc_param(sym(&db, "p"), ty.into());
    let (param_name, param_ty) = store.param(param);
    assert_eq!(store.name(param_name), sym(&db, "p"));
    assert_eq!(param_ty, ty.into());

    let closure = store.alloc_closure(vec![param], tail);
    let (params, closure_body) = store.closure_parts(closure);
    let params: Vec<_> = params.iter().collect();
    assert_eq!(params, vec![param]);
    assert_eq!(closure_body, tail);

    let prefix = store.alloc_prefix(int.into(), name.into());
    let prefix = store.prefix(prefix);
    assert_eq!(prefix.op, int.into());
    assert_eq!(prefix.expr, name.into());

    let postfix = store.alloc_postfix(name.into(), int.into());
    let postfix = store.postfix(postfix);
    assert_eq!(postfix.expr, name.into());
    assert_eq!(postfix.op, int.into());
}
