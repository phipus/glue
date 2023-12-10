use std::sync::{Mutex, Arc};

use crate::{gc::Collector, runtime::Thread};

use super::compile_file;

fn test_run_code(code: &str) {
    let gc = Mutex::new(Collector::new());
    let func = compile_file(&gc, code, "<inline>").unwrap();

    let mut t = Thread::new(Arc::new(gc));
    unsafe {
        t.push_func(func);
        t.eval().unwrap();
    }
}

#[test]
fn test_compile_print_float() {
    test_run_code("print_float(42.2);");
}

#[test]
fn test_compile_variable() {
    let code = r#"
        let a = 43.0;
        print_float(a);
    "#;

    test_run_code(code);
}