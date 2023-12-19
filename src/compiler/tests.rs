use std::sync::{Arc, Mutex};

use crate::{gc::Collector, runtime::Thread};

use super::compile_file;

fn test_print_code(code: &str) {
    let gc = Mutex::new(Collector::new());
    let func = compile_file(&gc, code, "<inline>").unwrap();

    let instrs = unsafe { &(*(*func).code).instrs };
    for (i, instr) in instrs.iter().enumerate() {
        println!("{:4}: {:?}", i, instr)
    }
}

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

#[test]
fn test_if_else() {
    let code = r#"
        if false {
            print_float(42);
        } else {
            print_float(88);
        }
    "#;
    test_print_code(code);
    test_run_code(code);
}

#[test]
fn test_fn_stmt() {
    let code = r#"
        fn print_fortytwo(): unit {
            print_float(42);
        }

        fn print_float2(a: float, b: float): unit {
            print_float(a);
            print_float(b);
        }

        fn add_float(a: float, b: float) {
            return a + b;
        }

        fn abs_float(f: float) {
            if f >= 0 {
                return f;
            }
            return -f;
        }

        print_fortytwo();
        print_float2(8, 17);
        
        print_float(add_float(39.7, 2.3));

        print_float(abs_float(2));
        print_float(abs_float(-2));
    "#;

    test_run_code(code);
}
