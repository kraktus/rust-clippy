#![feature(lint_reasons)]
#![warn(clippy::manual_filter)]

fn main() {
    match Some(0) {
        None => None,
        Some(x) => {
            if x > 0 {
                None
            } else {
                Some(x)
            }
        },
    };

    match Some(1) {
        Some(x) => {
            if x > 0 {
                None
            } else {
                Some(x)
            }
        },
        None => None,
    };

    match Some(2) {
        Some(x) => {
            if x > 0 {
                None
            } else {
                Some(x)
            }
        },
        _ => None,
    };

    match Some(3) {
        Some(x) => {
            if x > 0 {
                Some(x)
            } else {
                None
            }
        },
        None => None,
    };

    let y = Some(4);
    match y {
        // Some(4)
        None => None,
        Some(x) => {
            if x > 0 {
                None
            } else {
                Some(x)
            }
        },
    };

    match Some(5) {
        Some(x) => {
            if x > 0 {
                Some(x)
            } else {
                None
            }
        },
        _ => None,
    };

    match Some(6) {
        Some(ref x) => {
            if x > &0 {
                Some(x)
            } else {
                None
            }
        },
        _ => None,
    };

    let external_cond = true;
    match Some(String::new()) {
        Some(x) => {
            if external_cond {
                Some(x)
            } else {
                None
            }
        },
        _ => None,
    };

    if let Some(x) = Some(7) {
        if external_cond { Some(x) } else { None }
    } else {
        None
    };

    match &Some(8) {
        &Some(x) => {
            if x != 0 {
                Some(x)
            } else {
                None
            }
        },
        _ => None,
    };

    match Some(9) {
        Some(x) => {
            if x > 10 && x < 100 {
                Some(x)
            } else {
                None
            }
        },
        None => None,
    };

    const fn f1() {
        // Don't lint, `.filter` is not const
        match Some(10) {
            Some(x) => {
                if x > 10 && x < 100 {
                    Some(x)
                } else {
                    None
                }
            },
            None => None,
        };
    }

    #[expect(clippy::blocks-in-if-conditions)]
    match Some(11) {
        // Lint, statement is preserved by `.filter`
        Some(x) => {
            if {
                println!("foo");
                x > 10 && x < 100
            } {
                Some(x)
            } else {
                None
            }
        },
        None => None,
    };

    #[expect(clippy::blocks-in-if-conditions)]
    match Some(12) {
        // Don't Lint, statement is lost by `.filter`
        Some(x) => {
            if x > 10 && x < 100 {
                println!("foo");
                Some(x)
            } else {
                None
            }
        },
        None => None,
    };

    match Some(13) {
        // Don't Lint, because of `None => Some(1)`
        Some(x) => {
            if x > 10 && x < 100 {
                println!("foo");
                Some(x)
            } else {
                None
            }
        },
        None => Some(1),
    };

    unsafe fn f(x: u32) -> bool {
        true
    }
    let _ = match Some(14) {
        Some(x) => {
            if unsafe { f(x) } {
                Some(x)
            } else {
                None
            }
        },
        None => None,
    };
    let _ = match Some(15) {
        Some(x) => unsafe {
            if f(x) { Some(x) } else { None }
        },
        None => None,
    };
}
