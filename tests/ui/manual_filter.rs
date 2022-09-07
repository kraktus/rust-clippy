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

    
    match Some(6) { // Not supported for now, but it could lint and remove `ref`, because this pattern only work if `x` implements the `Copy` trait.
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
        if external_cond {
                Some(x)
            } else {
                None
            }
    } else {
        None
    };

    match &Some(8) {
        &Some(x) => if x != 0 {
                Some(x)
            } else {
                None
            },
        _ => None,
    };
}
