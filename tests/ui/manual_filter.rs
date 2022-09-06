#![warn(clippy::manual_filter)]
#![warn(clippy::manual_map)] // DEBUG

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

    match Some(0) {
        Some(x) => {
            if x > 0 {
                None
            } else {
                Some(x)
            }
        },
        None => None,
    };

    match Some(0) {
        Some(x) => {
            if x > 0 {
                None
            } else {
                Some(x)
            }
        },
        _ => None,
    };

    match Some(0) {
        Some(x) => {
            if x > 0 {
                Some(x)
            } else {
                None
            }
        },
        None => None,
    };

    let y = Some(0);
    match y {
        None => None,
        Some(x) => {
            if x > 0 {
                None
            } else {
                Some(x)
            }
        },
    };
}
