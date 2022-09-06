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
        Some(x) => Some(x + 1),
        _ => None,
    };

}
