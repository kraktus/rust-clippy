#![allow(unused)]
#![warn(clippy::confusing_xor_and_pow)]
#![allow(clippy::eq_op)]

fn main() {
    // Should warn:
    let _ = 2 ^ 9;
    let _ = 50 ^ 3;
    let _ = 5 ^ 8;
    let _ = 2 ^ 32;

    // Should not warn:
    let _ = 0x02 ^ 5;
    let _ = 2 ^ 0x02;
    let _ = 10 ^ 0b0101;
}
