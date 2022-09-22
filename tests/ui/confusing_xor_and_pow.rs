#![allow(unused)]
#![warn(clippy::confusing_xor_and_pow)]
#![allow(clippy::eq_op)]

fn main() {
    // Should warn:
    let _ = 2i32 ^ 9i32;
    let _ = 50i32 ^ 3i32;
    let _ = 5i32 ^ 8i32;
    let _ = 2i32 ^ 32i32;

    // Should not warn:
    let _ = 0x02 ^ 5;
    let _ = 2 ^ 0x02;
    let _ = 10 ^ 0b0101;
}
