/* Find the sum of the digits in the number 100!
 */

#![feature(iter_arith)]

extern crate num;

use num::bigint::BigUint;
use num::traits::One;

fn main() {
    let n = (2..101u8).fold(BigUint::one(), |n, m| n * BigUint::from(m));
    let sum = n.to_str_radix(10).as_bytes().iter().map(|&d| (d as char).to_digit(10).unwrap()).sum::<u32>();
    println!("{}", sum);
}
