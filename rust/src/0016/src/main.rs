/* What is the sum of the digits of the number 2^1000?
 */

#![feature(iter_arith)]

extern crate num;

use num::bigint::ToBigUint;

fn main() {
    let n = num::pow(2.to_biguint().unwrap(), 1000);
    let sum = n.to_str_radix(10).as_bytes().iter().map(|&d| (d as char).to_digit(10).unwrap()).sum::<u32>();
    println!("{}", sum);
}
