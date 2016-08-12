/* What is the 10 001st prime number?
 */

#![feature(step_by)]

fn main() {
    println!("{}", (3..).step_by(2).filter(|&n| is_prime(n)).nth(9999).unwrap());
}

fn is_prime(n: u64) -> bool {
    let ubound = sqrt(n);
    (3..ubound + 1).step_by(2).all(|d| n % d != 0)
}

fn sqrt(n: u64) -> u64 {
    (n as f64).sqrt().floor() as u64
}
