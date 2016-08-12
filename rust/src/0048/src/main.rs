/* The series, 11 + 22 + 33 + ... + 1010 = 10405071317.
 * Find the last ten digits of the series, 11 + 22 + 33 + ... + 10001000.
 */

#![feature(iter_arith)]

const MOD: u64 = 10_000_000_000;

fn main() {
    println!("{}", (1..1001u64).map(|n| (1..n).fold(n, |acc, _| (acc * n) % MOD)).sum::<u64>() % MOD);
}
