/* The prime 41, can be written as the sum of six consecutive primes:
 *
 * 41 = 2 + 3 + 5 + 7 + 11 + 13
 *
 * This is the longest sum of consecutive primes that adds to a prime below
 * one-hundred.
 *
 * The longest sum of consecutive primes below one-thousand that adds to a
 * prime, contains 21 terms, and is equal to 953.
 *
 * Which prime, below one-million, can be written as the sum of the most
 * consecutive primes?
 */

#![feature(step_by)]

const N: usize = 1_000_000;

fn main() {
    let primes = genprime_below(N);
    let mut longest_sum = 0;
    let mut sum_length = 0;
    let mut i = 0;
    while i < (primes.len() - sum_length) {
        let mut sum = 0;
        let mut length = 0;
        for &p in &primes[i..] {
            sum += p;
            length += 1;
            if sum >= N { break }
            if length > sum_length && primes.binary_search(&sum).is_ok() {
                sum_length = length;
                longest_sum = sum;
            }
        }
        i += 1;
    }
    println!("{} ({} terms)", longest_sum, sum_length);
}

fn genprime_below(n: usize) -> Vec<usize> {
    let mut primes = Vec::with_capacity(n / (n as f32).ln() as usize);
    let mut is_prime = vec![true; n];
    primes.push(2);
    for p in (3..n).step_by(2) {
        if is_prime[p] {
            primes.push(p as usize);
            for np in (p*p..n).step_by(2*p) { is_prime[np] = false; }
        }
    }
    primes
}
