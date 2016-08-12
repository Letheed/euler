/* Find the sum of all the primes below two million.
 */

#![feature(step_by, iter_arith)]

fn main() {
    let mut primes = vec![2];
    for n in (3..2_000_000).step_by(2) {
        if is_prime(n, &primes) {
            primes.push(n);
        }
    }
    println!("{}", primes.iter().sum::<u64>());
}

fn is_prime(n: u64, smaller_primes: &[u64]) -> bool {
    let ubound = sqrt(n);
    smaller_primes.iter().take_while(|&&p| p <= ubound).all(|d| n % d != 0)
}

fn sqrt(n: u64) -> u64 {
    (n as f64).sqrt().floor() as u64
}
