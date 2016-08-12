/* The number 3797 has an interesting property. Being prime itself, it is
 * possible to continuously remove digits from left to right, and remain prime
 * at each stage: 3797, 797, 97, and 7. Similarly we can work from right to
 * left: 3797, 379, 37, and 3.
 *
 * Find the sum of the only eleven primes that are both truncatable from left to
 * right and right to left.
 *
 * NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
 */

#![feature(iter_arith, step_by)]

fn main() {
    let primes = genprimes_truncatable_below(1_000_000);
    let truncatable = primes[4..].iter().filter(|&&p| is_truncatable_prime(p, &primes)).cloned().collect::<Vec<_>>();
    println!("{:?}", truncatable.iter().sum::<usize>());
}

fn is_truncatable_prime(n: usize, primes: &[usize]) -> bool {
    let mut m = n / 10;
    let mut mul = 10;
    while m != 0 {
        if primes.binary_search(&m).is_err() { return false }
        if primes.binary_search(&(n - (m * mul))).is_err() { return false }
        m /= 10;
        mul *= 10;
    }
    true
}

fn genprimes_truncatable_below(n: usize) -> Vec<usize> {
    let mut primes = vec![2];
    let mut is_prime = vec![true; n];
    for m in (3..n).step_by(2) {
        if is_prime[m] {
            if can_be_truncated(m) { primes.push(m); }
            for i in (m*m..n).step_by(2*m) {
                is_prime[i] = false;
            }
        }
    }
    primes
}

fn can_be_truncated(mut n: usize) -> bool {
    n /= 10;
    while n > 9 {
        if (n % 10) % 2 == 0 { return false }
        n /= 10;
    }
    true
}
