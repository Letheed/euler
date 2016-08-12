/* The number, 197, is called a circular prime because all rotations of the
 * digits: 197, 971, and 719, are themselves prime.
 *
 * There are thirteen such primes below 100:
 * 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
 *
 * How many circular primes are there below one million?
 */

#![feature(iter_arith, step_by)]

fn main() {
    let prime_set = potentially_circular_primes_below(1_000_000);
    let mut circular_primes = Vec::new();
    for &p in &prime_set {
        if circular_primes.binary_search(&p).is_err() {
            let rotations = rotations(p);
            if rotations.iter().all(|n| prime_set.binary_search(n).is_ok()) {
                if !rotations.contains(&p) { circular_primes.push(p); }
                circular_primes.extend_from_slice(&rotations);
                circular_primes.sort();
            }
        }
    }
    println!("{}", circular_primes.iter().count());
}

fn potentially_circular_primes_below(n: usize) -> Vec<usize> {
    let mut primes = vec![2];
    let mut is_prime = vec![true; n];
    for m in (3..n).step_by(2) {
        if is_prime[m] {
            if can_be_circular(m) { primes.push(m); }
            for i in (m*m..n).step_by(2*m) {
                is_prime[i] = false;
            }
        }
    }
    primes
}

fn can_be_circular(mut n: usize) -> bool {
    n /= 10;
    while n != 0 {
        if (n % 10) % 2 == 0 { return false }
        n /= 10;
    }
    true
}

fn rotations(mut n: usize) -> Vec<usize> {
    let order = (n as f32).log10() as usize;
    let mut rotations = Vec::with_capacity(order);
    let multiple = std::iter::repeat(10).take(order).product::<usize>();
    for _ in 0..order {
        let d = n % 10;
        n /= 10;
        n += d * multiple;
        rotations.push(n);
    }
    rotations
}
