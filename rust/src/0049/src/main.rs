/* The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
 * increases by 3330, is unusual in two ways: (i) each of the three terms are
 * prime, and, (ii) each of the 4-digit numbers are permutations of one another.
 *
 * There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
 * exhibiting this property, but there is one other 4-digit increasing sequence.
 *
 * What 12-digit number do you form by concatenating the three terms in this
 * sequence?
 */

#![feature(step_by)]

fn main() {
    let (primes, is_prime) = genprime_below(10_000);
    let i4digit = primes.binary_search(&1000).err().unwrap();
    let primes = &primes[i4digit..];
    for (i, &p0) in primes.iter().enumerate() {
        let p1_max = p0 + (*primes.last().unwrap() - p0) / 2;
        let imax = match primes.binary_search(&p1_max) {
            Ok(i)  => i + 1,
            Err(i) => i,
        };
        for &p1 in &primes[i+1..imax] {
            let p2 = p1 + (p1 - p0);
            if is_prime[p2 as usize] && are_permutations(p0, p1, p2) {
                println!("    {}, {}, {}", p0, p1, p2);
            }
        }
    }
}

fn are_permutations(p0: u32, p1: u32, p2: u32) -> bool {
    let digits_p0 = digits(p0);
    digits_p0 == digits(p1) && digits_p0 == digits(p2)
}

fn digits(mut p0: u32) -> [u32; 4] {
    debug_assert!(1000 <= p0 && p0 < 10_000);
    let mut digits = [0; 4];
    for d in digits.iter_mut() {
        *d = p0 % 10;
        p0 /= 10;
    }
    digits.sort();
    digits
}

fn genprime_below(n: usize) -> (Vec<u32>, Vec<bool>) {
    let mut primes = Vec::with_capacity(n / (n as f32).ln() as usize);
    let mut is_prime = vec![true; n];
    for p in (3..n).step_by(2) {
        if is_prime[p] {
            primes.push(p as u32);
            for np in (p*p..n).step_by(2*p) { is_prime[np] = false; }
        }
    }
    (primes, is_prime)
}
