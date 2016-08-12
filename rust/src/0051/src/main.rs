/* By replacing the 1st digit of the 2-digit number *3, it turns out that six of
 * the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
 *
 * By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit
 * number is the first example having seven primes among the ten generated numbers,
 * yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993.
 * Consequently 56003, being the first member of this family, is the smallest prime
 * with this property.
 *
 * Find the smallest prime which, by replacing part of the number (not necessarily
 * adjacent digits) with the same digit, is part of an eight prime value family.
 */

#![feature(step_by)]

const FAMILY_SIZE: usize = 8;
const PRIME_UBOUND: usize = 1_000_000;

fn main() {
    assert!(FAMILY_SIZE <= 10, "FAMILY_SIZE > 10 in base 10");
    assert!(PRIME_UBOUND == 10usize.pow(numlen(PRIME_UBOUND) as u32 - 1), "PRIME_UBOUND not a power of 10");
    let (primes, is_prime) = genprime_below(PRIME_UBOUND);
    let mut patterns = vec![Vec::new(); numlen(PRIME_UBOUND)];
    gen_patterns(&mut patterns);
    for &p in &primes {
        let patterns = &patterns[numlen(p)];
        if family(p, patterns, &is_prime) { return }
    }
}

fn family(p: usize, patterns: &[Vec<usize>], is_prime: &[bool]) -> bool {
    for pattern in patterns {
        let mut family_size = 0;
        let dmin = if *pattern.last().unwrap() == numlen(p) - 1 { 1 } else { 0 };
        for d in dmin..10 {
            let member = replace(p, pattern, d);
            if is_prime[member] { family_size += 1; }
        }
        if family_size == FAMILY_SIZE {
            print_result(p, pattern, dmin);
            return true
        }
    }
    false
}

fn replace(mut p: usize, pattern: &[usize], d: usize) -> usize {
    for &i in pattern {
        let multiple = 10usize.pow(i as u32);
        p = p - (p % (multiple * 10) - (p % multiple)) + d * multiple;
    }
    p
}

fn print_result(p: usize, pattern: &[usize], dmin: usize) {
    let family = (dmin..10).map(|d| replace(p, pattern, d)).collect::<Vec<_>>();
    println!("family = {:?}", family);
    let plen = numlen(p);
    let family_pattern = p.to_string()
                          .chars()
                          .enumerate()
                          .map(|(i, c)| if pattern.contains(&(plen - i - 1)) { '·' } else { c })
                          .collect::<String>();
    println!("family pattern: {} ({} members)", family_pattern, FAMILY_SIZE);
}

fn gen_patterns(patterns: &mut [Vec<Vec<usize>>]) {
    for (numlen, pattern_collection) in patterns.iter_mut().enumerate().skip(1) {
        let pattern = Vec::new();
        let dmin = if FAMILY_SIZE > 4 { 1 } else { 0 };
        add_patterns(pattern, pattern_collection, dmin, numlen);
    }
}

fn add_patterns(pattern: Vec<usize>, patterns: &mut Vec<Vec<usize>>, dmin: usize, dmax: usize) {
    for d in dmin..dmax {
        let mut pattern = pattern.clone();
        pattern.push(d);
        if pattern.len() != dmax { patterns.push(pattern.clone()); }
        add_patterns(pattern, patterns, d + 1, dmax);
    }
}

fn numlen(mut n: usize) -> usize {
    let mut len = 1;
    n /= 10;
    while n != 0 {
        len += 1;
        n /= 10;
    }
    len
}

fn genprime_below(n: usize) -> (Vec<usize>, Vec<bool>) {
    let mut primes = Vec::with_capacity(n / (n as f32).ln() as usize);
    let mut is_prime = vec![true; n];
    primes.push(2);
    for p in (3..n).step_by(2) {
        if is_prime[p] {
            primes.push(p as usize);
            for np in (p*p..n).step_by(2*p) { is_prime[np] = false; }
        }
    }
    (primes, is_prime)
}
