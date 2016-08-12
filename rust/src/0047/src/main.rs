/* The first two consecutive numbers to have two distinct prime factors are:
 *
 * 14 = 2 × 7
 * 15 = 3 × 5
 *
 * The first three consecutive numbers to have three distinct prime factors are:
 *
 * 644 = 2² × 7 × 23
 * 645 = 3 × 5 × 43
 * 646 = 2 × 17 × 19.
 *
 * Find the first four consecutive integers to have four distinct prime factors.
 * What is the first of these numbers?
 */

#![feature(step_by)]

const N: usize = 4;

fn main() {
    let (primes, notprimes) = genp_notp_up_to(150_000);
    let mut i = N - 1;
    'i: while i < notprimes.len() {
        if notprimes[i-(N-1)] + (N - 1) != notprimes[i] {
            let mut j = 1;
            while notprimes[i-j] + j == notprimes[i] { j += 1; }
            i += N - j;
            continue 'i
        }
        for j in 0..N {
            if factors_count(notprimes[i-j], &primes).is_err() {
                i += N - j;
                continue 'i
            }
        }
        println!("{}", notprimes[i-(N-1)]);
        return
    }
}

fn genp_notp_up_to(iubound: usize) -> (Vec<usize>, Vec<usize>) {
    let nprimes = iubound / (iubound as f64).ln() as usize;
    let mut primes = Vec::with_capacity(nprimes);
    let mut notprimes = Vec::with_capacity(iubound - nprimes);
    let mut is_prime = vec![true; iubound + 1];
    notprimes.push(1);
    primes.push(2);
    for p in 3..is_prime.len() {
        if p % 2 != 0 && is_prime[p] {
            primes.push(p);
            for np in (p*p..is_prime.len()).step_by(2*p) {
                is_prime[np] = false;
            }
        }
        else { notprimes.push(p); }
    }
    (primes, notprimes)
}

fn factors_count(mut n: usize, primes: &[usize]) -> Result<(), ()> {
    let mut nfactors = 0;
    'n: while n != 1 {
        for &p in primes {
            if n % p == 0 {
                n /= p;
                nfactors += 1;
                while n % p == 0 { n /= p; }
                if nfactors == N && n != 1 { return Err(()) }
                continue 'n
            }
        }
    }
    if nfactors == N { Ok(()) }
    else { Err(()) }
}
