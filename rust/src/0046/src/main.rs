/* It was proposed by Christian Goldbach that every odd composite number can be
 * written as the sum of a prime and twice a square.
 *
 * 9 = 7 + 2×12
 * 15 = 7 + 2×22
 * 21 = 3 + 2×32
 * 25 = 7 + 2×32
 * 27 = 19 + 2×22
 * 33 = 31 + 2×12
 *
 * It turns out that the conjecture was false.
 *
 * What is the smallest odd composite that cannot be written as the sum of a
 * prime and twice a square?
 */

#![feature(step_by)]

fn main() {
    let (primes, notprimes) = genp_oddnotp_up_to(6_000);
    'np: for np in notprimes.into_iter().skip(1) {
        for p in primes.iter().rev().filter(|&&p| p < np) {
            if is_a_square((np - p) / 2) { continue 'np }
        }
        println!("{}", np);
        return
    }
}

fn is_a_square(n: usize) -> bool {
    let sqrt = (n as f64).sqrt() as usize;
    sqrt * sqrt == n
}

fn genp_oddnotp_up_to(iubound: usize) -> (Vec<usize>, Vec<usize>) {
    let nprimes = iubound / (iubound as f64).ln() as usize;
    let mut primes = Vec::with_capacity(nprimes);
    let mut notprimes = Vec::with_capacity(iubound as usize / 2 - nprimes);
    let mut is_prime = vec![true; iubound as usize + 1];
    notprimes.push(1);
    primes.push(2);
    for p in (3..is_prime.len()).step_by(2) {
        if is_prime[p] {
            primes.push(p);
            for np in (p*p..is_prime.len()).step_by(2*p) {
                is_prime[np] = false;
            }
        }
        else { notprimes.push(p); }
    }
    (primes, notprimes)
}
