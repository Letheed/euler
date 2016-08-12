/* Euler discovered the remarkable quadratic formula:
 *
 * n² + n + 41
 *
 * It turns out that the formula will produce 40 primes for the consecutive
 * values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is
 * divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly
 * divisible by 41.
 *
 * The incredible formula  n² − 79n + 1601 was discovered, which produces 80
 * primes for the consecutive values n = 0 to 79. The product of the
 * coefficients, −79 and 1601, is −126479.
 *
 * Considering quadratics of the form:
 *
 * n² + an + b, where |a| < 1000 and |b| < 1000
 * where |n| is the modulus/absolute value of n
 * e.g. |11| = 11 and |−4| = 4
 *
 * Find the product of the coefficients, a and b, for the quadratic expression
 * that produces the maximum number of primes for consecutive values of n,
 * starting with n = 0.
 */

#![feature(step_by)]

use primes::Primes;

mod primes {
    fn sqrt(n: u64) -> u64 {
        (n as f32).sqrt().floor() as u64
    }

    pub struct Primes {
        list: Vec<u64>,
        upper_bound: u64,
    }

    impl Primes {
        pub fn new() -> Self {
            Primes {
                list: vec![2, 3],
                upper_bound: 3,
            }
        }

        pub fn is_prime(&mut self, n: u64) -> bool {
            if n == 0 || n == 1 { return false }
            let ubound = sqrt(n);
            if self.upper_bound < ubound { self.generate_up_to(ubound); }
            self.list.iter().take_while(|&&p| p <= ubound).all(|d| n % d != 0)
        }

        fn generate_up_to(&mut self, n: u64) {
            if self.upper_bound < n {
                for m in (*self.list.last().unwrap() + 2..n + 1).step_by(2) {
                    if self.is_prime(m) {
                        self.list.push(m);
                    }
                }
                self.upper_bound = n;
            }
        }

        // Another way could be:
        // pub fn is_prime(&mut self, n: u64) -> bool {
        //     if self.upper_bound < n { self.generate_up_to(n); }
        //     self.list.binary_search(&n).is_ok()
        // }

        // fn generate_up_to(&mut self, n: u64) {
        //     if self.upper_bound < n {
        //         for m in (*self.list.last().unwrap() + 2..n + 1).step_by(2) {
        //             let is_prime = {
        //                 let ubound = sqrt(n);
        //                 self.list.iter().take_while(|&&p| p <= ubound).all(|d| m % d != 0)
        //             };
        //             if is_prime { self.list.push(m); }
        //         }
        //         self.upper_bound = n;
        //     }
        // }
    }
}

fn main() {
    let mut primes = Primes::new();
    let (mut coef_product, mut np_max) = (0, 0);
    for a in -999..1000 {
        for b in -999..1000 {
            let consecutive_primes = (0i64..).take_while(|n| primes.is_prime((n*n + a*n + b).abs() as u64))
                                             .count();
            if consecutive_primes > np_max {
                np_max = consecutive_primes;
                coef_product = a * b;
            }
        }
    }
    println!("{}", coef_product);
}
