/* What is the index of the first term in the Fibonacci sequence to contain
 * 1000 digits?
 */

extern crate num;

use num::bigint::BigInt;

struct Fibonacci {
    antep: BigInt,
    prev: BigInt,
}

impl Fibonacci {
    fn new() -> Self {
        Fibonacci {
            antep: BigInt::from(-1),
            prev: BigInt::from(1),
        }
    }
}

impl Iterator for Fibonacci {
    type Item = BigInt;

    fn next(&mut self) -> Option<Self::Item> {
        let next = &self.antep + &self.prev;
        std::mem::swap(&mut self.antep, &mut self.prev);
        self.prev = next.clone();
        Some(next)
    }
}

fn main() {
    let min = num::pow(BigInt::from(10), 999);
    println!("{}", Fibonacci::new().enumerate().skip_while(|t| t.1 < min).next().unwrap().0);
}
