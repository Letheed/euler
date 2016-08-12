/* 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
 *
 * Find the sum of all numbers which are equal to the sum of the factorial of
 * their digits.
 *
 * NOTE: as 1! = 1 and 2! = 2 are not sums they are not included.
 */

#![feature(iter_arith)]

const FACTORIAL: [u64; 10] = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880];

fn main() {
    let digit_factorials = (10..6*FACTORIAL[9]).filter(|&n| n == digit_facsum(n)).collect::<Vec<_>>();
    println!("{:?}", digit_factorials);
    println!("sum = {}", digit_factorials.into_iter().sum::<u64>());
}

fn digit_facsum(mut n: u64) -> u64 {
    let mut sum = 0;
    while n != 0 {
        sum += FACTORIAL[(n % 10) as usize];
        n /= 10;
    }
    sum
}
