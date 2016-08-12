/* Find the difference between the sum of the squares of the first one hundred
 * natural numbers and the square of the sum.
 */

#![feature(iter_arith)]

fn main() {
    println!("{}", ((1..101).sum::<u64>()).pow(2) -  (1..101).map(|n| n * n).sum::<u64>());
}
