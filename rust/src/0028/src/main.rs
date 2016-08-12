/* Starting with the number 1 and moving to the right in a clockwise direction
 * a 5 by 5 spiral is formed as follows:
 *
 *  21 22 23 24 25
 *  20  7  8  9 10
 *  19  6  1  2 11
 *  18  5  4  3 12
 *  17 16 15 14 13
 *
 * It can be verified that the sum of the numbers on the diagonals is 101.*
 *
 * What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
 * formed in the same way?
 */

const N: u64 = 1001;

fn main() {
    debug_assert!(N % 2 == 1, "N should be odd");
    let n = N / 2;
    let mut csum = 0;
    for i in 0..n { csum += corner(i); }
    let sum = 1 + 20 * (n*(n+1)/2) + 4*csum;
    println!("{}", sum);
}

#[inline]
fn corner(i: u64) -> u64 {
    1 + 8 * i*(i+1)/2
}
