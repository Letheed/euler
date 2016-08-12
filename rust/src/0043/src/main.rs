/* The number, 1406357289, is a 0 to 9 pandigital number because it is made up
 * of each of the digits 0 to 9 in some order, but it also has a rather
 * interesting sub-string divisibility property.
 *
 * Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:
 *
 *    d2d3d4=406 is divisible by 2
 *    d3d4d5=063 is divisible by 3
 *    d4d5d6=635 is divisible by 5
 *    d5d6d7=357 is divisible by 7
 *    d6d7d8=572 is divisible by 11
 *    d7d8d9=728 is divisible by 13
 *    d8d9d10=289 is divisible by 17
 *
 * Find the sum of all 0 to 9 pandigital numbers with this property.
 */

#![feature(iter_arith)]

fn main() {
    let mut pan09 = Vec::new();
    let mut digits = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    for d in 1..10 {
        digits.swap(d, 9);
        gen_pan09(d as u64, &mut digits[..9], &mut pan09);
        digits.swap(d, 9);
    }
    println!("{:?}\nsum = {}", pan09, pan09.iter().sum::<u64>());
}

fn gen_pan09(mut n: u64, digits: &mut [u64], pan09: &mut Vec<u64>) {
    let dlen = digits.len();
    if dlen == 0 && is_subdivisible(n) { pan09.push(n); }
    else {
        n *= 10;
        for i in 0..dlen {
            let m = n + digits[i];
            digits.swap(i, dlen - 1);
            gen_pan09(m, &mut digits[..dlen - 1], pan09);
            digits.swap(i, dlen - 1);
        }
    }
}

fn is_subdivisible(mut n: u64) -> bool {
    let divisors = [2, 3, 5, 7, 11, 13, 17];
    for d in divisors.iter().rev() {
        if (n % 1000) % d != 0 { return false }
        n /= 10;
    }
    true
}
