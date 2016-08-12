/* Find the sum of all the numbers that can be written as the sum of fifth
 * powers of their digits.
 */

#![feature(iter_arith)]

fn main() {
    let mut set = Vec::new();
    for n in 2..5 * 9u32.pow(5) {
        if n == powered_digits_sum(n) { set.push(n); }
    }
    println!("{:?}", set);
    println!("{}", set.iter().sum::<u32>());
}

fn powered_digits_sum(mut n: u32) -> u32 {
    let mut sum = 0;
    while n != 0 {
        let d = n % 10;
        sum += d*d*d*d*d;
        n /= 10;
    }
    sum
}
