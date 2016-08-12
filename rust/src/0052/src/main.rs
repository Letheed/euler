/* It can be seen that the number, 125874, and its double, 251748, contain
 * exactly the same digits, but in a different order.
 *
 * Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
 * contain the same digits.
 */

fn main() {
    println!("{}", (1..).skip_while(|&n| !has_permuted_multiples(n)).next().unwrap());
}

fn has_permuted_multiples(n: u32) -> bool {
    let digits_n = digits(n);
    (2..7).all(|m| digits_n == digits(n * m))
}

fn digits(mut n: u32) -> Vec<u32> {
    let mut digits = Vec::new();
    while n != 0 {
        digits.push(n % 10);
        n /= 10;
    }
    digits.sort();
    digits
}
