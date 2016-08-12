/* The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
 *
 * Find the sum of all numbers, less than one million, which are palindromic in
 * base 10 and base 2.
 *
 * NOTE: that the palindromic number, in either base, may not include leading zeros.)
 */

#![feature(iter_arith, step_by)]

fn main() {
    let sum = (1..1_000_000).step_by(2).filter(|&n| is_palindromic_b10(n) && is_palindromic_b2(n)).sum::<u32>();
    println!("{}", sum);
}

fn is_palindromic_b10(n: u32) -> bool {
    let mut n2 = n;
    let mut m = 0;
    while n2 != 0 {
        m = (m * 10) + (n2 % 10);
        n2 /= 10;
    }
    n == m
}

fn is_palindromic_b2(n: u32) -> bool {
    let mut n2 = n;
    let mut m = 0;
    while n2 != 0 {
        m = (m << 1) + (n2 & 1);
        n2 >>= 1;
    }
    n == m
}
