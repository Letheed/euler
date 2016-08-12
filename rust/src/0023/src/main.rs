/* A perfect number is a number for which the sum of its proper divisors is
 * exactly equal to the number. For example, the sum of the proper divisors of
 * 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
 *
 * A number n is called deficient if the sum of its proper divisors is less than
 * n and it is called abundant if this sum exceeds n.
 *
 * As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
 * number that can be written as the sum of two abundant numbers is 24. By
 * mathematical analysis, it can be shown that all integers greater than 28123
 * can be written as the sum of two abundant numbers. However, this upper limit
 * cannot be reduced any further by analysis even though it is known that the
 * greatest number that cannot be expressed as the sum of two abundant numbers
 * is less than this limit.
 *
 * Find the sum of all the positive integers which cannot be written as the sum
 * of two abundant numbers.
 */

#![feature(step_by, iter_arith)]

fn main() {
    let abundants = (0..28124).filter(|&n| is_abundant(n)).collect::<Vec<_>>();
    let mut sieve = vec![false; 28124];
    for n in &abundants {
        for m in &abundants {
            if n + m >= sieve.len() { break }
            sieve[n + m] = true;
        }
    }
    let sum = sieve.into_iter().enumerate().filter(|&(_, is_sum)| !is_sum).map(|(n, _)| n).sum::<usize>();
    println!("{}", sum);
}

fn is_abundant(n: usize) -> bool {
    n < divisors(n).iter().sum::<usize>()
}

fn divisors(n: usize) -> Vec<usize> {
    if n == 0 || n == 1 { return Vec::new() }
    let ubound = sqrt(n);
    let mut small_divisors = match is_even(n) {
        true => (2..).take_while(|&d| d <= ubound)
                     .filter(|d| n % d == 0)
                     .collect::<Vec<_>>(),
        false => (3..).step_by(2)
                      .take_while(|&d| d <= ubound)
                      .filter(|d| n % d == 0)
                      .collect::<Vec<_>>(),
    };
    let mut divisors = Vec::with_capacity(1 + small_divisors.len() * 2);
    divisors.push(1);
    divisors.extend_from_slice(&small_divisors);
    if n == ubound * ubound { small_divisors.pop(); }
    divisors.extend(small_divisors.into_iter().map(|d| n / d));
    divisors
}

fn sqrt(n: usize) -> usize {
    (n as f32).sqrt().floor() as usize
}

fn is_even(n: usize) -> bool {
    if n % 2 == 0 { true } else { false }
}
