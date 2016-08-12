/* Let d(n) be defined as the sum of proper divisors of n (numbers less than n
 * which divide evenly into n). If d(a) = b and d(b) = a, where a ≠ b, then a
 * and b are an amicable pair and each of a and b are called amicable numbers.
 *
 * For example, the proper divisors of 220 are: 1, 2, 4, 5, 10, 11, 20, 22, 44,
 * 55 and 110. Therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4,
 * 71 and 142. So d(284) = 220.
 *
 * Evaluate the sum of all the amicable numbers under 10000.
 */

#![feature(step_by, iter_arith)]
    
fn main() {
    let sums = (0..10000).map(|n| divisors(n).into_iter().sum::<usize>())
                         .collect::<Vec<_>>();
    let mut amicables = Vec::new();
    for (n, &sum) in sums.iter().enumerate() {
        if sum < sums.len() && n != sum && sums[sum] == n { amicables.push(n); }
    }
    println!("{}", amicables.into_iter().sum::<usize>());
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
