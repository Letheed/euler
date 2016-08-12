/* Find the value of d < 1000 for which 1/d contains the longest recurring cycle
 * in its decimal fraction part.
 */

fn main() {
    let (mut best_d, mut max_len) = (0, 0);
    for d in (2..1000).rev() {
        if d <= max_len { break }
        let dp = one_over_d_decimal_part(d);
        if let Some(cycle_len) = recurring_cycle_length(&dp, d) {
            if cycle_len > max_len {
                max_len = cycle_len;
                best_d = d;
            }
        }
    }
    println!("{}", best_d);
}

fn one_over_d_decimal_part(d: usize) -> Vec<u8> {
    debug_assert!(d > 1);
    let mut dp = Vec::with_capacity(3 * d as usize);
    let mut n = 1;
    for _ in 0..3*d {
        n *= 10;
        dp.push((n / d) as u8);
        n %= d;
        if n == 0 { break }
    }
    dp
}

fn recurring_cycle_length(dp: &[u8], d: usize) -> Option<usize> {
    debug_assert!(d > 1);
    for i in 0..dp.len().saturating_sub(d) {
        for j in i+1..dp.len().saturating_sub(d - 1) {
            if dp[i..].iter().zip(dp[j..].iter()).all(|(c1, c2)| c1 == c2) {
                return Some(j - i)
            }
        }
    }
    None
}
