/* A palindromic number reads the same both ways.
 * The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
 *
 * Find the largest palindrome made from the product of two 3-digit numbers.
 */

fn main() {
    let mut numbers = Vec::new();
    for i in 100..1000 {
        for j in 100..i + 1 {
            let n = i * j;
            if is_palindromic_ascii(&n.to_string()) {
                numbers.push(n);
            }
        }
    }
    numbers.sort();
    println!("{}", numbers.last().unwrap());
}

fn is_palindromic_ascii(s: &str) -> bool {
    let forward = s.as_bytes().iter().take(s.len() / 2);
    let backward = s.as_bytes().iter().rev().take(s.len() / 2);
    forward.eq(backward)
}
