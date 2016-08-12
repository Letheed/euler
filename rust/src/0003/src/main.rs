/* The prime factors of 13195 are 5, 7, 13 and 29.
 *
 * What is the largest prime factor of the number 600851475143 ?
 */

const N: u64 = 600851475143;

fn sqrt(n: u64) -> u64 {
    (n as f64).sqrt().floor() as u64
}

fn main() {
    let (mut n, mut ubound) = (N, sqrt(N));
    let mut d = 3;
    while d <= ubound {
        while n % d == 0 {
            n /= d;
            ubound = sqrt(n);
        }
        d += 2;
    }
    println!("{}", n);
}
