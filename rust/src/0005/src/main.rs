/* 2520 is the smallest number that can be divided by each of the numbers from 1
 * to 10 without any remainder.
 *
 * What is the smallest positive number that is evenly divisible by all of the
 * numbers from 1 to 20?
 */

const DIVISORS: [u32; 8] = [3, 7, 9, 11, 13, 16, 17, 19];

fn main() {
    let smallest_multiple = (1..).filter_map(|x| {
        let n = x * 20;
        if DIVISORS.iter().all(|d| n % d == 0) { Some(n) }
        else { None }
    }).next().unwrap();
    println!("{}", smallest_multiple);
}
