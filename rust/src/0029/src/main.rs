/* How many distinct terms are in the sequence generated by ab
 * for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?
 */

extern crate num;

use num::bigint::BigUint;

fn main() {
    let mut seq = Vec::new();
    for a in 2..101u32 {
        for b in 2..101usize {
            seq.push(num::pow(BigUint::from(a), b));
        }
    }
    seq.sort();
    seq.dedup();
    println!("{}", seq.iter().count());
}
