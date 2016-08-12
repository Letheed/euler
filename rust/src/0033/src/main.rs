/* The fraction 49/98 is a curious fraction, as an inexperienced mathematician
 *in attempting to simplify it may incorrectly believe that 49/98 = 4/8,
 * which is correct, is obtained by cancelling the 9s.
 *
 * We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
 *
 * There are exactly four non-trivial examples of this type of fraction, less
 * than one in value, and containing two digits in the numerator and denominator.
 *
 * If the product of these four fractions is given in its lowest common terms,
 * find the value of the denominator.
 */

extern crate num;

use num::rational::Ratio;

fn main() {
    let fractions = digit_cancelling_fractions();
    println!("curious fractions: {:?}", fractions.0);
    println!("lucky reductions: {:?}", fractions.1);
    let num = fractions.1.iter().fold(1, |acc, n| acc * n.0);
    let denom = fractions.1.iter().fold(1, |acc, n| acc * n.1);
    println!("product denom = {}", Ratio::new(num, denom).denom());
}

fn digit_cancelling_fractions() -> (Vec<(u32, u32)>, Vec<(u32, u32)>) {
    let mut fractions1 = Vec::new();
    let mut fractions2 = Vec::new();
    for a in (11..99).filter(|&a| a % 10 >= a / 10) {
        let d1 = a % 10;
        for d0 in (1..9).filter(|&d0| d0 != a / 10) {
            let b = 10 * d1 + d0;
            let fraction1 = (a as f64) / (b as f64);
            let fraction2 = ((a / 10) as f64) / (d0 as f64);
            if fraction1 == fraction2 {
                fractions1.push((a, b));
                fractions2.push((a/10, d0));
            }
        }
    }
    (fractions1, fractions2)
}
