/* If the numbers 1 to 5 are written out in words: one, two, three, four, five,
 * then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
 *
 * If all the numbers from 1 to 1000 (one thousand) inclusive were written out
 * in words, how many letters would be used?
 *
 * NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
 * forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
 * letters. The use of "and" when writing out numbers is in compliance with
 * British usage.
 */

#![feature(iter_arith)]

extern crate numeral;

use numeral::Numeral;


fn main() {
    let n = (1..1001).map(|n| letter_count(&n.ordinal())).sum::<usize>();
    println!("{}", n);
}

fn letter_count(numeral: &str) -> usize {
    let mut count = numeral.chars().filter(|c| c.is_alphabetic()).count();
    if numeral.contains("hundred") && !numeral.ends_with("hundred") { count += 3; }
    count
}
