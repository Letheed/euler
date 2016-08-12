/* The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1);
 * so the first ten triangle numbers are:
 *
 * 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
 *
 * By converting each letter in a word to a number corresponding to its
 * alphabetical position and adding these values we form a word value. For
 * example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value
 * is a triangle number then we shall call the word a triangle word.
 *
 * Using words.txt, a 16K text file containing nearly two-thousand common
 * English words, how many are triangle words?
 */

#![feature(iter_arith)]

use std::io::Read;

fn main() {
    let tnums = triangular_nums_gen(50);
    let input = {
        let mut buffer = String::new();
        std::fs::File::open("../words").unwrap().read_to_string(&mut buffer).unwrap();
        buffer
    };
    let n = input.split(',')
                 .map(|s| s.trim_matches('"'))
                 .map(|w| word_value(w))
                 .filter(|v| tnums.binary_search(v).is_ok())
                 .count();
    println!("{}", n);
}

fn word_value(w: &str) -> u32 {
    w.as_bytes().iter().map(|&c| (c - 'A' as u8 + 1) as u32).sum()
}

fn triangular_nums_gen(n: usize) -> Vec<u32> {
    let mut tnums = Vec::with_capacity(n);
    let mut tn = 0;
    for n in 0..n as u32 {
        tn += n;
        tnums.push(tn);
    }
    tnums
}
