/* Using names, a 46K text file containing over five-thousand first names,
 * begin by sorting it into alphabetical order. Then working out the
 * alphabetical value for each name, multiply this value by its alphabetical
 * position in the list to obtain a name score.
 * 
 * For example, when the list is sorted into alphabetical order, COLIN, which is
 * worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
 * would obtain a score of 938 × 53 = 49714.
 * 
 * What is the total of all the name scores in the file?
 */

#![feature(iter_arith)]

use std::io::Read;

fn main() {
    let input = {
        let mut buffer = String::new();
        std::fs::File::open("../names").unwrap().read_to_string(&mut buffer).unwrap();
        buffer
    };
    let mut names = input.split(',').map(|s| s.trim_matches('"')).collect::<Vec<_>>();
    names.sort();
    let score = names.iter().enumerate().map(|(i, name)| (i + 1) * value(name)).sum::<usize>();
    println!("{}", score);
}

fn value(name: &str) -> usize {
    name.as_bytes().iter()
        .map(|c| (c - ('A' as u8) + 1) as usize)
        .sum::<usize>()
}
