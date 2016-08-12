/* Find the maximum total from top to bottom in the triangle.
 */

use std::io::BufRead;

fn main() {
    let file = std::fs::File::open("../triangle").unwrap();
    let tree = std::io::BufReader::new(file).lines()
                                            .map(|l| {
                                                l.unwrap()
                                                 .split_whitespace()
                                                 .map(|s| s.parse::<u32>().unwrap())
                                                 .collect::<Vec<_>>()
                                            }).collect::<Vec<_>>();
    let max_sum = tree.iter().rev().skip(1).fold(tree[tree.len() - 1].clone(), |sum, row| {
        (0..row.len()).map(|j| row[j] + std::cmp::max(sum[j], sum[j+1])).collect::<Vec<_>>()
    })[0];
    println!("{}", max_sum);
}
