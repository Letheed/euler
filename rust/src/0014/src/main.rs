/* The following iterative sequence is defined for the set of positive integers:
 *
 * n → n/2 (n is even)
 * n → 3n + 1 (n is odd)
 *
 * Using the rule above and starting with 13, we generate the following sequence:
 *
 * 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
 *
 * It can be seen that this sequence (starting at 13 and finishing at 1) contains
 * 10 terms. Although it has not been proved yet (Collatz Problem), it is thought
 * that all starting numbers finish at 1.
 *
 * Which starting number, under one million, produces the longest chain?
 *
 * NOTE: Once the chain starts the terms are allowed to go above one million.
 */


fn main() {
    let mut length_from: Vec<usize> = vec![0; 1_000_000];
    length_from[0] = 1; // N/A
    length_from[1] = 1;
    for n in 2..length_from.len() {
        compute_length(n, &mut length_from);
    }
    let (mut max_seed, mut max_length) = (1, 1);
    for seed in 2..length_from.len() {
        if length_from[seed] > max_length {
            max_seed = seed;
            max_length = length_from[seed];
        }
    }
    println!("{}", max_seed);
}

fn compute_length(n: usize, length_from: &mut[usize]) {
    if length_from[n] != 0 { return }
    let mut next;
    let mut steps = 1;
    if n % 2 == 0 { next = n / 2; }
    else {
        next = 3 * n + 1;
        while next >= 1_000_000 {
            if next % 2 == 0 { next /= 2; }
            else { next = 3 * next + 1; }
            steps += 1;
        }
    }
    if length_from[next] == 0 { compute_length(next, length_from); }
    length_from[n] = length_from[next] + steps;
}
