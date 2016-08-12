/* In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
 *
 * 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
 *
 * It is possible to make £2 in the following way:
 *
 * 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
 *
 * How many different ways can £2 be made using any number of coins?
 */

const TOTAL: u16 = 200;
const COINS: [u16; 7] = [1, 2, 5, 10, 20, 50, 100];

static mut N: u32 = 0;

fn main() {
    add_coins(0, 0);
    println!("{}", unsafe {N + 1} );
}

fn add_coins(mut sum: u16, i: usize) {
    loop {
        if sum == TOTAL { unsafe { N += 1; return } }
        else if sum < TOTAL {
            if i + 1 != COINS.len() { add_coins(sum, i + 1); }
        }
        else { return }
        sum += COINS[i];
    }
}
