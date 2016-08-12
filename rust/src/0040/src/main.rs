/* An irrational decimal fraction is created by concatenating the positive
 * integers:
 *
 * 0.123456789101112131415161718192021...
 *
 * It can be seen that the 12th digit of the fractional part is 1.
 * If dn represents the nth digit of the fractional part, find the value of the
 * following expression.
 *
 * d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
 */

#![feature(iter_arith, time2)]

struct FracRange {
    digits: u32,
    start: u32,
    end: u32,
    len: u32,
}

impl FracRange {
    fn new() -> Self {
        FracRange {
            digits: 1,
            start: 1,
            end: 10,
            len: 9,
        }
    }

    fn next(&mut self) {
        self.start = self.end;
        self.digits += 1;
        self.len *= 10;
        self.end = self.start + self.digits * self.len;
    }
}

fn main() {
    let tstart = std::time::Instant::now();
    let mut fracrange = FracRange::new();
    let product = (0..7).map(|order| {
        let digit_index = std::iter::repeat(10).take(order).product::<u32>();
        while digit_index >= fracrange.end { fracrange.next(); }
        let local_digit_index = digit_index - fracrange.start;
        let n_start = std::iter::repeat(10).take(fracrange.digits as usize - 1).product::<u32>();
        let n = n_start + (local_digit_index / fracrange.digits);
        nth_digit(local_digit_index % fracrange.digits, n, fracrange.digits)
    }).product::<u32>();
    let elapsed = tstart.elapsed().subsec_nanos();
    println!("{}, in {} ns", product, elapsed);
}

fn nth_digit(index: u32, mut n: u32, mut digits: u32) -> u32 {
    digits -= 1;
    while digits != index {
        n /= 10;
        digits -= 1;
    }
    n % 10
}
