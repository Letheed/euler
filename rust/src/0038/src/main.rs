/* Take the number 192 and multiply it by each of 1, 2, and 3:
 *
 *   192 × 1 = 192
 *   192 × 2 = 384
 *   192 × 3 = 576
 *
 * By concatenating each product we get the 1 to 9 pandigital, 192384576. We
 * will call 192384576 the concatenated product of 192 and (1,2,3)
 *
 * The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4,
 * and 5, giving the pandigital, 918273645, which is the concatenated product of
 * 9 and (1,2,3,4,5).
 *
 * What is the largest 1 to 9 pandigital 9-digit number that can be formed as
 * the concatenated product of an integer with (1,2, ... , n) where n > 1?
 */

#![feature(iter_arith)]

fn main() {
    println!("{:?}", (9234..9488).filter_map(|n| pan_multiple(n)).max());
}

fn pan_multiple(n: u32) -> Option<u32> {
    macro_rules! check { ($result: expr) => (if $result.is_err() { return None }) }

    if let Some(mul_ub) = multiplicator_ub(n) {
        let mut digits = [false; 10];
        check!(mark_digits(n, &mut digits));
        for mul in 2..mul_ub {
            check!(mark_digits(n * mul, &mut digits));
        }
        Some(concat_multiples(n, mul_ub))
    }
    else { None }
}

fn concat_multiples(n: u32, mul_ub: u32) -> u32 {
    let mut concat = n;
    for mul in 2..mul_ub {
        let m = n * mul;
        let order_mul: u32 = std::iter::repeat(10).take(order(m) as usize).product();
        concat = concat * order_mul + m;
    }
    concat
}

/// Marks the digits in `n` as `true` in the `digits` sieve.
/// Returns an `Err` if that digit was already marked or is `0`.
fn mark_digits(mut n: u32, digits: &mut[bool; 10]) -> Result<(),()> {
    while n != 0 {
        let d = n % 10;
        if d == 0 || digits[d as usize] { return Err(()) }
        digits[d as usize] = true;
        n /= 10;
    }
    Ok(())
}

/// Returns the upper bound of the multiplicators to produce a 9-digit number
/// by concatenation, or `None` if the concatenation is not 9-digit.
fn multiplicator_ub(n: u32) -> Option<u32> {
    let mut mul = 1;
    let mut order_tot = order(n);
    let order_max = 9 - order_tot;
    while order_tot <= order_max  {
        mul += 1;
        order_tot += order(mul * n);
    }
    if order_tot == 9 { Some(mul + 1) }
    else { None }
}

/// Returns the order of magnitude.
fn order(mut n: u32) -> u32 {
    n /= 10;
    let mut order = 1;
    while n != 0 {
        n /= 10;
        order += 1;
    }
    order
}
