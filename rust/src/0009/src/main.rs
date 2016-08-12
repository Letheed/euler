/* A Pythagorean triplet is a set of three natural numbers, a < b < c, for which
 * a^2 + b^2 = c^2
 *
 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 * Find the product abc.
 */

fn main() {
    for a in 1..1000 / 2 {
        for b in 1..1000 - a {
            let c = 1000 - (a + b);
            if a * a + b * b == c * c {
                println!("{}", a *  b * c);
                return
            }
        }
    }
}
