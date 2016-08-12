/* We shall say that an n-digit number is pandigital if it makes use of all the
 * digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1
 * through 5 pandigital.
 *
 * The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
 * multiplicand, multiplier, and product is 1 through 9 pandigital.
 *
 * Find the sum of all products whose multiplicand/multiplier/product identity
 * can be written as a 1 through 9 pandigital.
 *
 * NOTE: Some products can be obtained in more than one way so be sure to only
 * include it once in your sum.
 */

#![feature(iter_arith)]

mod panproducts {
    pub struct Panproducts {
        dmax: usize,
        digits: [u32; 9],
        products: Vec<(u32, u32, u32)>,
    }

    impl Panproducts {
        pub fn new() -> Self {
            Panproducts {
                dmax: 9,
                digits: [1, 2, 3, 4, 5, 6, 7, 8, 9],
                products: Vec::new(),
            }
        }

        pub fn gen_products(&mut self) {
            self.gen_products_(0);
        }

        pub fn into_inner(self) -> Vec<(u32, u32, u32)> {
            self.products
        }
    }

    impl Panproducts {
        fn gen_products_(&mut self, mut n: u32) {
            if self.dmax == 0 { self.panproduct_test(n); }
            else {
                n *= 10;
                self.dmax -= 1;
                for i in 0..self.dmax + 1 {
                    let m = n + self.digits[i];
                    self.digits.swap(i, self.dmax);
                    self.gen_products_(m);
                    self.digits.swap(i, self.dmax);
                }
                self.dmax += 1;
            }
        }

        fn panproduct_test(&mut self, n: u32) {
            let (a, b, c) = split_144(n);
            if a * b == c { self.products.push((a, b, c)); }
            let (a, b, c) = split_234(n);
            if a * b == c { self.products.push((a, b, c)); }
        }
    }

    #[inline]
    fn split_144(n: u32) -> (u32, u32, u32) {
        let a = n / 100_000_000;
        let b = (n / 10_000) % 10_000;
        let c = n % 10_000;
        (a, b, c)
    }

    #[inline]
    fn split_234(n: u32) -> (u32, u32, u32) {
        let a = n / 10_000_000;
        let b = (n / 10_000) % 1000;
        let c = n % 10_000;
        (a, b, c)
    }
}

fn main() {
    let products = gen_panproducts();
    let mut results = products.into_iter().map(|(_, _, c)| c).collect::<Vec<_>>();
    results.sort();
    results.dedup();
    println!("{}", results.iter().sum::<u32>());
}

fn gen_panproducts() -> Vec<(u32, u32, u32)> {
    use panproducts::Panproducts;

    let mut panproducts = Panproducts::new();
    panproducts.gen_products();
    panproducts.into_inner()
}
