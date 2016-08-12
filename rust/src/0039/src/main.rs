/* If p is the perimeter of a right angle triangle with integral length sides,
 * {a,b,c}, there are exactly three solutions for p = 120.
 *
 * {20,48,52}, {24,45,51}, {30,40,50}
 *
 * For which value of p ≤ 1000, is the number of solutions maximised?
 */

#![feature(step_by)]

fn main() {
    let mut p_max_sol = 0;
    let mut max_sol = 0;
    for p in (12..1001).step_by(2) {
        let pf = p as f32;
        let bmax = ((p * p / 2) - 1) / (p - 1) as u32;
        let nsol = (1..bmax + 1).filter_map(|b| a(b as f32, pf)).count();
        if nsol > max_sol {
            max_sol = nsol;
            p_max_sol = p;
        }
    }
    println!("{} has {} solutions", p_max_sol, max_sol);
}

fn a(b: f32, p: f32) -> Option<u32> {
    let a = p * ((p / 2.0) - b) / (p - b);
    if a.fract() == 0.0 { Some(a as u32) }
    else { None }
}
