use std::collections::HashSet;

/* Given the sum {}, return all possible Pythagorean triplets, which
produce the said sum, or an empty HashSet if there are no such
triplets. Note that you are expected to return triplets in [a, b, c]
order, where a < b < c */
pub fn find(sum: u32) -> HashSet<[u32; 3]> {
    let max = sum / 2;
    let target = sum;

    let mut result = HashSet::new();

    for x in 1..max {
        for y in (x + 1)..max {
            let sum = x * x + y * y;
            if !is_square(sum) {
                continue;
            }
            let z = f64::from(sum).sqrt() as u32;

            if x + y + z < target {
                continue;
            }
            if x + y + z > target {
                break;
            }

            result.insert([x, y, z]);
        }
    }

    result
}

fn is_square(n: u32) -> bool {
    f64::from(n).sqrt().fract() == 0.0
}
