use std::collections::HashSet;

/* Given the sum {}, return all possible Pythagorean triplets, which
produce the said sum, or an empty HashSet if there are no such
triplets. Note that you are expected to return triplets in [a, b, c]
order, where a < b < c.

When m and n are any two positive integers (m < n):
    a = n2 - m2
    b = 2nm
    c = n2 + m2
Then a, b, and c form a Pythagorean Triple.
*/
pub fn find(sum: u32) -> HashSet<[u32; 3]> {
    let target = sum;

    let mut result = HashSet::new();

    for r in 2..(target / 4) {
        if r * r % 2 != 0 {
            continue;
        }
        let factor_me = r * r / 2;
        let factors = half_divisors(factor_me);

        for s in factors {
            let t = factor_me / s;

            let a = r + s;
            let b = r + t;
            let c = r + s + t;
            if a + b + c == target {
                let mut abc = [a, b, c];
                abc.sort();
                result.insert(abc);
            }
        }
    }

    result
}

// Let's only look at "half" of the divisors, since we will be
// computing pairs anyway
fn half_divisors(n: u32) -> Vec<u32> {
    let upper_limit = (n as f64).sqrt() as u32 + 1;
    let mut divisors: Vec<u32> = Vec::new();
    for i in 1..upper_limit {
        if n % i == 0 {
            divisors.push(i);
        }
    }
    divisors
}
