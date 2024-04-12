fn magnitude(vec: &[f64; 3]) -> f64 {
    let mut sq_sum = 0.0;
    for coords in vec {
        sq_sum += coords * coords;
    }
    sq_sum.sqrt()
}

fn normalize(vec: &mut [f64; 3]) {
    let mag = magnitude(vec);
    for v in vec {
        *v /= mag;
    }
}

fn main() {
    println!(
        "Magnitude of a unit vector: {}",
        magnitude(&[0.0, 1.0, 0.0])
    );

    let mut v = [1.0, 2.0, 9.0];
    println!("Magnitude of {v:?}: {}", magnitude(&v));
    normalize(&mut v);
    println!("Magnitude of {v:?} after normalization: {}", magnitude(&v));
}
