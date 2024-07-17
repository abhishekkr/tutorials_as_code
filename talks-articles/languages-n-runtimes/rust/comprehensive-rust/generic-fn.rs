fn find_max<T: std::cmp::PartialOrd>(v: Vec<T>) -> T
where
    T: Copy,
{
    let mut result: T = v[0];
    for n in &v {
        if result < *n {
            result = *n;
        }
    }
    result
}

fn main() {
    let v: Vec<f32> = vec![1.1, 2.1, 3.0, 4.5, 1.2, 1.0];
    println!("Vector: {:?}", v);
    println!("Max is {}", find_max(v));

    let v: Vec<u32> = vec![11, 21, 3, 45, 12, 1];
    println!("Vector: {:?}", v);
    println!("Max is {}", find_max::<u32>(v));
}
