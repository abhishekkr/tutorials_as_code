use std::mem;

fn what_slice(s: &[i32]) {
    println!("first element of slice: {}, total elements are {}", s[0], s.len());
}

fn main() {
    let arr: [i32; 3] = [10, 15, 25]; // fixed size array
    let arq: [i32; 5] = [90; 5]; // all elements initialized by same value

    println!("elements in arr: {:?}", arr);
    println!("elements in arq: {:?}", arq);

    println!("{} sized array occupied {} bytes", arq.len(), mem::size_of_val(&arq));

    what_slice(&arr); // whole array as slice
    what_slice(&arq[1..3]); // section of array as slice
    // println!("{}",arq[10]); // will panic
}
