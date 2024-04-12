fn main() {
    let mut a: char = 'A';
    let b = 'B';

    let mut r: &char = &a;
    println!("r: {:?}", *r); //same

    r = &b;
    println!("r: {:?}", r); //same

    let exr = &mut a;
    *exr = 'C';
    println!("exr: {:?}", exr);
    println!("a: {:?}", a);
}
