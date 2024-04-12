fn main() {
    let a: [i32; 6] = [10, 20, 30, 40, 50, 60];
    let sa: &[i32] = &a[2..5];
    let sb: &[i32] = &a[5..];
    let sc: &[i32] = &a[..3];
    let sd: &[i32] = &a[..];
    println!("{sa:?}");
    println!("{sb:?}");
    println!("{sc:?}");
    println!("{sd:?}");
}
