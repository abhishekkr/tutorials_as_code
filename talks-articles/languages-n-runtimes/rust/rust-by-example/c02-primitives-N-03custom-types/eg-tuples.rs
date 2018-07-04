// tuples as arguments
fn rev(triplet: (i32, i32, bool)) -> (bool, i32, i32) {
    let (x, y, z) = triplet;
    (z, y, x)
}

// accessing values from tuples
fn dotpicker(){
    let xyz = (100i32, 250i32, false);
    let abc_xyz = ((10u32, true), (32u32, false));

    println!("x: {}, y: {}, z: {}", xyz.0, xyz.1, xyz.2);
    println!("abc_y: {}", (abc_xyz.0).1);
    println!("{:?}", rev(xyz)); // tuples are printable using :?
}

fn main(){
    dotpicker();
}
