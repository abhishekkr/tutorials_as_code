fn main() {
    let arr = [1; 7];
    println!("{:?}", arr);

    let mut xarr: [i8; 7] = [10; 7];
    xarr[5] = arr.len() as i8;
    println!("{:#?}", xarr);

    let egarr = [1, 2, 3, 4, 5];
    for elem in egarr {
        println!("> {}", elem);
    }

    for idx in 0..egarr.len() {
        println!("=> {}", egarr[idx]);
    }
}
