use std::cmp::Ordering;

// TODO: implement the `min` function used in `main`.
fn min<T: Ord>(x: T, y: T) -> T {
    match x.cmp(&y) {
        Ordering::Less | Ordering::Equal => x,
        Ordering::Greater => y,
    }
}

fn main() {
    assert_eq!(min(0, 10), 0);
    assert_eq!(min(500, 123), 123);

    assert_eq!(min('a', 'z'), 'a');
    assert_eq!(min('7', '1'), '1');

    assert_eq!(min("hello", "goodbye"), "goodbye");
    assert_eq!(min("bat", "armadillo"), "armadillo");

    println!("{:?}", min(1, 10));
    println!("{:?}", min(20, 10));
}
