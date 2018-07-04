#![allow(dead_code)]

// with implicit discriminator
enum Fruits {
    Apple,
    Banana,
    Chiku,
}

// with explicit discriminator
enum Colors {
    White = 0xffffff,
    Black = 0x000000,
    Maroon = 0x800000,
}

fn main() {
    println!("apple is {}, but Banana is {}", Fruits::Apple as i32, Fruits::Banana as i32);
    println!("white is {}, but maroon is {}", Colors::White as i32, Colors::Maroon as i32);
}
