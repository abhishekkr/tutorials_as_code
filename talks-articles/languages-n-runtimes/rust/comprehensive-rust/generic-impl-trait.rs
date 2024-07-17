fn add_one<T: Into<i32>>(x: T) -> i32 {
    x.into() + 1
}

// impl Trait becomes syntactic sugar for above
// must have From<{T}> implemented for i32, for T to be passed
fn add_ten(x: impl Into<i32>) -> i32 {
    x.into() + 10
}

// when don't wanna expose concrete type in Public API
fn off_by_one_zero_or_more(x: u32) -> impl std::fmt::Debug {
    if x < 1 {
        return (0, 0);
    }
    (x - 1, x + 1)
}

fn main() {
    let x: String = "hello".into();
    // whatever gets 'from' defined; gets an into too
    let y = String::from("hello");
    if x == y {
        println!("{x:?} == {y:?}");
    }

    println!("add_one.100 => {:?}", add_one(100));
    println!("add_ten.100 => {:?}", add_ten(100));
    println!("add_ten.true => {:?}", add_ten(true));

    println!(
        "off_by_one_zero_or_more(10) => {:?}",
        off_by_one_zero_or_more(10)
    );
    println!(
        "off_by_one_zero_or_more(0) => {:?}",
        off_by_one_zero_or_more(0)
    );
}
