use std::convert::From;

#[derive(Debug)]
struct Number {
    lineup: i64,
}

impl From<i64> for Number {
    fn from(element: i64) -> Self {
        Number { lineup: element }
    }
}

fn main() {
    // str into String comes free
    let x_str = "hello";
    let x_string = String::from(x_str);
    println!("converted string {:?}", x_string);

    let m = 100;
    let n = Number::from(m);
    println!("converted n {:?}", n);

    let o: Number = m.into();
    println!("converted o {:?}", o);
}
