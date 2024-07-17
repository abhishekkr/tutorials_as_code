// String::from is available because String implements From<&str>

#[derive(Debug)]
struct Double<T>(T);

impl From<u8> for Double<u8> {
    fn from(from: u8) -> Double<u8> {
        Double(from * 2)
    }
}

impl From<f32> for Double<f32> {
    fn from(from: f32) -> Double<f32> {
        Double(from * 2.0)
    }
}

impl From<i32> for Double<i32> {
    fn from(from: i32) -> Double<i32> {
        Double(from * 2)
    }
}

fn main() {
    println!("double of {} is {:?}", 2, Double::from(2));
    println!("double of {} is {:?}", 3.0, Double::from(3.0));
    println!("double of {} is {:?}", -2, Double::from(-2));
}
