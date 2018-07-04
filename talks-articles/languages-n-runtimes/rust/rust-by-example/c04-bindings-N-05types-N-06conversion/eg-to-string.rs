use std::string::ToString;

struct Dog {
    count: i32
}

// convert any type to a String, implement ToString trait
impl ToString for Dog {
    fn to_string(&self) -> String {
        format!("total dogs found are {:?}", self.count)
    }
}

// idiomatic way to convert string into a number by parsing
// can be done without type inference or using turbofish syntax
fn parse_to_string(){
    let i32_from_string: i32 = "10".parse().unwrap();
    let i64_from_string = "10".parse::<i64>().unwrap();
    println!("{} should be same as {}", i32_from_string, i64_from_string);
}

fn main() {
    let dog = Dog { count: 10 };
    println!("{}", dog.to_string());
    parse_to_string();
}
