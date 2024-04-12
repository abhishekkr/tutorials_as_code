fn main() {
    let name: &'static str = "John Doe";
    println!("Name: {}", name);

    let mut myname = String::new();
    for chr in name.chars() {
        myname.push(chr);
    }
    myname.push('.');
    myname.push_str("...");
    println!("Name: {}", myname);

    let mut urname = String::from("John");
    println!("Name: {}", urname);
    urname.push_str("Doe !!");
    println!("Name: {}", urname);

    let mut chars: Vec<char> = name.chars().collect();
    println!("{:?}", chars);
    chars.sort();
    println!("{:?}", chars);
    chars.dedup();
    println!("{:?}", chars);

    println!("Name: {}", urname.replace("John", "Jack"));
}
