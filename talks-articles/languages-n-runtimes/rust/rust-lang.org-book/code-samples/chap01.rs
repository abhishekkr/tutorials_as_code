use std::env;

fn main() {
    let user;
    match env::var("USER") {
        Ok(val) => user = val,
        Err(_e) => user = String::from("World"),
    }

    println!("Hello {}", capitalize_first_letter(user));
}

fn capitalize_first_letter(s: String) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
    }
}
