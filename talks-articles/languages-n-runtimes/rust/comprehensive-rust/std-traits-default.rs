#[derive(Debug)]
struct That(String);

impl Default for That {
    fn default() -> Self {
        Self("".into())
    }
}

#[derive(Debug, Default)]
struct This {
    n: u32,
    s: That,
}

fn main() {
    let this = This::default();
    println!("{this:#?}");

    let some_of_this = This {
        s: That("This with That!".into()),
        ..This::default()
    };
    println!("{some_of_this:#?}");

    let none_of_this: Option<This> = None;
    println!("{:#?}", none_of_this.unwrap_or_default());
}
