use std::str;

fn main() {
    let lorem: &'static str = "lorem ipsum dolor sit amet";
    println!("{} in reverse ", lorem);
    for w in lorem.split_whitespace().rev() {
        print!("{} ", w);
    }
    println!(); // for new line at end

    let mut ipsum: Vec<char> = lorem.chars().collect();
    ipsum.sort();
    ipsum.dedup();

    let mut dolor = String::new(); // create an empty and growable String
    for i in ipsum {
        dolor.push(i); // append char to string
        dolor.push_str(", "); // append string to string
    }

    println!("dolor ~ {}", dolor);

    let to_trim: &[char] = &[' ', ',']; // trimmed string is slice to
    let trimmed: &str = dolor.trim_matches(to_trim); // original string
    println!("used: {}", trimmed);

    let abc = String::from("fox eats cats"); // heap allocate a string
    let def: String = abc.replace("fox", "dog"); // allocate new mem and store modified

    println!("{} and {}", abc, def);

    // escaping to write bytes with their hexadecimal and unicode values
    let some_escapees = "hex-ed \x23 \x4f \x77";
    println!("23,4f,77 ~ {}", some_escapees);
    let uni_escapees = "uni-fied \u{100D}";
    println!("100D ~ {}", uni_escapees);

    // just simple text
    let txt: &[u8; 25] = b"a simple text \x90 no\\u{90D}";
    println!("txt ~ {:?} ~ allowing hex not unicode", txt);
    let raw_txt: &[u8; 25] = br"simple text \x90 \u{110D}";
    println!("raw_txt ~ {:?} ~ allowing hex and unicode", raw_txt);

    // converting byte-array to string, might fail
    if let Ok(s) = str::from_utf8(txt) {
        println!("same as txt {}", s);
    } else {
        println!("no txt");
    }
    if let Ok(s) = str::from_utf8(raw_txt) {
        println!("same as raw_txt {}", s);
    } else {
        println!("no raw_txt");
    }
}
