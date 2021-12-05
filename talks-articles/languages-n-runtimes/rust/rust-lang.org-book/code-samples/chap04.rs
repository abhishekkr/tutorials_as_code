fn main() {
    str_moves();
    str_clones();
    stack_only_copy();
    move_copy_trait_on_fn();
    move_copy_trait_on_return();
    take_string_by_ref();
    take_string_by_ref_and_change();
    mutable_references();
    mixed_references();
    first_token_fn();
}

fn str_moves() {
    let s = "this";
    println!("s: {:?}", s);
    let t = s;
    if "this" == s {
        println!("s: {:?}", s);
    }
    println!("t: {:?}", t);

    let s: String = "this".to_string();
    println!("s: {:?}", s);
    let t = s;
    let _val = String::from("this");
    /*
    // with 'if let' this body will be skipped if the pattern is refuted
    if _val == s {              // would fail for 's' as borrowed after move
        println!("s: {:?}", s);
    }
    */
    println!("t: {:?}", t);
}

fn str_clones() {
    let sw = String::from("~str clone");
    let sx = sw.clone();
    println!("sw: {}, sx: {}", sw, sx);
}

fn stack_only_copy() {
    let sw = 10;
    let sx = sw;
    println!("sw: {}, sx: {}", sw, sx);
}

fn move_copy_trait_on_fn() {
    let s = String::from("this will go out of scope when passed as arg to fn");
    let this_will_be_copied_when_passed_so_remains_in_scope = 10;
    print_string(s);
    print_number(this_will_be_copied_when_passed_so_remains_in_scope);
    // print_string(s); // thus this will panic
    print_number(this_will_be_copied_when_passed_so_remains_in_scope); // this will work
}
fn print_string(s: String) {
    println!("{}", s);
}
fn print_number(n: i32) {
    println!("{}", n);
}

fn move_copy_trait_on_return() {
    let s = this_return_string();
    let n = this_return_number();

    let sx = &s;
    println!(
        "address of string is same after return: {:?}",
        sx as *const String
    );
    let nx = &n;
    println!(
        "address of int is different after return: {:?}",
        nx as *const i32
    );

    println!("{:?} | {:?}", s, n);

    let s2 = take_string_process_and_return(s);
    println!("this works with pass through of string: {}", s2);
}
fn this_return_string() -> String {
    let sw = String::from("some things are written");
    let sx = &sw;
    println!("address of string at called: {:?}", sx as *const String);
    return sw;
}
fn this_return_number() -> i32 {
    let nw = 10;
    let nx = &nw;
    println!("address of int at called: {:?}", nx as *const i32);
    return nw;
}
fn take_string_process_and_return(sw: String) -> String {
    println!("decorated print {}", sw);
    return sw;
}

fn take_string_by_ref() {
    let sw = String::from("hello");
    let len = calculate_length(&sw);
    println!("The length of '{}' is {}.", sw, len);
}
fn calculate_length(s: &String) -> usize {
    s.len()
}

fn take_string_by_ref_and_change() {
    let sw = String::from("hello");
    change_string_fail(&sw);
}
fn change_string_fail(s: &String) {
    // s.push_str(" you");
    println!("changing '{}' not allowed, references are immutable", s);
}

fn mutable_references() {
    let mut sw = String::from("hello");
    change_string(&mut sw);
}
fn change_string(s: &mut String) {
    s.push_str(" you");
    println!("changing mutable ref: '{}'", s);

    {
        let sx = &mut *s;
        sx.push_str(" welcome");
        println!("changing mutable ref: '{}'", sx);
    }
    {
        let sw = &mut *s;
        sw.push_str(" sayonara");
        println!("changing mutable ref: '{}'", sw);
    }
}

fn mixed_references() {
    let mut s = String::from("mixed");

    let sw = &s;
    let sx = &s;
    println!("{} == {}", sw, sx);

    let sy = &mut s;
    sy.push_str(" again");
    println!("{}", sy);
}

fn first_token_fn() {
    let txt = String::from("this is a text");
    let first_size = first_token(&txt);
    println!("{}", first_size);
    // if 'txt' was declared to be mutable; `clear()` could be called upon it
    // txt.clear(); // empties the string; 'first_size' isn't connected with 'txt'

    let txt = String::from("lorem ipsum or just blah");
    let b_to_e = string_slice(&txt, 2, 7);
    println!("{}", b_to_e);

    let sw = &txt[0..5];
    let sx = &txt[16..];
    let sy = &txt[..11];
    let sz = &txt[..];
    println!("{} | {} | {} | {}", sw, sx, sy, sz);
    // txt = String::from("say whatever you want"); // fail as gets borrow before and txt was mutable

    let fw = first_word_slice(&txt);
    println!("{}", fw);
}
fn first_token(s: &String) -> usize {
    let txt = s.as_bytes();
    for (idx, &item) in txt.iter().enumerate() {
        if item == b' ' {
            return idx;
        }
    }
    s.len()
}
fn string_slice(s: &String, b: usize, e: usize) -> String {
    s[b..e].to_string()
}
fn first_word_slice(s: &String) -> &str {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[0..i];
        }
    }

    &s[..]
}
