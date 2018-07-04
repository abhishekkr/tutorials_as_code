static SOME: i32 = 128;

fn change_static<'a>(_: &'a i32) -> &'a i32 {
    &SOME
}

fn main() {
    {
        let static_str = "this is read only";
        println!("ro: {}", static_str);
    }
    {
        let num_to_change = 10;
        let new_num = change_static(&num_to_change);
        println!("new_num: {}", new_num);
    }
    println!("SOME: {}", SOME);
}

