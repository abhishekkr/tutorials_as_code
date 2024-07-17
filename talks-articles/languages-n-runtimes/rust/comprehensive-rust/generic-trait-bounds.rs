fn duplicate<T: Clone>(a: T) -> (T, T) {
    (a.clone(), a.clone())
}

fn logit<T>(a: T)
where
    T: std::fmt::Debug,
    T: std::fmt::Display,
{
    println!("[dbg] {a:?}");
    println!("[dsp] {a}");
}

/*
// #[derive(Clone, Debug)]
struct NotClonable;
*/

fn main() {
    let foo = String::from("foo");
    let pair = duplicate(foo);
    println!("pair => {pair:?}");

    /*
    let boo = NotClonable {};
    let bair = duplicate(boo);
    println!("bair => {bair:?}");
     */

    let five = 5;
    logit(five);
}
