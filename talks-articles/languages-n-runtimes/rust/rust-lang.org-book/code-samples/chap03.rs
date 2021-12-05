fn main() {
    variables();
    data_types();
    func();
    control_flow();

    println!("33.8F => {:.1}C", fahrenheit2celsius(33.8));
    println!("50F => {:.1}C", fahrenheit2celsius(50.0));
    fib(100);
    the_12_days_of_christmas();
}

fn variables() {
    const WORK_DAYS: u32 = 5;

    // shadowing
    let x = 5;
    println!("x: {}", x);
    let x = x + x;
    println!("x: {}", x);
    let x = WORK_DAYS;
    println!("x: {}", x);
}

fn data_types() {
    let max255: u8 = 10;
    println!("max255: {}", max255);
    let max255 = 256;
    println!("max255: {}", max255);

    let tup = (10, 20);
    let (t01, _t02) = tup;
    println!("t01: {}", t01);

    let tup_b: (i32, f64, u8) = (500, 6.4, 1);
    println!("tup_b.0: {}", tup_b.0);

    let tup_c = ();
    println!("tup_c: {}", format!("{:#?}", tup_c));

    let arr = [11, 22];
    println!("arr: {:?}", arr);
    let arra: [i32; 3] = [1, 2, 12];
    println!("arra: {:?}", arra);
    let ar = [20; 10];
    println!("ar: {:?}", ar);

    let mut xs = vec![1i32, 2, 3];
    println!("xs vector: {:?} | {}", xs, xs.len());
    xs.push(10);
    println!("xs vector: {:?} | {}", xs, xs.len());
    println!("xs pop: {:?} | {}", xs.pop(), xs.len());
    println!("xs vector: {:?} | {}", xs, xs.len());
}

fn func() {
    let num: i32 = 10;
    let pow2 = { num * 2 };
    println!("plus_one: {:?}", plus_one(num));
    println!("plus_two: {:?}", plus_two(pow2));
}
fn plus_one(x: i32) -> i32 {
    x + 1
}
fn plus_two(x: i32) -> i32 {
    return x + 2;
}

fn control_flow() {
    let x = if true { 10 } else { 0 };
    println!("x: {}", x);

    if x == 10 {
        println!("it's 10");
    }

    if x > 10 {
        println!("it's greater than 10");
    } else {
        println!("it's less than or equal to 10");
    }

    if x > 10 {
        println!("it's greater than 10");
    } else if x < 10 {
        println!("it's less than to 10");
    } else {
        println!("it's equal to 10");
    }

    let mut x = 10;
    loop {
        x -= 1;
        if x < 1 {
            break;
        }
    }

    x = 10;
    let mut sum = 0;
    let sum_of_1_to_10 = loop {
        sum += x;
        x -= 1;
        if x < 1 {
            break sum;
        }
    };
    println!("sum_of_1_to_10: {}", sum_of_1_to_10);

    // let one2five = 1..5; // avoid as Range don't allow Copy trait
    for elem in 1..5 {
        println!("the value is: {}", elem);
    }
    for elem in (1..5).rev() {
        println!("the value is: {}", elem);
    }
}

// summary problem: Convert temperatures between Fahrenheit and Celsius
fn fahrenheit2celsius(f: f64) -> f64 {
    return (f - 32.0) * 5.0 / 9.0;
}

// summary problem: Generate the nth Fibonacci number
fn fib(upto: i32) {
    if upto < 0 {
        panic!("{} is negative!", upto);
    }
    let mut prev = 0;
    let mut now = 1;

    print!("{} {}", prev, now);
    while now <= upto {
        let num = prev + now;
        print!(" {}", num);
        prev = now;
        now = num;
    }
    println!(" | prev: {}, now: {}", prev, now);
}

// summary problem: Print "The Twelve Days of Christmas" taking advantage of repetition
fn the_12_days_of_christmas() {
    let first_line = "On the twelfth day of Christmas, my true love sent to me";
    let lines = [
        "A partridge in a pear tree",
        "Two turtle doves, and",
        "Three french hens",
        "Four calling birds",
        "Five golden rings",
        "Six geese a-laying",
        "Seven swans a-swimming",
        "Eight maids a-milking",
        "Nine ladies dancing",
        "Ten lords a-leaping",
        "Eleven pipers piping",
        "Twelve drummers drumming",
    ];
    let mut lines_to_print = vec![];
    for line in lines {
        println!("{}", first_line);
        lines_to_print.push(line);
        for to_print in lines_to_print.iter().rev() {
            println!("{}", to_print);
        }
        println!("")
    }
}
