fn main() {
    if_else();
    eg_match();
    eg_while();
    eg_for();
    eg_loop();
    eg_label();
    eg_block();
    eg_scope_shadow();
    println!("eg_gcd: {:?}", eg_gcd(10, 15));
    println!("eg_gcd: {:?}", eg_gcd(143, 52));
    println!("Collatz Length for {}: {}", 11, collatz_length(11));
    println!("Collatz Length for {}: {}", 19, collatz_length(19));
}

/// Determine the length of the collatz sequence beginning at `n`.
fn collatz_length(mut n: i32) -> u32 {
    let mut len = 1;
    while n > 1 {
        n = match n % 2 {
            0 => n / 2,
            _ => 3 * n + 1,
        };
        len += 1;
    }
    len
}

fn eg_gcd(a: u32, b: u32) -> u32 {
    if b > 0 {
        return eg_gcd(b, a % b);
    }
    return a;
}

fn eg_scope_shadow() {
    let num = 10;
    {
        let abc = 20;
        let num = 100;
        println!("abc: {} | inner: {}", abc, num);

        let abc = 2;
        let num = 1;
        println!("abc-shadowed: {} | inner-shadowed: {}", abc, num);
    }
    println!("outer: {}", num);
}

fn eg_block() {
    let num = 10;
    let abc = 100;
    let result = {
        let abc = 20;
        num * abc
    };
    let not_needed = {
        let abc = 20;
        println!("in-block abc: {}", abc);
    };
    println!("abc = {} | result = {}", abc, result);
    println!("{:?}", not_needed);
}

fn eg_label() {
    let rand_arr = [[1, 2, 3], [4, 5, 6]];
    'yadayada: for i in 0..rand_arr.len() {
        for j in 0..rand_arr[i].len() {
            if rand_arr[i][j] % 2 != 0 {
                continue;
            }
            println!("{}", rand_arr[i][j]);
            if rand_arr[i][j] == 4 {
                break 'yadayada;
            }
        }
    }
}

fn eg_loop() {
    let mut a = 1;
    loop {
        if a >= 10 {
            break;
        }
        a += 1;
    }
    println!("a now is {}", a);
}

fn eg_for() {
    let mut sum = 0;
    for num in 1..10 {
        sum += num;
    }
    println!("sum is {}", sum);

    sum = 0;
    for elm in [1, 2, 3, 4, 5, 6, 7, 8, 9] {
        sum += elm;
    }
    println!("sum is {}", sum);
}

fn eg_while() {
    let mut a = 1;
    while a < 10 {
        a += 1;
    }
    println!("a now is {}", a);
}

fn eg_match() {
    let b = true;
    let msg = match b {
        true => "yeah right",
        _ => "nah, never",
    };
    println!("* match: {}", msg);
}

fn if_else() {
    let b = true;
    let msg = if b == true {
        "yeah right"
    } else {
        "nah, never"
    };
    println!("{}", msg);
}
