use rand::Rng;
use std::cmp::Ordering;
use std::io;

fn main() {
    println!("Guess the number!");

    let secret_number = rand::thread_rng().gen_range(1..101);
    // println!("The secret number is: {}", secret_number);

    loop {
        let guess: u32 = get_input();
        if guess == 0 {
            println!("Guess a number please.");
            continue;
        }

        match guess.cmp(&secret_number) {
            Ordering::Less => println!("too small"),
            Ordering::Greater => println!("too big"),
            Ordering::Equal => {
                println!("voila, you win");
                break;
            }
        }
    }
}

fn get_input() -> u32 {
    println!("Please input your guess.");
    let mut guess = String::new();
    io::stdin()
        .read_line(&mut guess)
        .expect("Failed to read line");

    // let guess: u32 = guess.trim().parse().expect("You need to guess a number.");
    // println!("You guessed: {}", guess);
    let guess: u32 = match guess.trim().parse() {
        Ok(num) => num,
        Err(_) => 0,
    };
    return guess;
}
