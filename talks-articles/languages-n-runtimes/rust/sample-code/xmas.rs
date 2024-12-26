use std::{thread, time::Duration};

const SLEEP_FOR: Duration = Duration::from_millis(500);
const TREE_WIDTH: usize = 40;
const CLEAR_CODE: &str = "\x1B[2J";
const COLOR_SYSTEM: &str = "\x1b[0m";
const COLOR_RED: &str = "\x1b[31m";
const COLOR_GREEN: &str = "\x1b[32m";
const COLOR_YELLOW: &str = "\x1b[33m";
const COLOR_BLUE: &str = "\x1b[34m";
const BGCOLOR_RED: &str = "\x1b[41m";
const BGCOLOR_YELLOW: &str = "\x1b[43m";
const BGCOLOR_BLUE: &str = "\x1b[44m";
const COLOR_YELLOWB: &str = "\x1b[1m\x1b[33m";

fn main() {
    print!("{CLEAR_CODE}");
    let mut color_flag: bool = true;
    loop {
        if color_flag {
            print_tree(BGCOLOR_RED, COLOR_YELLOW, COLOR_BLUE);
        } else {
            print_tree(COLOR_RED, BGCOLOR_YELLOW, BGCOLOR_BLUE);
        }
        thread::sleep(SLEEP_FOR);
        print!("{CLEAR_CODE}");
        color_flag = !color_flag;
    }
}

fn print_tree(star_color: &str, trail_color: &str, trunk_color: &str) {
    let mut msg_spacing: usize = TREE_WIDTH;
    if msg_spacing > 10 {
        msg_spacing /= 2;
    }
    let msg = format!("{COLOR_GREEN}Merry {COLOR_SYSTEM}{COLOR_RED}Christmas {COLOR_SYSTEM}{COLOR_YELLOW}and {COLOR_SYSTEM}{COLOR_BLUE}Happy {COLOR_SYSTEM}{COLOR_YELLOWB}Holidays{COLOR_SYSTEM}\n");
    println!("{}{}", " ".repeat(msg_spacing), msg);

    let xstar = format!("{star_color}*{COLOR_SYSTEM}");
    println!("{}{COLOR_RED}X.{COLOR_SYSTEM}", " ".repeat(TREE_WIDTH));
    for n in (1..TREE_WIDTH).rev() {
        let fslash = format!("{COLOR_GREEN}{}{COLOR_SYSTEM}", "/".repeat(TREE_WIDTH - n));
        let bslash = format!("{COLOR_GREEN}{}{COLOR_SYSTEM}", "\\".repeat(TREE_WIDTH - n));
        let spacing = " ".repeat(n);
        println!("{}{xstar}{}{}{xstar}", spacing, fslash, bslash);
    }

    let trails = format!("{trail_color}{}{COLOR_SYSTEM}", ";".repeat(TREE_WIDTH * 2));
    println!("{xstar}{}{xstar}", trails);

    let trunk = format!("{trunk_color}||{COLOR_SYSTEM}");
    for _n in 1..(TREE_WIDTH / 10) {
        println!("{}{}", " ".repeat(TREE_WIDTH), trunk);
    }
}
