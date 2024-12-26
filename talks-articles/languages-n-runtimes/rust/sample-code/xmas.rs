use std::{thread, time::Duration};

const SLEEP_FOR: Duration = Duration::from_millis(500);
const TREE_WIDTH: usize = 35;
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
    print!("\x1B[2J");
    let mut color_flag: bool = true;
    loop {
        if color_flag {
            print_xmas_tree(COLOR_YELLOW, BGCOLOR_BLUE);
        } else {
            print_xmas_tree(COLOR_YELLOWB, COLOR_BLUE);
        }
        thread::sleep(SLEEP_FOR);
        print!("\x1B[2J");
        color_flag = !color_flag;
    }
}

fn print_xmas_tree(light_color: &str, trail_color: &str) {
    let msg = format!("M{COLOR_GREEN}erry{COLOR_SYSTEM}C{COLOR_RED}hri{COLOR_SYSTEM}{COLOR_YELLOW}st{COLOR_SYSTEM}{trail_color}mas{COLOR_SYSTEM}.");

    // print the greeting
    let mut msg_spacing: usize = TREE_WIDTH;
    if TREE_WIDTH > 10 {
        msg_spacing -= 7;
    }
    println!("{}{}{}{}", "\n", " ".repeat(msg_spacing), msg, "\n");

    // print the leaves
    let xtop = format!("{BGCOLOR_RED}@{COLOR_SYSTEM}{COLOR_RED}X{COLOR_SYSTEM}");
    let star = format!("{light_color}*{COLOR_SYSTEM}");
    println!("{}{}", " ".repeat(TREE_WIDTH), xtop);
    for n in (1..TREE_WIDTH).rev() {
        let fslash = format!(
            "{}{}{}",
            COLOR_GREEN,
            "/".repeat(TREE_WIDTH - n),
            COLOR_SYSTEM
        );
        let bslash = format!(
            "{}{}{}",
            COLOR_GREEN,
            "\\".repeat(TREE_WIDTH - n),
            COLOR_SYSTEM
        );
        println!("{}{}{}{}{}", " ".repeat(n), star, fslash, bslash, star);
    }
    let trails = format!(
        "{}{}{}",
        trail_color,
        ";".repeat(TREE_WIDTH * 2),
        COLOR_SYSTEM
    );
    println!("{}{}{}", star, trails, star);

    //print out the trunk
    let trunka = format!("{}{}{}", BGCOLOR_YELLOW, "||", COLOR_SYSTEM);
    for _n in 1..(TREE_WIDTH / 10) {
        println!("{}{}", " ".repeat(TREE_WIDTH), trunka);
    }
    println!("{}{}{}", " ".repeat(TREE_WIDTH), trunka, "\n");
}
