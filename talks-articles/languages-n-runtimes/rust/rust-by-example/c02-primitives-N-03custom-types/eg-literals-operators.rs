fn main() {
    println!("1 + 2 = {}", 1u32 + 2); // int addition
    println!("1 + 2 = {}", 1i32 - 2);
    // println!("1 + 2 = {}", 1u32 - 2); // can result into overflow

    println!("hex: {}, octal {}, binary: {}", 0xf, 0o77, 0b1000);

    // short-circuit boolean
    println!("true AND flase is {}", true && false);
    println!("true OR flase is {}", true || false);
    println!("not true is {}", !true);

    // bitwise opeations
    println!("0011 AND 1101 is {:04b}", 0b0011u32 & 0b1101);
    println!("0011 OR 1101 is {:04b}", 0b0011u32 | 0b1101);
    println!("0011 XOR 1101 is {:04b}", 0b0011u32 ^ 0b1101);
    println!("1 << 5 is {}", 1u32 << 5);
    println!("0x80 >> 2 is 0x{:x}", 0x80u32 >> 2);

    // underscores to improve readability
    println!("one mil like {}", 1_000_000u32);
    println!("0.0001 like {}", 0.00_01f32);
}
