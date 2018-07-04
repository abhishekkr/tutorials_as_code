// variable bindings

// supress all warnings caused by casting overflow
#![allow(overflowing_literals)]

type SmallNumber = u8;
type LargeNumber = u64;

fn main() {
    let an_int = 1u32;
    let a_bool = true;
    let a_unit = ();

    let int_copy = an_int; // copy an int to another

    println!("{:?} == {:?}", an_int, int_copy);
    println!("the bool {:?}", a_bool);
    println!("the unit {:?}", a_unit);

    /* casting */
    let a_decimal = 65.4321_f32;
    // errorneous; no implicit conversion
    // let a_uint: u8 = a_decimal;

    // explicit conversion
    let a_uint = a_decimal as u8;
    let a_char = a_uint as char;
    println!("cast: {} -> {} -> {}", a_decimal, a_uint, a_char);

    println!("1000 as a u8 is : {}", 1000 as u8);
    println!("-1 as a u8 is : {}", (-1i8) as u8);
    println!(" 128 as a i16 is: {}", 128 as i16); // if most significant bit of value is 1
    println!(" 128 as a i8 is: {}", 128 as i8); // then the value is negative

    /* literals */
    // `&x` passed by reference as in C
    println!("size of `a_decimal` in bytes: {}", std::mem::size_of_val(&a_decimal));
    // size_of_val is called here by full path, defined in `mem` module of `std` crate
    println!("size of `a_uint` in bytes: {}", std::mem::size_of_val(&a_uint));

    /* using aliased types */
    let small: SmallNumber = 2;
    let large: LargeNumber = 987654321;
    println!("small: {}, large: {}", small, large)
}
