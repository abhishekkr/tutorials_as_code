// likes snakeCase naming
fn using_let(some_var: i32) {
    // let expression is a pattern rather than being a variable
    let a = multiply(2,5);
    let (b,c) = (1,2); //inferential static type system
    let d : i32 = 3; //though can specify type if required
    // by default bindings are immutable so now can't do
    // d = 33;
    let mut e = 4; // need to ask for mutable association
    e = some_var + e;
    // will give warning if ```let x i32``` but never initialized and used
    // gives error if never initialized but used
  
    println!("{} {} {} {} {}", a, b, c, d, e)
    // formatting rules that can be used: http://doc.rust-lang.org/std/fmt/
}

fn multiply(x: i32, y: i32) -> i32 {
    // return x*y; // could have been used but poor style; only good for early returns
    x * y //here a semicolon will give error
}

/*
Rust is an expression based language with just two types of statements. Declaration and Expression statements.
`let` can only begin a statement not an expression.
let x = (let y = 5); // ERRONEOUS

Here value of an assignment is an empty tuple as assigned value can have just one owner.
let mut y = 5;
let x = (y = 7); //makes y =7 and x =()

Expression statement turns any expression to a statement. Semicolon separated expressions.
*/

fn oops() -> ! {
/*
   its return type is ! (called 'diverges')
   cuz it will crash and never return hence is a diverging function
*/
    panic!("oye bye")
}

fn primitive_types() -> fn(i32) -> i32 {
    let sach = true;
    let jhoot: bool = false;

    let chr = 'c';
    let unicode_chr = '💕';
    println!("{} {} {} {}", sach, jhoot, chr, unicode_chr);

    //arrays
    let arr = [1, 10, 10000, 1000, 100];
    let all = [7; 20]; // a: [i32; 20] //20 elements, all init to 7
    println!("{}\n{} {}", all.len(), all[0], all[19]); //doesn't interpolate array
    for x in &all {
        print!("{}, ", x);
    }

    //slice
    let middle_arr = &arr[1..3];
    let full_arr = &arr[..];
    println!("len {} :@0 {}\nlen {} :@0 {}", middle_arr.len(), middle_arr[0], full_arr.len(), full_arr[0]);
    // coercing an array to a slice
    let str_slice: &[&str] = &["one", "two", "three"];
    // above were shared slice, mutable slice is following
    let mut_slice = &mut ["oh", "ok"];
    mut_slice[1] = "no";
    println!("len {} :@1 {}\nlen {} :@1 {}", str_slice.len(), str_slice[1], mut_slice.len(), mut_slice[1]);
    print!("window of 2: ");
    for x in str_slice.windows(2) {
        print!("{:?}, ", x);
    }
    print!("\nchunk of 2: ");
    for x in str_slice.chunks(2) {
        print!("{:?}, ", x);
    }

    let tada = "tada";
    let nada: &str = "nada";
    println!("\n{} <> {}", tada, nada);

    //tuples
    let tupl01 = (1, "ohk");
    let tupl02: (i32, &str) = (1, "oh ok");
    let tupl03: (i32, &str) = (1, "ohk");
    if tupl01 == tupl02 {
      println!("don't compare it")
    } else if tupl01 == tupl03 {
      println!("just compare it; tuple indexes {}, {}", tupl01.0, tupl03.1)
    }

    fn some_foo(x: i32) -> i32 { x+1 }
    return some_foo
}

/// * rustdoc for main
fn main() {
    // rust got 2 types of comments: line-comment and doc-comment
    // this is line-comment; doc-comment got /// or //! and supports Markdowno

    //! these **//!** are used for non-method rustdoc
    //! * rustdoc can be used to generate document from doc-comment
    //! usage:
    //! ```
    //! rustdoc -w html -o a.html src/main.rs
    //! ```

    println!("Hello, Rust!");

    using_let(40);

    let some_fn: fn(i32) -> i32 = primitive_types(); //function primitive and others
    println!("increment 1 to {}", some_fn(1));

    // let xyz: String = oops(); // a diverging function can be used as any type
    oops();
}
