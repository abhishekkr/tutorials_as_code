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

fn primitive_types(){
    let sach = true
    let jhoot: bool = false

    let chr = 'c'
    let unicode_chr = '💕'
}

fn main() {
    println!("Hello, Rust!");
    
    using_let(40);

    primitive_types();

    // let xyz: String = oops(); // a diverging function can be used as any type
    oops();
}
