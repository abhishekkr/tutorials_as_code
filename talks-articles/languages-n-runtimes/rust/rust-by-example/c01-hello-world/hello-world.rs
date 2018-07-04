//! is a doc-comment

use std::fmt::{self, Formatter, Display};

// comment like haskell, C, go
/*
  below is a skeleton function
*/

// snake case-d function name
fn some_formatted_prints(){
    /// This is also a doc comment
    ///
    /// # Argument
    ///
    /// none
    ///

    // declaring a var with 'let'
    let somevar = format!("stringified args with braces - {}", 10);
    println!("{}", somevar);

    // print to console
    print!("stringified with idx - {0}, {1}, {0}\n", 10, 25);

    // like print! with newline wrapped
    println!("name arg fst:{fst}, snd:{snd}", fst="one", snd="two");

    eprint!("this goes out to stderr\n");
    eprintln!("this goes out to stderr with newline");
}

// more formatted prints
fn more_formatted_prints(){
    println!("{} is {:b} in binary", 10, 10); //special formatting
    println!("{} is 0x{:X} in hex", 10, 10); //special formatting
    println!("{} is 0o{:o} in octal", 10, 10); //special formatting
    println!("right align text: {number:>width$}", number=7, width=25);
    println!("pad extra zeroes: {number:>0width$}", number=7, width=25);

    #[allow(dead_code)]
    struct S(i32);
    //println!("struct print here failing :? {:?}", S);
}

fn formatting_eg(){
    #[derive(Debug)] // debug to be derived
    struct Celsius {
        temperature: f32,
    }

    impl Display for Celsius {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            let  temperature = self.temperature + 32.0;
            write!(f, "{} farenhite", temperature)
        }
    }

    for t in [
        Celsius{ temperature: 1.0 },
        Celsius{ temperature: 25.0 },
        Celsius{ temperature: 100.0 },
    ].iter(){
        println!(">> {}", *t)
    }

    let c = Celsius{ temperature: 123.0 };
    println!("{} | ? {:?} | pretty print debug {:#?}", c, c, c);

    // reordering provided params in interpolation
    println!("{1:?} {0:?} is the {actor:?} name.",
             "Slater",
             "Christian",
             actor="actor's");
}

fn main(){
    /// main()
    ///
    /// prints 'yada yada yoda' when ran
    println!("yada yada yoda");
    some_formatted_prints();
    more_formatted_prints();
    formatting_eg();
}
