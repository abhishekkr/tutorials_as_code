use std::fs::File;
use std::io::{self, ErrorKind, Read};

fn main() {
    recoverable_error();
    recoverable_unwrap();
    println!("{:?}", propagate_error());
    println!("{:?}", propagate_error_operator());
}

fn recoverable_error() {
    let fyl = File::open(".");
    match fyl {
        Ok(t) => println!("=> {:?}", t),
        Err(e) => println!("=> {:?}", e),
    }

    let fyl = File::open("blah");
    match fyl {
        Ok(t) => println!("=> {:?}", t),
        Err(e) => match e.kind() {
            ErrorKind::NotFound => {
                println!("can call file create option here and return file handle")
            }
            other_err => panic!("Unhandled Error: {:?}", other_err),
        },
    }
}

fn recoverable_unwrap() {
    let fyl = File::open("blah.blah").unwrap_or_else(|err| {
        if err.kind() == ErrorKind::NotFound {
            println!("can call file create option here and return file handle");
            File::create("/tmp/blah").unwrap_or_else(|error| {
                panic!("can't create /tmp/blah: {:?}", error);
            })
        } else {
            panic!("Unhandled Error: {:?}", err);
        }
    });
    println!(
        "it would have panicked if not manageable; otherwise would have created file; {:?}",
        fyl
    );
}

fn propagate_error() -> Result<String, io::Error> {
    // here since mutable s need to be passed, f need to be mutable as well
    let mut f = match File::open(".") {
        Ok(f) => f,
        Err(e) => return Err(e),
    };
    println!("{:?}", f);
    let mut s = String::new();
    return match f.read_to_string(&mut s) {
        Ok(_) => Ok(s),
        Err(e) => Err(e),
    };
}

fn propagate_error_operator() -> Result<std::fs::File, io::Error> {
    let f = File::open("/blah")?;
    return Ok(f);
}
