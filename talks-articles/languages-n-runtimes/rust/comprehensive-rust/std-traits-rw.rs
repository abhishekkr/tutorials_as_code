use std::fs::File;
use std::io::{BufRead, BufReader, Read, Result, Write};

use std::io::BufWriter;
use std::net::TcpStream;

/*
// For Cusom Error
type Result<T> = std::result::Result<T, WriteError>;
#[derive(Debug, Clone)]
struct WriteError;
impl fmt::Display for WriteError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Failed to Write.")
    }
}
*/

/*
 * 'Read', 'BufRead' to abstract over u8 sources
 */
fn grep_lines<R: Read>(reader: R, expr: &str) {
    let buf_reader = BufReader::new(reader);
    for lyn in buf_reader.lines() {
        let txt: String = lyn.unwrap_or_default();
        if txt.clone().contains(expr) {
            // println!("{:?}", txt);
            match write_multi(txt) {
                Err(e) => println!("[ERROR] {:?}", e),
                Ok(()) => (),
            }
        }
    }
}

fn grep_file(file_path: &str, expr: &str) {
    let fyl = File::open(file_path);
    match fyl {
        Ok(f) => grep_lines(f, expr),
        err => println!("Failed: {:?}", err),
    }
}

/*
 * 'Write' for u8 sinks
 */

fn write_log<W: Write>(writer: &mut W, msg: &str) -> Result<()> {
    writer.write_all(msg.as_bytes())?;
    writer.write_all(b"\n")
}

fn write_tcp<W: Write>(writer: &mut W, msg: &str) -> Result<()> {
    writer.write(&msg.as_bytes())?;
    writer.write_all(b"\n")?;
    writer.flush()?;
    Ok(())
}

fn write_multi(txt: String) -> Result<()> {
    let mut buffer = Vec::new();
    write_log(&mut buffer, &txt)?;
    println!("Logged: {:?}", buffer);

    // can start a TCP server to receive it using 'nc -kl 9876'
    let mut stream = BufWriter::new(TcpStream::connect("127.0.0.1:9876").unwrap());
    write_tcp(&mut stream, &txt)?;

    Ok(())
}

fn main() {
    grep_file("std-traits-rw.rs", "fn ");
}
