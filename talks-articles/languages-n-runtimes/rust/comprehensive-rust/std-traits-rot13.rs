use std::io::{Read, Result};

struct RotDecoder<R: Read> {
    input: R,
    rot: u8,
}

// Implement the `Read` trait for `RotDecoder`.
impl<R: Read> Read for RotDecoder<R> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        let upper_a = 'A' as u8;
        let lower_a = 'a' as u8;
        let size = self.input.read(buf)?;

        let rotate = |chr: u8, base: u8| -> u8 { (chr - base + self.rot) % 26 + base };

        for chr in &mut buf[..size] {
            if !chr.is_ascii_alphabetic() {
                continue;
            }
            *chr = match chr.is_ascii_uppercase() {
                true => rotate(*chr, upper_a),
                false => rotate(*chr, lower_a),
            };
        }
        Ok(size)
    }
}

fn main() {
    let mut rot = RotDecoder {
        input: "Gb trg gb gur bgure fvqr!".as_bytes(),
        rot: 13,
    };
    let mut result = String::new();
    rot.read_to_string(&mut result).unwrap();
    println!("{}", result);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn joke() {
        let mut rot = RotDecoder {
            input: "Gb trg gb gur bgure fvqr!".as_bytes(),
            rot: 13,
        };
        let mut result = String::new();
        rot.read_to_string(&mut result).unwrap();
        assert_eq!(&result, "To get to the other side!");
    }

    #[test]
    fn binary() {
        let input: Vec<u8> = (0..=255u8).collect();
        let mut rot = RotDecoder::<&[u8]> {
            input: input.as_ref(),
            rot: 13,
        };
        let mut buf = [0u8; 256];
        assert_eq!(rot.read(&mut buf).unwrap(), 256);
        for i in 0..=255 {
            if input[i] != buf[i] {
                assert!(input[i].is_ascii_alphabetic());
                assert!(buf[i].is_ascii_alphabetic());
            }
        }
    }
}
