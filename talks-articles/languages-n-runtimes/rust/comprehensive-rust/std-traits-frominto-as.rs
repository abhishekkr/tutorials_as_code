// impl From & Into for lossless conversions
fn sample_from_into() {
    let s: String = "hello".into();
    let addr: std::net::Ipv4Addr = [127, 0, 0, 1].into();
    let one: i16 = true.into();
    let bigger: i32 = 123_i16.into();
    assert_eq!(String::from("hello"), s);
    assert_eq!(std::net::Ipv4Addr::from([127, 0, 0, 1]), addr);
    assert_eq!(i16::from(true), one);
    assert_eq!(i32::from(123_i16), bigger);
    println!("{s}, {addr}, {one}, {bigger}");
}

// 'as' for explicit type conversion; could be buggy for
fn sample_as() {
    let value: i64 = 100000;
    println!("i64 value: {}", value);
    println!("as u32:    {}", value as u32);
    println!("as u16:    {}", value as u16);
    println!("as i16:   {}", value as i16);
    println!("as u8:     {}", value as u8);
}

fn main() {
    sample_from_into();
    sample_as();
}
