/*
 * ## Layers of built-in rust libraries.. core, alloc & std.
 * * 'core' includes fundamentals types & functions that don't even depend on libc, alloc or OS
 * * 'alloc' includes types as Box, Vec, Arc that require Global Heap Allocator
 * * 'std' is the Rust Stdlib available to all Rust crates, a minimal standardized stable set of
      primitive tpes, macros and core-use functionalities.
*/
use std::collections::HashMap;
use std::env;
use std::ops::Deref;

/// Prepares & Returns a chained string of '-' for provided count.
pub fn dash(count: u32) -> String {
    // This comment shall not occur. Not in internal doc. Below should.
    //! This shall be part of generated doc.
    /*!
     * > There are other ways to generate it as well.
     * > * (0..count).map(|_| "-").collect::<String>();
     * > * std::iter::repeat("-").take(count).collect::<String>()
     */
    String::from_utf8(vec![b'-'; count.try_into().unwrap()]).unwrap()
}

/// Sample for Option
pub fn char_index(txt: &str, chr: char) -> i32 {
    let position: Option<usize> = txt.find(chr);
    match position {
        None => -1,
        Some(idx) => idx.try_into().unwrap(),
    }
}

// Option<> sample with unwrap/expect
/// ```
/// use pew_pew::char_index_v2;
/// char_index_v2("exam", 'e');
/// ```
///
/// ```should_panic
/// use pew_pew::char_index_v2;
/// char_index_v2("Exam", 'e');
/// ```
pub fn char_index_v2(txt: &str, chr: char) -> i32 {
    txt.find(chr)
        .expect("Character not found.")
        .try_into()
        .unwrap()
}

// Result<> sample
pub fn get_env(key: String) -> String {
    match env::var(key.clone()) {
        Ok(val) => val,
        Err(err) => {
            println!("couldn't fetch env {}: {}", key, err);
            "".into()
        }
    }
}

// String sample
/// ```
/// use pew_pew::sample_string;
/// sample_string();
/// ```
pub fn sample_string() {
    let s1: String = String::from("test text");
    let mut txt = String::with_capacity(s1.len() + 1);
    txt.push_str(&s1);
    txt.push('!');
    println!(
        "txt: len = {}, capacity = {}, chars count = {}",
        txt.len(),
        txt.capacity(),
        txt.chars().count()
    );
    // Deref
    assert_eq!(s1.deref(), s1.to_string());
    assert_eq!(s1.chars().nth(7).unwrap(), 'x');
    assert_eq!(s1[5..s1.len()], "text".to_string());
}

// Vector sample
/// ```
/// use pew_pew::sample_vector;
/// sample_vector();
/// ```
pub fn sample_vector() {
    let mut names = Vec::new();
    names.push("alice");
    println!(
        "names: len = {}, capacity = {}",
        names.len(),
        names.capacity()
    );

    let mut more_names: Vec<&str> = Vec::with_capacity(names.len() + 1);
    more_names.extend(names.iter());
    println!(
        "more_names: len = {}, capacity = {}",
        more_names.len(),
        more_names.capacity()
    );

    // Canonical macro to initialize a vector with elements.
    let mut numbers = vec![0, 0, 1, 2, 3, 4];
    // Conditional selection
    numbers.retain(|x| x % 2 == 0);
    println!("{numbers:?}");
    // Remove consecutive duplicates.
    numbers.dedup();
    println!("{numbers:?}");

    // .get(idx) returns Option
    println!("number@0: {}; number@11: {:?}", numbers[0], numbers.get(11));
}

// Hashmap sample
/// ```
/// use pew_pew::sample_hashmap;
/// sample_hashmap();
/// ```
pub fn sample_hashmap() {
    let mut score = HashMap::new();
    score.insert("player-x", 100);
    score.insert("player-b", 51);
    score.insert("player-m", 30);

    if !score.contains_key("player-z") {
        println!("No player: player-z | total players: {}", score.len());
    }
    println!("player-z scored {}", score.get("player-z").unwrap_or(&25));

    match score.get("player-b") {
        Some(s) => println!("player-b scored {s}"),
        None => println!("player-b hasn't played"),
    };

    // Use the .entry() method to insert a value if nothing is found.
    let z_score: &mut i32 = score.entry("player-z").or_insert(0);
    // increase score
    *z_score += 10;

    let b_score: &mut i32 = score.entry("player-b").or_insert(0);
    *b_score += 10;

    println!("{score:#?}");
}
