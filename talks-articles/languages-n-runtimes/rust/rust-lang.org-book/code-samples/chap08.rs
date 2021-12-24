fn main() {
    println!("\n[VECTORS]\n");
    use_vectors();
    println!("\n\n[UTF-8 Encoded Text]\n");
    use_utf8txt();
    println!("\n\n[HashMap]\n");
    use_hashmap();
}

// Vector
fn use_vectors() {
    let vec_a = vec![11, 21, 32];
    let vec_b = use_vectors_empty_push();
    println!("first a {}", vec_a[0]);
    println!("first b {}", vec_b[0]);

    /*
     * following would fail,
     * as vec_a gets moved in first call
     * so can't be borrowed again;
     * thus passed as reference
     *
    println!(
        "2nd in provided vector: {}",
        use_vectors_match_2nd(vec_a[1], vec_a)
    );
    println!(
        "2nd in provided vector: {}",
        use_vectors_match_2nd(vec_a[2], vec_a)
    );
    */
    for a in &vec_a {
        println!(
            "is {} 2nd in vec_a: {}",
            *a,
            use_vectors_match_2nd(*a, &vec_a)
        );
    }
    use_vector_enum();
}

fn use_vectors_empty_push() -> Vec<&'static str> {
    let mut vec = Vec::new();
    vec.push("plato");
    vec.push("aristotle");
    vec.push("socrates");
    vec[0] = "Mr. Plato"; // this could change only if mutable as for anything else in Rust
    return vec;
}

fn use_vectors_match_2nd(val: i32, vec: &Vec<i32>) -> bool {
    match vec.get(1) {
        Some(v) => {
            if val == *v {
                true
            } else {
                false
            }
        }
        None => false,
    }
}

fn use_vector_enum() {
    #[derive(Debug)]
    enum CalendarPage {
        Year(u16),
        Month(String),
    }
    let page = vec![
        CalendarPage::Year(2021),
        CalendarPage::Month(String::from("Dec")),
    ];
    println!("page for {:?}/{:?}", page.get(1), page[0]);
}

// UTF-8 Text
fn use_utf8txt() {
    use_utf8txt_create();
    use_utf8txt_update();
    use_utf8txt_index();
    use_utf8txt_slice();
    use_utf8txt_loop();
}

fn use_utf8txt_create() {
    let data = "loose yourself in the music";

    let mut sx1 = String::new();
    if sx1 == "" {
        sx1 = data.to_string();
    }

    let sx2 = String::from(data);
    println!("{} : {}", sx1, sx2);

    let helo = String::from("नमस्ते");
    println!("{}", helo);
}

fn use_utf8txt_update() {
    let mut helo = String::from("Buenos Dias");
    println!("{}", helo);
    helo = String::from("Buenos Noches");
    helo.push_str(", Rustaceans!"); // pushing a string
    helo.push('!'); // pushing a char

    let x = " .BLAH";
    helo.push_str(x);
    println!("{}", helo);
    println!("still has ownership, not passed for: {}", x);

    let base = String::from("yada");
    let suffixa = "-yada";
    let y = base + &suffixa;
    let msg = format!(
        "{}; can't access base anymore but can suffixa: {}",
        y, suffixa,
    );
    println!("{}", msg);
}

fn use_utf8txt_index() {
    println!("direct indexing isn't allowed, range based slicing is");
}

fn use_utf8txt_slice() {
    let helo = String::from("Buenos Noches");
    println!("{}", &helo[2..5]); // doesn't allow &helo[0..1]
                                 /*
                                  * following would fail for
                                  * byte index 2 is not a char boundary;
                                  * it is inside 'न' (bytes 0..3) of `नमस्ते`
                                  *
                                 let helo = String::from("नमस्ते");
                                 println!("{}", &helo[2..5]);
                                 */
}

fn use_utf8txt_loop() {
    let helo = String::from("नमस्ते");
    for c in helo.chars() {
        println!("{}", c);
    }
    for c in helo.chars() {
        println!("{:?}", c);
    }
    for c in helo.bytes() {
        println!("{}", c);
    }
}

// HashMap
use std::collections::HashMap;

fn use_hashmap() {
    let kv = use_hm_create();
    use_hm_access(kv);
    use_hm_update();
}

fn use_hm_create() -> HashMap<char, &'static str> {
    let mut hm = HashMap::new();
    hm.insert("x".to_string(), 101);
    println!("{:?}", hm);

    let k = vec!['x', 'y', 'z'];
    let v = vec!["X", "Y", "Z"];
    let kv: HashMap<_, _> = k.into_iter().zip(v.into_iter()).collect();
    println!("{:?}", kv);
    kv
}

fn use_hm_access(kv: HashMap<char, &'static str>) {
    println!("{:?}", kv.get(&'y'));
    match kv.get(&'a') {
        Some(val) => println!("got {}", val),
        None => println!("got none"),
    }
    for (k, v) in &kv {
        println!("{} got {}", k, v);
    }
}

fn use_hm_update() {
    let mut kv = HashMap::new();
    kv.insert(10, "ten");
    kv.insert(20, "twenty");
    kv.insert(30, "thirtee");
    println!("{:?}", kv);

    // overwrite
    kv.insert(30, "thirty");
    println!("{:?}", kv);

    // if not exists
    kv.entry(30).or_insert("thirtyyy");
    kv.entry(40).or_insert("fort");
    println!("{:?}", kv);

    use_hm_update_wordcount();
}

fn use_hm_update_wordcount() {
    let txt = "waves crashing at volcanic rocks at sunrise";
    let mut wordcount = HashMap::new();
    let mut charcount = HashMap::new();
    for wrd in txt.split_whitespace() {
        // using reference to key's content so whether new 0 or old; gets incremented next
        let count = wordcount.entry(wrd).or_insert(0);
        *count += 1;
    }
    println!("{:?}", wordcount);
    for ch in txt.chars() {
        let count = charcount.entry(ch).or_insert(0);
        *count += 1;
    }
    println!("{:?}", charcount);
}
