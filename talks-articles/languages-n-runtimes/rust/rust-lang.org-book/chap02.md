
## Programming a Guessing Game

* `cargo new chap02_guessing_game` under [code-samples](./code-samples) creating [chap02\_guessing\_game](./code-samples/chap02_guessing_game/src/main.rs)

* has the boilerplate hello world code, could `cd chap02_guessing_game` and `cargo run`

### Processing a Guess

> a mutable string var `let mut guess = String::new();`

* without `mut` keyword, `let` creates an immutable var

* `::` denoes `new` is a function associated with `String`


> * populate it with user input `io::stdin().read_line(&mut guess).expect("Failed to read line");` with `use std::io;`

* can use `std::io::stdin()` if not `use std::io;`

* `&` like popular usage deals in reference

* `io::Result` enumerations gets returned by `read_line(..)`, Result variants are `Ok` or `Err`

* to handle `Err` value, `expect(..)` gets used and if not handled gives a compile warning

> `{}` placeholder for value interpolation into stringat `println!`


### Generating a Secret Number

> to generate random integer between 1 and 100 using [rand](https://crates.io/crates/rand) crate added to `Cargo.toml`

* `chap02_guessing_game` is a binary crate, `rand` is a library crate

* `cargo build` would get new dependencies; `cargo update` to update dependencies upto minor versions

* `rand::Rng` trait gets used; in `gen_range` `1..=100` could have been used instead of `1..101`

> `cargo doc --open` builds doc for all dependencies to be browsed

> bring type `Ordering` into scope, an enum with `Less, Greater, Equal` variants

* `cmp` method at a Type comapres given value with it and returns one of the Ordering variants; with `match` expression working like Switch in other languages

* would need `guess` to be Int parsed to be `cmp`-d with generated secret Int; same variable name could be used as Rust allows shadowing of existing variable names

* annotating variable type with `<varname>: <type>`

> extract `get_input() -> u32` and put the call and `match` into `loop {..}`; using `break;` to leave on match

> instead of `guess.trim().parse().expect("NaN");` using a `match` on `parse()` to check if returned value is a number then that else an out-of-range value to allow `continue;` the loop

---

