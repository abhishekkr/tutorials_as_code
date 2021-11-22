
## Getting Started

> when faced with compile time warning, [check this](https://doc.rust-lang.org/rustc/lints/listing/warn-by-default.html)

### Hello, World!

* an introductory code as [chap01.rs](code-samples/chap01.rs)

* has `fn main() { ... }`, the first code to run in an executable which has no params and returns nothing

> `rustc` to compile, `rustfmt` to format code

* `println!` is a macro as specified by `!`, `;` as end of expression

* declares a var called `user`, assigned a env var if existed otherwise a static string using `match`

* decalres and calls a function `capitalize_first_letter` that accepts a string and returns one

> `rustc chap01.rs` would create a binary `chpa01` printing `Hello <username>`,
>
> with `Hello World` printed if no `USER` env var is available


### Hello, Cargo!

> rust's build system and package manager

* create new project as `cargo new new_project`; creates `new_project` dir with `Cargo.toml` meta file and `src/main.rs` as starting point

* `Cargo.toml` has a `pacakge` section for details and rust `edition` config, `dependencies` section for lib listing

* build & run as `cargo build && ./target/debug/new_project` or `cargo run`; it doesn't compile each run if source files don't change

* `cargo check` ensures no compile issues exist without producing binary

* `cargo build --release` when ready for release, this compiles with optimizations under `target/release`

> use release build for benchmarking tests

* Cargo helps maintaining standards when building a complex project composed of multiple crates

---

