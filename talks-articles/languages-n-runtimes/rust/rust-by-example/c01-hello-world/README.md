
## 01. Hello World

> cover comments, doc-comments, basic function skeleton


* sample [rust code](./hello-world.rs)

> above `println!` is a macro printing text to console
> [macros](https://rustbyexample.com/macros.html) allow metaprogramming, used and look like other functions except their name ends with a bang `!`

* macros for print
> * format! ~ formatted text to `string`
> * print! ~ print formatted text to stdout
> * println! ~ print! with a newline appended
> * eprint! ~ print formatted text to stderr
> * eprintln! ~ eprint! with a newline appended

* `fmt::Debug` uses `{:?}` for debugging, `fmt::Display` uses `{}` for user
> `fmt::Display` is cleaner than `fmt::Debug`
> structure need to `impl` a `Display` and deriver `#[derive(Debug)]` for them to work

```
use std::fmt;

struct List(Vec<i32);

impl fmt::Display for List {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let vec = &self.0; // extract value using tuple indexing, create ref to vec

    write!(f, "[")?; // to see if it errors, returns error else continues

    for (count, v) in vec.iter().enumerate() {
      if count != 0 { write!(f, ", ")?; }
      write!(f, "{}", v)?;
    }

    try!(write!(f, "]")); // as an alternative to ? post write!
  }
}

fn main() {
  let v = List(vec![1,2,3]);
  println!("{}", v);
}
```


* compiling a rust code

```
rustc hello-world.rs
```


* above would create a binary by code file name without extension, run it like any other binary

```
./hello-world
```

---
