
## Error Handling

> [code-sample](./code-samples/chap09.rs)

* Rust has 2 category of errors, `recoverable` & `unrecoverable`.

* It doesn't have exceptions. But `Result<T,E>` return for recoverable errors and `panic!` macro for crash & burn.

### Unrecoverable Errors with *panic!*

* `panic!("..");` macro would print message, unwind, clean-up stack and quit

> `unwinding` means Rust walks back the stack cleaning data from each function; if not done and just opted for `abort`.. memory cleaning would be left for OS
>
> * opting for `abort` does keep binary size slightly smaller as well

* can switch to `abort` at `panic!` by updating `Cargo.toml` with

```
[profile.release]
panic = 'abort'
```

* an internal error like `buffer overflow` due to out-of-bound element access at Vector could trigger `panic!` as well

> can set env var `RUST_BACKTRACE=1` for all steps to be printed; and `RUST_BACKTRACE=all` for more verbose backtrace

* these debug symbols are enabled by default when usng `cargo build` or `cargo run` without `--release`


### Recoverable Errors with *Result*

* `Result` enum type has 2 variants `Ok` & `Err` as `enum Result<T,E> {Ok(T), Err(E)}`; T & E being generic types here

> * `T` is whatever intended Result type is, like `std::fs::File` when file is opened
>
> * `E` is of `std::io::Error` type

* different Error `kind` could be matched for branched out error handling

* use `unwrap_or_else` method can clean up nested match for Result's error handling branches as in `recoverable_unwrap()` at code-sample

* `risky_giving_result().unwrap()` will make it panic if Result variant is Err

* `risky_giving_result().expect("..")` allows to choose panic message to convey intent

* any function can return `Result<T, E>` to bubble up error instead of acting on it; as in code-sample `propagate_error()`

* using operator `?`, shortcut `risky_giving_result()?;` could be used to return `Err(e)` if an error or just assign whatever value is passed for `Ok(T)`.. code-sample `propagate_error_operator()`

> * although, since it's a common operation to read file.. this entire error handled logic is available in stdlib as `fs::read_to_string(filename);`
>
> * but to implement by self, the entire `propagate_error()` could be shorthanded into cleaner code as

```
fn read_file(filename String) -> Result<String, io::Error> {
  let mut s = String::new();
  File::open(filename)?.read_to_string(&mut s)?; // here handles existence & readability
  Ok(s)
}
```

* `?` can only be used in functions returning `Result<T, E>` type or `Option` (any type implementing `std::ops::Try`)

```
use std::error::Error;
use std::fs::File;

fn main() -> Result<(), Box<dyn Error>> {  // here Box<dyn Error> is for any kind of error
  File::open("/blah")?;
  Ok(())
}
```


### To *panic!* or Not to Panic

* example, prototyping, tests shall just call `unwrap` or `expect` for quick feedback

* for flows where context of error depends, better to handle `Result` with giving more info on issue

* `panic!` reasonable when flow shall reset to avoid bad state (bad user input or unhandled library response)

* better to make a new type for fields which require detailed validations; as they can be part of implementation and logically managed in domain

---

