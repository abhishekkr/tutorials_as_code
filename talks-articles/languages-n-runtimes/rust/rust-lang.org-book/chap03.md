
## Common Programming Concepts

### Variables and Mutability

* default mode of `let var = "value";` is immutable with compile time errors on re-assignment

* `let mut var = "mutable value";` allows make variable mutable via `mut` keyword

* constants (`const HOURS: u32 = 24;`) and immutable variables have some differentiators

> * since constants are **always** immutable, not just by default.. type value must be annotated
>
> * Rust's convention is all CAPS for constants
>
> * [Constant Evaluation](https://doc.rust-lang.org/reference/const_eval.html) is process of computing result of expressions during compilation. Only a subset of all expressions can be evaluated at compile-time.

#### Shadowing

* can shadow a variable's value to what program sees by repeating use of `let`

> * without use of `mut` compiler will raise error on accidental overriding value without `let`
>
> * with this type of variable can be altered since it's redefining the var


### Data Types

* statically typed; compiler can usually infer based on usage intention

> needs explicit notations in certain cases, as `parse()` included while assignment

* scalar types as integers, floats, booleans, characters

* signed integers `i8/i16/i32/i64/i128/isize` & unsigned `u8/u16/u32/u64/u128/usize`; `isize` & `usize` depend on 32bit/64bit arch

> * number literals: decimal `1_000`, hex `0xf1a`, octal `0o57`, binary `0b1100`, byte `b'A'`
>
> * `Integer Overflow`, changing variable (say `u8`) to an out of range value (say `256`). Compiling in debug mode includes checks to cause panic at runtime. Compiled with `--release` flag doesn't include these checks but perform `two's complement wrapping` causing an unwanted value to be assigned.

* `f32` & `f64` (default) for floats; `true/false` for bool; char literals in single quotes 4byte for Unicode

* 2 primitive compound types as tuples and arrays

* tuples stay of declared length `let tup = (10, 20); let (x, y) = tup;`

> * tuple elements also accesible as `let x: (i32, f64, u8) = (500, 6.4, 1); println!("{}", x.0);` for `500`
>
> * empty tuple `()` has `unit value` i.e. `()` of `unit type`

* arrays are traditional `let arr = [11, 22]; let arra: [i32; 3] = [1, 2, 12];`; allocated on stack

> * to have an array with repeated element like `let ar = [2; 10];` for `[10, 10]`
>
> * does panic on out-of-bound index access

* a similar `vector` type can grow & shrink

```
let mut xs = vec![1i32, 2, 3];
println!("xs vector: {:?} | {}", xs, xs.len());
xs.push(10);
println!("xs vector: {:?} | {}", xs, xs.len());
```


### Functions

* defalred with `fn`, entrypoint function is `fn main`; snake-case naming convention

* definition order of functions isn't of significance

* `fn func_name (param: i32)` is the way to decalre paramaeters in signature, needs type

* func bodies are series of statements optionally ending with expression; calling a func or macro is expression

```
// valid
let num = 10;
let pow2 = {
  let power = 2;
  // here writing 'num * power;' will make it a statement due to semicolon and cause error
  num * power
};
```

* similarly for return

```
// valid
fn plus_one(x: i32) -> i32 {
    x + 1
}
fn plus_two(x: i32) -> i32 {
    return x + 2;
}
```


### Comments

* `// single line comments`, or multiple line with double forward slash at each line


### Control Flow

#### Conditional `if`

* if expression here only work with `bool` values not truthy/falsy; if-else-if-else blocks are just concats

```
// valid
let x = if true { 10 } else { 0 };
// can not have different type values via if-else, must have else

if x == 10 {
  println!("it's 10");
}

if x > 10 {
  println!("it's greater than 10");
} else {
  println!("it's less than or equal to 10");
}

if x > 10 {
  println!("it's greater than 10");
} else if x < 10 {
  println!("it's less than to 10");
} else {
  println!("it's equal to 10");
}
```

#### Loops with `loop`, `while` and `for`

> managing flow with `break` and `continue`

* `loop` runs block unless explicitly broken

```
// valid
let mut x = 10;
loop {
  x -= 1;
  if x < 1 {
    break;
  }
}
```

* can make `loop` return value

```
// valid
let mut x = 10; let mut sum = 0;
let sum_of_1_to_10 = loop {
  sum += x;
  x -= 1;
  if x < 1 {
    break sum;
  }
};
```

* `while` follows most common pattern `while _condition(..) { ... }`

* `for` is safest for iteration over ranges/iterables for no out-of-bound probability

```
for elem in (1..5).rev() {
  println!("the value is: {}", elem);
}
```

---

