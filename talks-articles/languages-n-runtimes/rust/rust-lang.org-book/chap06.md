
## Enums and Pattern Matching

* Rust's enums are like algebraic data types from Haskell

### Defining an Enum

> in [chap06.rs](code-samples/chap06.rs), `MonitorState` declares 3 states reflective of what a check over process state monitor might give

* `enum` can be mapped to data directly as with `ServicePort` example; can have multiple components as in `Service`; component could be any type.. even struct

* `ServiceAction` enum has a field with no data associated, a field with struct like fields and a field with unnamed field

> * `enum` doesn't use `struct` keyword, and all variants are grouped together under `ServiceAction` type
>
> * decalring them as structures will make too many independent types

* methods can be defined on enums using `impl`, as for `ServiceAction`

* `if let` matching can be used to fetch internal fields as done for `NamedPort` from `ServiceAction` in `service_action`

#### Option Enum and its advantages over Null Values

* `Option` enum defined by stdlib. `Option` type used in places where a value is something or nothing.

```
// defined in stdlib as
enum Option<T> {  // <T> is a Generic Type syntax, chapter.10
  None,
  Some(T),
}

// example usage
let some_int = Some(10);
let missing_int: Option<i32> = None;
```

> Rust doesn't have NULL feature. Which devoids it of any chance using a NULL value where only NOT NULL is handled.

* it's variants `None` & `Some` could be used without `Option::`

> we can't mix a standard `(type)` and `Some(type)`

```
let x: i32 = 10;
let y: Option<i32> = Some(10);
let x_plus_y = x + y; // is ERRORNEOUS
```

* thus, only a type `Option<T>` would be capable of holding `None` or `Some` value of `T`



### The `match` Control Flow Operator

* this allows following, or as in example of enum `RGB`

```
enum XOrY {X, Y}

fn value_in_x_or_y(xy: XOrY) -> u8 {
  match xy {
    XOrY:X => 10,
    XOrY:Y => 15,
  }
}
```

* the `match` could contain an expression on RHS to `=>` as well

* can have one enum nested into another and pattern match the nested enum as

```
enum AB{A, B}
enum CD{C, D(AB)}
...
match cd {
  CD::C => "c",
  CD::D(what) => {
    println!("a or b: {:?}", what);
    "d"
  }
}
```

* `match` can be similarly applied over `Option<T>`, to alter value internal to `Some(T)`, covering `None` case is required

> examples at `handle_incr_option` and `handle_option_normalization`

* `match` have to be exhaustive of all possible branches; or could use `_` else `var_name` placeholder; `var_name` used when any other value is to be passed or used in branched expression

> example of `_` is in `print_style()`



### Concise Control Flow with `if let`

* `if let` allows to match one pattern and ignore rest

* `else` block is allowed to be linked block as well

```
let xcode = Some(String::from("Ferrous"));
if let Some(x) = xcode {
  println!("{:?}", x);
}

if let Some(x) = xcode {
  println!("{:?}", x);
} else {
  println!("no code provided");
}
```

---
