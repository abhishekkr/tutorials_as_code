
## 02. Primitives

### Scalar Types

* signed integers: i8, i16, i32, i64, i128, isize (pointer size)

* unsigned integers: u8, u16, u32, u64, u128, usize (pointer size)

* floating point: f32, f64

* unicode scalar values: char (4bytes)

* str: as string slice, usually seen as `&str`, also the type of string literals `&'static str`; always UTF-8
> two types of strings:
>
> * String: stored as vector of bytes (`Vec<u8>`), always UTF-8. It's heap allocated, growable and not null terminated.
>
> * &str: is a slice `&[u8]` always pointing to UTF-8 sequence, a view into String

* boolean type

* unit: `()` with only possible value is an empty tuple

* [never](https://doc.rust-lang.org/std/primitive.never.html): `!` type represent the computation which never resolve to any value at all

* [pointer](https://doc.rust-lang.org/std/primitive.pointer.html): raw, unsafe pointers (`*const C`, `*mut M`)

* [reference](https://doc.rust-lang.org/std/primitive.reference.html): both mutable and shared, can get by using `&` or `&mut` operators on a value; or using a `ref` or `ref mut` pattern

---

### Compound Types

* [arrays](https://doc.rust-lang.org/std/primitive.array.html): `[1,2,3]`
> collection of objects of same type
> a fixed-size array, denoted [Type;Size], size is to be known at compile time

* tuples: `(1, true)`, can have tuples of tuples

* dynamically sized view into contiguous sequence

* slices are similar to arrays with unknown size at compile time
> it's a two word object, first is pointer to data and second is length of slice, signature `&[T]`

---

### Inference

* Inference engine of Rust also looks at how variable is used post initialization.
* No type annotation is needed.

```
fn x(){
  let a; // declare variable binding
  {
    let b = 81;
    a = 81 + 9;
  }
  println!("a: {}", a);
}
```

---

### Scope and Shadowing

* Variable bindings have scope and constrained in block.
* Variable shadowing is allowed.
* `'static` lifetime lasts for running program, maybe coerced for a shorter time

---

### Custom Types

* Mainly formed using 2 keywords, `struct` and `enum`.
* Constants can also be created via `const` and `static` keywords.

#### Struct

* 3 types: `tuples`, classic `C structs` and `unit` (useful for generics)

#### Enum

* allows creation of a type which may be one of a few different variants
* any variant valid as a struct is valid as enum
* [C like enum](./eg-enum-c-like.rs), [enum use](./eg-enum-use.rs)
* [testcase](./eg-enum-testcase-linked-list.rs) of enum making linked list

#### Constants

* 2 types of constants, the unchangeable `const` and possibly mutable `'static`
* can be decalred in any scope

---

#### [Rust's Standard Library](https://doc.rust-lang.org/std/)

It offers core types like [Vec<T>](https://doc.rust-lang.org/std/vec/index.html) and [Option<t>](https://doc.rust-lang.org/std/option/enum.Option.html), library defined [operations on language primitives](https://doc.rust-lang.org/std/#primitives), [standard macros](https://doc.rust-lang.org/std/#macros), [I/O](https://doc.rust-lang.org/std/io/index.html) and [multithreading](https://doc.rust-lang.org/std/thread/index.html) among [other things](https://doc.rust-lang.org/std/#what-is-in-the-standard-library-documentation).

---
