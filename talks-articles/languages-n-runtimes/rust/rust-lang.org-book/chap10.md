
## Generic Types, Traits, and Lifetimes

[code sample](./code-samples/chap10.rs)

> if your code needs lot of Generics; probably it needs restructuring first into smaller pieces to resolve your real issue

> * Rust has Generics; similar to `Vec<T>`, `HashMap<K,V>` and `Result<T,E>`.
>
> * Traits are to define behavior in Generic way.
>
> * Lifetimes are variety of generics that give compiler information about how references relate to each other.


### Generic Data Types

* defining a function that uses generics, generics are placed in signature of function

> * example `type_name` gets passed any data type's list and returns string of type's name
>
> * if the function tries perform an operation (like mathematical comparisons) for which traits are not available on relevant types; compile error happens

* can define structs with one or more generic fields as `Date` in code-sample

> if a Struct gets defined for only one Generic type, all fields need to have same type once initialized

* stdlib provides `enum Option<T> { Some(T), None }` and `enum Result<T,E> { Ok(T), Err(E) }` as Enum defining generics

> example generic Enums are in code-sample as `Timestamp<T>`

* methods can be defined on structs and enums with Generics; as done for above samples in code

> also a cleaner example in sample over GuessMyType struct




#### Performance of Code using Generics





### Traits




### Validations



---
