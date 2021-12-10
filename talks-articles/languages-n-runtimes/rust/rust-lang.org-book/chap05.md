
## Using Structs to Structure Related Data

> `struct`, a custom data type that lets you name and package multiple related values

### Defining and Instantiating Structs

* declaration example `struct Person { firstname: String, lastname: String, id: u64}`

* instantiation example `let john = Person{ firstname: String::from("John"), lastname: String::from("Doe"), id: 1 };`

* dot notation to access as `john.id` or update field of mutable instance as `john.firstname = String::from("Jane");`

* borrowing & ownership fundamentals are same as before

> * if `struct` field passing partial values don't pass a String, they will be available afterwards as well

* when multiple (unnamed fields') struct with same primitive types; a function taking one as parameter can't take another as an argument.. as `struct X(i32); struct Y(i32); fn incrY(y: Y){ y + 1}` can't pass `X` type into `incrY`

* `Unit-Like Structs` don't have any fields as `()` tuple; useful when need implement a trait on a type that doesn't have any data.. decalred as `struct AlwaysGreater;`

* It's possible for Structs to have reference types; but this requires use of `lifetimes`, discussed in [Chapter.10](./chap10.md).


### An Example Program Using Structs

* As sample usage shown in [chap05.rs](./code-samples/chap05.rs) for above discussed implementation.

* Can add useful functionality with derived traits. As for `Point3D` struct.

> in default declaration, while `println!` it will error with `Point3D cannot be formatted using {}` as custom type via struct don't implement `std::fmt::Display` trait or `Debug` that gets printed using `{:?}`


* adding `#[derive(Debug)]` to struct helps with printing in debug mode

> * for larger structs we can use `{:#?}` instead of `{:?}`

* another way to print value using Debug format is `dbg!()` macro; this prints out to `stderr`

* there are multiple [derivable traits](https://doc.rust-lang.org/book/appendix-03-derivable-traits.html)


### Method Syntax

* Methods are function like constructs defined in context of a struct (or enum or trait object); their first param is always `self`

> as `display()` implemented for `Person`
>
> * in signature `&self` is short for `self: &Self`

* Methods can borrow immutably or mutably

* we use `&self` when we don't want to take ownership; just read

* `&mut self` as first param when method transforms self into something else

* can use these to define `getter` when want to declare fields as private and method public; thus enabling as read-only

> Rust has automatic referencing and dereferencing; calling methods is few of the places using it.
>
> When something like `ob.process()` is called; it automatically adds in `&`, `&mut` or `*` required.
>
> `ob.process(&oc);` works same as `(&ob).process(&oc)`

#### Methods with More Parameters

> check `same_firstname()` method on Person

#### Associated Functions

* all functions defined within same `impl` are associated by type

* can define associated functions that don't have `&self` as first param and thus not methods

> `String::from` is a similar associated function
>
> `Person::doe` is similar defined in code-sample

#### Multiple `impl` Blocks

> splitting impl into multiple blocks is valid

---
