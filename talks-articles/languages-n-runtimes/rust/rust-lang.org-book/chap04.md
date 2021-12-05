
## Understanding Ownership

> Ownership makes memory safety guarantees without needing GC

* Whether the cosntruct gets managed by Stack or Heap. A data with current owner going out of scope would be cleaned. Given slow push and access to heap, understanding ownership helps.

* Ownership Rules are each value has a variable called its `owner` (only one owner at a time). Owner going out of scope drops the value.

* like `loop { let v = 10; ...}`, here `v` remains valid for duration of each loop run

* `String` manage data in heap, can pull from literals using `let s = String::from("txt"); s.push_str(" msg");` making `s` to be `txt msg`


### Memory and Allocation

* Memory Allocator and Returning this memory to allocator when don't need. Traditionally GC takes care of this part or Devs explicitly free these. Pairing exactly one `allocate` with one `free` around scope of variable is the requirement.

* Rust does this internally around as soon `owner` gets decalred and goes out of scope. Rust calls `drop` automatically at end of scope. In C++ this pattern is RAII (Resource Acquisition Is Initialization).

* Behavior of code can be unexpected where multiple variables use data allocated on the heap.


#### Ways Variables and Data Interact: Move

* with simple fixed size values, when copies are made both get pushed onto Stack as with `let x = 1; let y = x;`

> `String` composed of Pointer to content, Lenght of it and Capacity it has at current allocation

* so if similar copy happens at String it would result into `double free` error due to pointer; after copy earlier string is not considered valid.. it's a `Move` instead of `shallow copy`

```
let s1 = String::from("W.I.P.");
let s2 = s1; // now s1 isn't accessible for 'value borrowed after move'
```


#### Ways Variables and Data Interact: Clone

* `str_var.clone()` makes a deep copy on heap data


#### Stack-Only Data: Copy

* since fix size types as Int/Float/Bool/Char/Tuples get on Stack; they get a quick copy not move

* Rust has a `Copy` trait; placed on any type is stored on *Stack*


#### Ownership and Functions

* semantics of passing a value to function is similar to that of assigning; it will move or copy based on traits

> example at `move_copy_trait_on_fn()`


#### Return Values and Scope

* return values can also transfer ownership, if doesn't have a Copy trait

> as in example `move_copy_trait_on_return()`

* multiple values can be returned using `tuple`



### References and Borrowing

* as in `take_string_by_ref()`, pass the String by reference to be able to use it after passing as arg

* when `&var` gets passed to a `called_function(&var)`; within the bounds of `called_function(x: &String) { x.len() }`  makes `x` go out of scope but since it doesn't have ownership of what it refers to.. so nothing happens

* action of creating a reference is termed **borrowing**

* a simple reference can't be mutated


#### Mutable References

* references can be mutated if mutable String get passed

* example `mutable_references()` shows passing and mutating a string

* mutable reference can't be borrowed more than once; Rust can prevent data races at compile time like this

> Rust wouldn't compile with data races

* internal new scopes can be created with curly braces and allow multiple manipulations of mutable references; just not simultaneous.. does cause cascading update

* borrowing a reference can't be done multiple combining mutable & immutable

> * can't have a mutable reference while having an immutable one; uses of immutable reference don't expect change
>
> * multiple immutable references are fine, as that doesn't change any value
>
> * a reference's scope starts from introduction to last usage; introducing mutable reference after last usage of immutable reference is acceptable
>
> * compiler ability to tell a reference is no longer used is NLL `Non-Lexical Lifetimes`


#### Dangling References

* Rust compiler guarantees of no dangling references

```
fn main(){ let ref_to_none = dangle(); }
fn dangle() -> &String{ let s = String::from("hey"); &s }
// this fails for return type containing a borrowed value;
// but no value to be borrowed from as s goes out of scope at end of function
```

> could be fixed with lifetime specifiers (covered in [Chapter10](./chap10.md))

* returning String directly would work here


#### The Rules of References

* References must always be Vaid

* At any time, you can have either one mutable reference or any count of immutable



### The Slice Type

* `slice` data type doesn't have ownership of content either; let you reference contiguous sequence of elements in a collection

> * in `fn first_token(s: &String) -> ?`, here to return first token from a text like returning index of end of word
> * using this is error prone, as mutable `txt` content would change and index remains from original content
> * this is also a pain to extract more than one token
>
> solution here is to use `slice`

* * `string slice` is reference to part of a String; and used as in `string_slice()`

> with string slice, updating mutable string gives compiler error if borrow of slice happens before
>
> * `first_word_slice()` present extracting first token with slice
>
> * `Deref` trait is attached to types with `deref` method that converts type provided into the type parameter needs


#### Other Slices

> other iterables can be sliced as well; as `[1, 2, 3]`


### Summary

* concept of ownership, borrowing and slices ensure meory safety

---
