
## Common Collections

* unlike array/tuples; collection point to data on Heap.. so can grow/shrink during run

> `vector` allows store values next to each other;
> `string` is char collection;
> `hash map` let's associate value with keys


### Storing Lists of Values with Vectors

> sample code under [chap08.rs](code-samples/chap08.rs) branched from `use_vector()` function

* `Vec<T>` vector allow store more than one value of same type in single data structure with values next to each other in memory

* creating empty vector as `let v: Vec<i32> = Vec::new();`

* can infer type if created with data using `vec!` macro as `let v = vec![11, 21, 31];`

* Updating a Vector; need to be mutable and type get inferred by pushed value

```
let mut v = Vec::new();

v.push(51);
v.push(101);
```

* when vector goes out of scope, all of its content gets dropped too; which might seem complicated when introducing references to elements of vector

* 2 ways to get vector's content; out of bounds index could be managed by `match`

```
let first: &i32 = &v[0];

match v.get(0) {
  Some(fst) => println!("first element: {}", fst),
  None => println!("no first element, empty"),
}
```

* with valid reference of vector, borrow-checker enforces ownership & borrowing rules.. so op like holding immutable reference to an element and try adding an element to end wouldn't work if the reference is accessed later

* iterate over Vector with `for elem in &vec {..}`

#### Using Enum for Multiple Types in Vector

* Vectors can only store values of same type; to store list of different kind of items Enums can be used

> this allows it to identify exactly how much heap needed for each element


### Storing UTF-8 Encoded Text with Strings

* strings implemented as byte collection; only one string type in rust core as `str` usually seen in borrowed form as `&str`

* string slices are ref to UTF-8 encoded str data stored elsewhere; String literals stored in binary are `string slices`

* stdlib also has few string types as `OsString/OsStr/CString/CStr` to represent different encodings

* many operations are same as `Vec<T>`; like `String::new()` creating empty string

* `String::from("..")` and `"..".to_string()` have same result of string create

* `String` can grow and change; like `Vec<T>` contents

* `str_var.push_str(str_2)` to append string and `str_var.push(char_2)` to append a single character; also `str_2` ownership isn't passed and stays available

* `let sx12 = sx1 + &sx2;` concats 2 strings; `sx1` is not valid after this since passed as self and `sx2` ref gets passed

> reference of second string gets added to first; compiler can coerce `&str` to a `String` that it does here

* can just `let sx12 = format("{}{}", sx1, sx2);` using `format!` macro for concat

* referencing char/slice in a string is not alowed by Indexes; string is a *Vector* wrapper over `Vec<u8>`

> it being vector; Unicode scalar value in string could take 1byte or 2bytes depending if it's english or cyrillic or some other char-set... thus making index correlation tricky
>
> since with this setup, indexing might not be a constant time lookup.. so Rust avoids it altogether

* can indicate slicing with use of range in `[]` as other languages like `let s = &sx[1..2];`

> string slices can panic the program if slice contains an encoding where bytes of a char are spread across more than one... as in comment under sample code for `use_utf8txt_slice()`

* can use `for .. in s.chars()` construct to iterate over string; `s.bytes()` are also available


### Storing Keys with Associated Values in Hash Maps

* type `HashMap<K,V>` stores k/v dict via hashing function of keys in memory; get stored on Heap

* can create empty map as `let mut kv = HashMap::new(); kv.insert("x".to_string(), 101);` and add k/v with `insert()`

* another way to create is have a vector of key and value each say `k` and `v`; then `let mut kv: HashMap<_,_> = k.into_iter().zip(v.into_iter()).collect()`

* any variables used to insert within hash-map gets borrowed and are invalid past that

* if references are passed to hash-map; the values wouldn't be moved but shall remain valid at least as long as hash-map

* `kv.get(&key)` fetchs the associated `Some(value)` or `None` of key is missing

* `for (k,v) in &kv {...}` could be used to iterate

* updating key in hashmap could cater to overwrite, write if not exists or merge

> * `insert()` again to overwrite
>
> * `kv.entry(k).or_insert(v);` would insert if no value exists; here `entry()` returns an *Enum* called `Entry` representing value that might be associated with it
>
> * updating a value based on old could be used as for `wordcount` like use-cases shown in `use_hm_update_wordcount()`

* default `HashMap` uses a hashing function called `SipHash` (able to resist DoS); can use even faster hashing function if can ignore security aspect for that flow

> a `hasher` is a type the implement `BuildHasher` trait

---
