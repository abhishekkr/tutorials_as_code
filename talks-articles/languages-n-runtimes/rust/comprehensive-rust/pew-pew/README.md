
## `pew-pew`'s crate based flows

### RustDoc

> `dash()` has internal doc with `//*` & `/*! .. */`

* `cargo doc --open` to generate and open in a browser; internally calls `rustdoc`

* does check for compile safety, so would error out in case of errors


### Option

An enum with `Some()` or `None` form value; good for return type.

> `char_index()` has Option sample code


### Result

|||r to Option, with `Ok()` or `Err()` form values.

> `get_env()` has Result sample code


### String

A growable UTF-8 encoded string.
String implements `Deref<Target = str>` which transparently gives it access to str's methods.

> `sample_string()` has String sample code


### Vector

Standard resizable heap-allocated buffer.

> `sample_vector()` has Vector sample code


### Hashmap

Hashmap with protection againsts HashDoS attacks (sending several request data to Server with same hash to spiral it into slow lookups).

> `sample_hashmap()` has Hashmap sample code

---
