
## Variable Bindings

* Safety via static typing. Values bound to variables using `let` binding.

* Several mechanisms to change or define the type of primitive and user defined types.

* need explicitly decalre variable to be mutable using `mut`, also immutable vars can be shadowed; [example](./eg-mutable-scope-shadow.rs)

---

## Types

* Can define and change primitive or user defined types.

### Casting

* There is no implicit type conversion.
* Explicit conversion can be performed using `as`

### Literals

* numeric literals can be type annotated by adding type as suffix, like `10i32`
* unsuffixed numeric literals will depend on how they are used
* can assign special chars in string with hex code or unicode escaped with `\`
* for a string with just normal chars, byte strings can be used as `str`/`String` must be valid UTF-8

### Aliasing

* a new name to an existing type, must be CamelCase

#### Attributes are discussed in detail [here](../c12-attributes-N-13generics-N-14scoping/README.md).

### Inference

Type inference doesn't just look at `r-value` but also at its usage afterwards.

---

## Conversion

Rust addresses conversion between types by the use of [traits](../c15-traits/README.md).

* Generic conversions will use `From` and `Into` traits. If can convert type A from type B, then can convert type B into type A.

* `From` trait allows type to define conversion from other type.

* `Into` is reciprocal of `From` trait.

---
