
## Zig Language Reference

> [source v0.10.1](https://ziglang.org/documentation/0.10.1/)

* A general-purpose programming language & toolchain.

* StdLib doc [online](https://ziglang.org/documentation/master/std/#A;std).

* [Hello World Code](./hello-world.zig). Has document comments on build/run.

> * Source code are UTF-8 encoded files usually `*.zig` named.
> * Public `main` function is necessary for Zig compiler to infer start point. Unless a library.
> * Return types are explicit as `!void` for main (full form is `<error set type>!<any data type>`).
> * `std.log`'s functions or `std.debug.print` shall be used for stdout where failure to write don't need be handled.
> * Semicolons to end an expression/statement are necessary.

* No multiline comments. `//` are single line comments to be tokenized out of context.

> * `///` 3-slash are document comments and consecutive line multiple document comments are merged.
> * Document comments are only allowed at relevant locations
> * Container level documentation, or top-level doc comments are to begin with `//!`

### Values

* Primitive Types, check `fn sampleValues()` in [hello-world.zig](./hello-world.zig)

> * `i8, i16, i32, i64, i128, isize` for signed x-bit & lastly sized integeres; C Equivalents be like `int8_t, ..., intptr_t`
> * `u8, u16, u32, u64, u128, usize` for unsigned x-bit & lastly sized integeres; C Equivalents be like `uint8_t, ..., uintptr_t`
> * `c_short, c_int, c_long, c_longlong, c_longdouble` for ABI compatibility with C for eqivalent like `short, int, .., long double`
> * `c_ushort, c_uint, c_ulong, c_ulonglong` for ABI compatibility with C for unsigned eqivalents
> * `f16, f32, f64, f80, f128` for C Equivalents of `_Float16, float, double, double, _Float128`
> * `bool` for true/false; `anyopaque` for type-erased pointers void in C; `void` for `void{}`
> * `noreturn` type of `break, continue, return, unreachable, while(true){..}`
> * `type` is type of types
> * `anyerror` an error code
> * `comptime_int` for compile-time known int literals; `comptime_float` for compile time known floats
>
> * arbitrary bit-width integers are also available e.g. `i7` or `u7` with `65536` being max width allowed

* Primitive Values are `true`, `false`, `null` for allocating optional type to, and `undefined` to leave a value unspecified.

> * `var` need to be initialized at declaration, to leave them uninitialized and coerce-able.. assign `undefined` to them.

#### Strings, check `fn sampleStrings()` in [hello-world.zig](./hello-world.zig)

* String literals are constant single-item Pointers to null-terminated byte arrays. Type encodes length and fact of null-terminated, thus can be coerced to Slices & Null-Terminated Pointers. Dereferencing converts to Arrays.

> * Multi-lines have no escapes and start with `\\` token from next line, end of line is not included in literal which has just `;`.
> * Unicode code point literals are of type `comptime_int`.
> * Bytes are not modified by Compiler, though non-UTF-8 bytes can be embedded using `xNN` notation.
> * maximum valid Unicode points is `0x10ffff`



### Zig Test

...


---
