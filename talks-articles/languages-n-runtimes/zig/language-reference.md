
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

---

### Zig Test

* `test "<test-name>" { test_block() }` declarations to ensure behavior meets expectations; ran via `zig test <source-file[s]>` which builds and runs executable using Stdlib's default test runner as **main** entry point. Test declarations are omitted from build unless `zig test` tool builds them.

> * (convention) UnNamed tests should only invoke other tests, these can't be filtered.
> * Implicit return type of tests is `anyerror!void` error-union type.
> * Tests can be in same source file as tested functionality, or separate. Tests are top-level declarations, thus order independent.

* Tests can be Nested within Blocks, but unless referenced in a top-level test.. will not be resolved.

> * all container tests can be referenced via `std.testing.refAllDecls(@This())` in a top-level test
> * `test "demo container.X"` & `test "demo container.Y"` can be referenced individually as in comments
> * to run tests of an imported file, reference as well `_ = @import("other_test.zig");`

* Can skip tests programmatically via `return error.SkipZigTest;`. Default test runner skip test with `suspend point` while test running using default blocking IO mode. Evented IO mode could be enabled via `--test-evented-io`

* Code allocating memory using `std.testing.allocator`, default test runner to report leaks found.

* `std.builtin.is_test` checks if build is in test mode or run

* `std.log.warn` lets you log statements to stderr for testing namespace

* Including `std.testing.expect`, we also have `expectEqual`, `expectDeepEqual` for container/custom types, `expectEqualSlices`, `expectEqualStrings`, `expectError`, `expectStringStartsWith`, `expectStringEndsWith`, etc.

---

### Variables

* Var identifiers never shadow identifiers from outer scope.

> * `@".."` syntax to be used for non-conventional Indentifier names; also for linking with external variables.

* Container level variables have static lifetime, are order independent & lazily analyzed. Init value is implicitly comptime.

* Local static variables can be used with containers within functions.

* `extern` structs are required when need layout of local struct to match layout of say C ABI. Because default structs can reorder struct fields, add hidden fields for safety/debug and perform other transformations. `@extern` bult-in function is available to, for linking against a variable from another object.

* `@export` fn or `export` keyword to make a variable available to other objects at linking, types shall be C ABI compatible.

* `comptime var idx: i32 = 0;` causes `idx` to enforce all loads & store happen at compile time; thus can't be used in an expression that also uses simple variable.

---

### Integers & Floats

* Integers have no size limitation. If value is not comptime, then it's vulnerable to Int Overflow & other compiler errors.

> * `+%` & `-%` perform wrapping arithmetic; `+|` & `-|` perform saturating arithmetic.

* `comptime_float` have same precision as `f128`; and floats coerce to any numeric type if they don't have fractional component.

* `NaN`, infinity, negative infinity available via `std.math`.

#### Float Operations, Optimized

* Default is `Strict` mode, can switch to `Optimized` per-block basis using `@setFloatMode(.Optimized);`.

> When set in an object file, enforces Floating Point Operations to optimize values for accuracy. If called function is in same file, optimizer figures out itself.

```
// floater_obj.zig
// built via: zig build-obj floater_obj.zig -O ReleaseFast
const std = @import("std");
const big = @as(f64, 1 << 40);

export fn floater_strict(x: f64) f64 {
    return x * big;
}

export fn floater_optimized(x: f64) f64 {
    @setFloatMode(.Optimized);
    return x * big;
}
```

```
// floater_mode.zig
// built via: zig build-exe floater_mode.zig floater_obj.o
const print = @import("std").debug.print;

extern fn floater_strict(x: f64) f64;
extern fn floater_optimized(x: f64) f64;

pub fn main() void {
    const x = 0.001;
    print("optimized = {}\n", .{floater_optimized(x)});
    print("strict = {}\n", .{floater_optimized(x)});
}
```


---

### ...

---
