
## Zig Language Reference

> [source: master/v0.11.0-dev.4282+0f21d3d4d](https://ziglang.org/documentation/master/)

* A general-purpose programming language & toolchain.

* StdLib doc [online](https://ziglang.org/documentation/master/std/#A;std).

* [Hello World Code](./hello-world/hello-world.zig). Has document comments on build/run.

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

* Primitive Types, check `fn sampleValues()` in [hello-values.zig](./hello-world/hello-values.zig)

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

#### Strings, check `fn sampleStrings()` in [hello-strings.zig](./hello-world/hello-strings.zig)

* String literals are constant single-item Pointers to null-terminated byte arrays. Type encodes length and fact of null-terminated, thus can be coerced to Slices & Null-Terminated Pointers. Dereferencing converts to Arrays.

> * Multi-lines have no escapes and start with `\\` token from next line, end of line is not included in literal which has just `;`.
> * Unicode code point literals are of type `comptime_int`.
> * Bytes are not modified by Compiler, though non-UTF-8 bytes can be embedded using `xNN` notation.
> * maximum valid Unicode points is `0x10ffff`

---

### Zig Test in [hello-tests.zig](./hello-world/hello-tests.zig)

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

### Variables in [hello-variables.zig](./hello-world/hello-variables.zig)

* Var identifiers never shadow identifiers from outer scope.

> * `@".."` syntax to be used for non-conventional Indentifier names; also for linking with external variables.

* Container level variables have static lifetime, are order independent & lazily analyzed. Init value is implicitly comptime.

* Local static variables can be used with containers within functions.

* `extern` structs are required when need layout of local struct to match layout of say C ABI. Because default structs can reorder struct fields, add hidden fields for safety/debug and perform other transformations. `@extern` bult-in function is available to, for linking against a variable from another object.

* `@export` fn or `export` keyword to make a variable available to other objects at linking, types shall be C ABI compatible.

* `comptime var idx: i32 = 0;` causes `idx` to enforce all loads & store happen at compile time; thus can't be used in an expression that also uses simple variable.

* `noreturn` type is for `break, continue, return, unreachable, while(true) {}`; allowing `const num = if (cond) 10 else return;`

---

### Integers & Floats in [hello-numbers.zig](./hello-world/hello-numbers.zig)

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

### Operator in [hello-operators.zig](./hello-world/hello-operators.zig)

* No operator overloading.

```
// Precedence
x() x[] x.y x.* x.?
a!b
x{}
!x -x -%x ~x &x ?x
* / % ** *% *| ||
+ - ++ +% -% +| -|
<< >> <<|
& ^ | orelse catch
== != < > <= >=
and
or
= *= *%= *|= /= %= += +%= +|= -= -%= -|= <<= <<|= >>= &= ^= |=
```

> * Wrapping & Saturating arithmetic, Bit operations are Int only.
> * Peer-type Resolution for add, subtract, multiply, divide.

* Fn like `@addWithOverflow(type, a, b, *result) <didFlow:bool>`, `@subWithOverflow(..)`, `@mulWithOverflow(..)`, `@divTrunc(..)`, `@divFloor(..)`, `@divExact(..)`, `@rem(..)`, `@mod(..)`.

* `a orlese b` (b could be of `noreturn` type) allows returning `a` if not-null else `b`. Also, `a.?` is same as `a orelse unreachable`.

---

### Arrays in [hello-arrays.zig](./hello-world/hello-arrays.zig)

* Array concat `++` & multiplication `**` are available only for comptime Arrays.

* `[N:x]T` describes array with sentinel element of value `x` at index corresponding to `len`

---

### Vectors in [hello-vectors.zig](./hello-world/hello-vectors.zig)

* Collection of bool/int/float/pointer, operated in parallel. Created via `@Vector(<len>, <type>){ ..values }`.

* Vectors supports built-in operators as underlying base types.

* `@splat` converts scalars to vectors. `@reduce` alongwith array indexing to convert vectors to scalars. Vectors support assignment to+from fixed-length arrays with comptime-known length.

* Rearranging within/between vectors, can use `@shuffle`/`@select`.

---

### Pointers in [hello-pointers.zig](./hello-world/hello-pointers.zig)

* `*T` single item pointer; supports deref `ptr.*`.

* `[*]T` many-item pointer to unknown item numbers; supports indexing & slice syntax. Pointer arithmetic is allowed as `ptr + x` & `ptr - x`.

> * `T` must have known size, so can't be `anyopaque` or other opaque types.
> * `*[N]T` pointer array to N items, same as single-item pointer to array.
> * `[]T` is a slice (fat pointer), contains a pointer of type `[*]T` & a length.

* `const ptr: *i32 = @ptrFromInt(0xdeadbee0);` to convert pointer from int address; & `@intFromPtr(ptr);` to get int adress from pointer.

* For MMIO (Memory Mapped Input/Output), using `volatile` ensures order as in source. `volatile` wouldn't be used for anything else than MMIO.

> * `@ptrCast` is unsafe op, to be only used when conversions ain't possible.
> * `alignment` of type is byte count for it when stored in memory; as power of 2 & based on CPU arch.
> * `@align` to check alignment & `@alignCast` to change pointer into more aligned pointer.

* AllowZero `var ptr: *allowzero i32 = @ptrFromInt(0);` lets pointer have address zero, only needed on freestanding OS target.

> * To represent null pointers, Optional Pointers shall be used. As `var ptr: ?*i32 = null;`.

---

### Slices in [hello-slices.zig](./hello-world/hello-slices.zig)

* slice is pointer & length. Array's length is comptime, Slice's length is runtime.

* Slices of Array (over Pointer) are preferred for operatios as they have bound checks.

---

### struct in [hello-structs.zig](./hello-world/hello-structs.zig)

* No guarantee of field order & size in default struct.

* Can have methods; only advantage is context namespacing.

* Can have declarations or zero fields as well. Can have default field values.

* `extern struct` only for in-mem layout matching C ABI target.

* `packed struct` have guaranteed in-mem layout (field order & size), so can be used in `@bitCast` or `@ptrCast` to reinterpret memory.

> * Address to a non-byte aligned field can be accessed; but not passed as normal pointer. This address would be same as other fields within their host integer. E.g. `samplePackedStruct()` in [hello-structs.zig](hello-world/hello-structs.zig)

* All structs are anonymous. Zig infers type name based on rules. If it's in `return` then named with fn name and param values serialized. Otherwise as `filename.fnName.__struct_ID`. A nested struct also gets parent name attached.

#### Tuples in [hello-tuples.zig](./hello-world/hello-tuples.zig)

* Anonymous structs created without field names are Tuples.

* Fields are implicitly named as indexed. Being integer, need to be accessed via `id.@"0"` or `id[0]`.

* Have a `len` field and allow `++`/`**` ops. Can be iterated via `inline for` (comptime).

---

### enum in [hello-enums.zig](./hello-world/hello-enums.zig)

* Can have ordinal values, with overrides. The values can be `switch` upon.

* Exported enums shall have C-ABI compatible enum with explicit tag like `const Xnum = enum(c_int) {ok, nok};`

* In a context block for an enum, fields can be directly notified with just dot.

* Non-exhaustive enums are available for cases to handle unseen or beyond values.

---

### union in [hello-unions.zig](./hello-world/hello-unions.zig)

* Bare unions (defining a set of possible fields, one active at a time) cannot be used to reinterpret memory. For guaranteed in-mem layout use `@ptrCast`/`extern union`/`packed union`.

> * Accessing non-active field is Undefined Behavior.

* Union can have methods like Struct. Can have anonymous usage as well like `var id: Id = .{.int = 101};`.

---

### opaque

* For a custom type declaration with non-zero unknown size & alignment. Has declarations same as struct.

> Used for type-safety while C code interaction, which don't expose struct details.

```
const SomeSharedStruct = opaque{
    fn process(self: *SomeSharedStruct) void {
        fetch_and_persist(self);
    }
};

extern fetch_and_persist(*SomeSharedStruct) callconv(.C) void;

test "opaque with declarations" {
    var some_obj: *SomeSharedStruct = undefined;
    some_obj.process();
}
```

---

### Blocks

* Are expressions with declaration scopes. If labeled, can return a value using `break :lbl value;`

```
...
var abc: i32 = 100;
const something = sth: {
    abc += 1;
    break :sth abc;
};
```

* No shadowing allowed. Also `{} == void{}`.

---

### switch in [hello-switch.zig](./hello-world/hello-switch.zig)

* All cases must be able to coerce to a common type. Can be used outside a function.

* Branches can't fallthrough; for that combine cases and use if. Can use block in value; in match for comptime available matchers.

* `else` clause can meet exhaustive list, all cases must be handled.

* `inline` switch prong to generate prong's body for each possible value.

* `inline else` are type-safe alternatives to `inline for`. With `inline for` comptime doesn't know all possible case have been handled. Allowed for tagged unions & exhaustive enums. For non-exhaustive enums with use of `_`.

> `inline for` block gets generated as series of `if` stmt relying on optimizer to convert to switch, could have `unreachable` flow. `inline else` fn are explicitly generated as desired switch handling all cases.

---

### while; for

> `while` & `for`, like `switch` allow `break`/`continue` flows.

* `while (cond) {..}` are simple statement; `while (i < 100) : ({i *= 2;}) {}` are loop continue statements... usage at [hello-while.zig](hello-world/hello-while.zig)

* `while (cond) : (updateCondVar) { if (condInner) { break retVal; } }` or `while (cond) : (updatecondVar) {...} else retVal;` or a mix of both allows return value from it.

* A labeled `blk: while () {.. {continue :blk;}}` while loop can be referenced with `break/continue` from within a nested loop.

* `while` can handle optional as `while (..) |val| {...}`; and handle error unions with `else |err| {..}` flow.

* `inline while` unrolls loop thus allowing compile time actions like using types as first class.

* Similar to `while`; `for` allows optional else, multi-list ranging if of same length, labeled break/continue from inner loop & `inline for`.. usage at [hello-for.zig](hello-world/hello-for.zig).

---

### if; defer

> `if` in [hello-if.zig](./hello-world/hello-if.zig)

* `if` expressions have 3 uses corresponding to 3 types (`bool`, `?T`, `anyerror!T`).

* If with `anyerror!?T`, the capture `|val|` is optional value `?T`.

> `defer` in [hello-defer.zig](./hello-world/hello-defer.zig)

* `defer` do Stack, as in Go. `errdefer` will only execute if scope returns an error.

---

### unreachable; noreturn

* `unreachable` are of `noreturn` type, yet can't be matched to those as reaching them in run-flow is compile error

> In `Debug/ReleaseSafe` mode `unreachable` emits `panic`. In `ReleaseFast/ReleaseSmall` mode, optimizer assumes it will never be hit to optimize rest of code.

* `noreturn` is also type of `break`, `continue`, `return` & `while(true) {}`.

---

### Functions, Errors, Optionals

> WIP

---

### Casting, 0-bit Types, Result Location Semantics, usingnamespace

---

### comptime, Assembly, Atomics, Async Functions, Builtin Functions

---

### Build Mode, Single Threaded Builds, Undefined Behavior, Memory

---

### Compile Variables, Root Source File, Zig Build system, C, WASM, Targets

---

### Style Guide, Source Encoding

---

### Keyword Reference, Appendix

> WIP

---
