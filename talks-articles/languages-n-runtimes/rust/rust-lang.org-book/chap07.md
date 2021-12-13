
## Managing Growing Projects with Packages, Crates and Module

* can organize code into multiple modules in multiple files; a package can contain multiple binary crates and optionally one library `crate`

* as project grows, code can be extracted into separate creates becoming external dependencies

> for a very large project of a set of interrelated packages evolving together, `Cargo` provides `workspaces`

* encapsulation with required details expose as public; can manage scopes (tools are available to resolve name conflicts)

> module systems features used for these are
>
> * `Packages`, a cargo feature to build/test/share crates
>
> * `Creates`, a tree of modules producing a lib or binary
>
> * `Modules` and `use`, let you control organization/scope/privacy of paths
>
> * `Paths`, way of naming item as struct/fn/module



### Package and Crates

* `crate root` is a source file that Rust compiler starts from and makes root module

* `package` is one or more crates providing a set of functionality; a package contains `Cargo.toml` file

> `package` can contain at most one library crate, as many binary crates as required.. but at least one `crate`

```
// package created with
cargo new chap07-pkg
```

* creates `Cargo.toml`

* `src/main.rs` is by convention the crate root of binary crate with the same name as package

* if has `src/lib.rs`, it contains library crate with same name as package.. can be just one

* for multiple binary crate, place files in `src/bin` directory with each file a separate binary crate

* `cargo run` to run crate binary; `cargo build` to compile and prepare crate lib/binary



### Defining Modules to Control Scope and Privacy

* creating a library crate at structure level only, `cargo new --lib chap07-restaurant` to organize customer area and kitchen separately

* define module `customer_area` at [src/lib.rs](code-samples/chap07-restaurant/src/lib.rs) using `mod` keyword; nested with `hosting` & `serving` modules

> `src/main.rs` & `src/lib.rs`, either of these form a module under module named `crate` at **root** of crate's module structure



### Path for Referring to an Item in the Module Tree

* can mention absolute paths from a root `crate`, or relative from current module via `self`/`super` or id in current module

> absolute and relative paths have identifiers separated by `::`

* as in `pub fn eat_at_restaurant()` could call `crate::hosting::take_registration()`

> * better using absolute paths as they work seamless while refactoring
>
> * it will be erroneous if used paths are not public; items in parent module can't use private items from nested modules

* can use `super::<for paths that begin in parent module>`; as for `show_menu()` in the example

* a public `struct` can be set/get values; but if fields not set public theu aren't accessible beyond `impl`

* although all variants of a public `enum`, are public



### Bringing Paths into Scope with the `use` Keyword

* there is a way to not use absolute or relative long paths; can make them local with `use` at path once

* example of `use crate::customer_area::serving;` for `serving::take_order();`

> with `use crate::customer_area::serving::take_order;` can call just `take_order();`; but that's not idiomatic due to ambiguity of parent

* bringing `structs` or `enums` or other types; it's idiomatic to bring inherit them directly so can be called without parent; just due to heavy usage no readability reasoning

> since even this could cause clash, can prefer not to use it

* for clashing child names to be used as inference point; `as` keyword could be used as

```
use this::is::Utils;
use this::is_also::Utils as AlsoUtils;
```

* re-exporting; to enable the `use this::is::Utils` allowing any code using this code as `Utils`; can `pub use this::is::Utils`

* using any other external libraries; similar to stdlib crate.. `use <crate-name>::<module...>`; as for `rand` in [chap02 example](code-samples/chap02_guessing_game)

* can bundle nested use paths from common parents in curly as `use std::{cmp::Ordering, io::Write};`; if it's self and a child then like `use std::io::{self, Write, Read}`

* to bring all public items into scope, use `*` Glob Operator as `use std::collections::*;`; often used for testing



### Separating Modules into Different Files

* a separate module like `src/customer.rs` with public constructs could be declared as

> * `mod <filename>;` to load contents of module
> * then `pub use crate::<filename>::<module-name..>` to be used

* even if we create a dir as `customer` and filename with modulename, the loading hierarchy remains same

---
