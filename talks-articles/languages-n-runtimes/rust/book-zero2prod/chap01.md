
## Chapter.1 Getting Started

* `rustup` manage setup of rust; `rustup update` to seamless update & `rustup toolchain list` for an overview

* `cargo new projectname` to create boilerplate

* `rust-analyzer` is LSP for it

* default linker is good; but `lld` (win/linux) & `zld` (mac) are faster OS specific alternatives

> can install the linker & add following to project's `Cargo.toml`; there could be WIP to use `lld` in Rust.. `mold` might be faster than `lld`

```
# .cargo/config.toml
# linux: `$pkgManager install clang lld`
# win: `cargo install -f cargo-binutils` & `rustup component add llvm-tools-preview`
# mac: `brew install clang michaeleisel/zld/zld`

[target.x86_64-unknown-linux-gnu]
rustflags = ["-C", "linker=clang", "-C", "link-arg=fuse-ld=lld"]

[target.x86_64-pc-windows-msvc]
rustflags = ["-C", "link-arg=fuse-ld=lld"]

[target.x86_64-aarch64-apple-darwin]
rustflags = ["-C", "link-arg=fuse-ld=/usr/local/bin/zld"]
```

* `cargo install cargo-watch` && `cargo watch -x check -x test -x run` like commands to check a file change then trigger testing & run of new code

* `cargo install cargo-tarpaulin` && `cargo tarpaulin --ignore-tests` to compute code coverage

* `rustup component add clippy` && `cargo clippy` as official rust linter

> * `cargo clippy -- -D warnings` to just show warnings
>
> * configure `clippy.toml` with `#![allow(clippy::lint_name)]` for project or `#[allow(clippy::lint_name)]` attribute in code to mute warnings

* `rustup component add rustfmt` && `cargo fmt` (or `cargo fmt -- --check`) to run code formatter

* `cargo install cargo-audit` && `cargo audit` to scan dependency tree

> `Rust Secure Code working group` maintains an Advisory Database; `cargo-deny` is another sub-command supporting vuln scanning


---

