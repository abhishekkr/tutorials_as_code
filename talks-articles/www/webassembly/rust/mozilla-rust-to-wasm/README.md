
## Compiling from Rust to WebAssembly

> [source](https://developer.mozilla.org/en-US/docs/WebAssembly/Rust_to_wasm)

* here npm package `wasm-pack` is used

### Environment setup

* install `rust`

* install `wasm-pack`, providing right tooling for npm package

```
cargo install wasm-pack
```

* create new rust library project for wasm `cargo new --lib hello-wasm`

* add following to `./hello-wasm/src/lib.rs`

```
extern crate wasm_bindgen;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern {
    pub fn alert(s: &str);
}

#[wasm_bindgen]
pub fn greet(name: &str) {
    alert(&format!("Hello, {}!", name));
}
```

* an external crate `wasm_bindgen` gets used here, provides a bridge between JS and Rust

* `extern` field with `#[wasm_bindgen]` denotes what can be called externally

* in `greet` function, JS provided `alert` function gets used

> this function also needs attribute `#[wasm_bindgen]` as need to callable from outside, JS in this case

* crate type `cdylib` tells what version of package is needed


### Building WASM Package

```
wasm-pack build --scope hellowasm
```

* it compiles rust to WASM

* run `wasm_bindgen` on WASM, generating JS file to wrap it up

* `wasm` module would require correct MIME type, done via `npm install ; npm run serve`

* open `index.html` via `http://localhost:8080/`, `index.js` uses JS generated for WASM

---
