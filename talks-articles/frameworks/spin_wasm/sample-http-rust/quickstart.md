
## Spin QuickStart

> [setup Spin](https://spin.fermyon.dev/contributing) binary to be used

```
% spin templates install --git https://github.com/fermyon/spin

% mkdir sample-http-rust ; cd sample-http-rust

% spin new http-rust
  Project description: a sample Spin HTTP Component in Rust
  Project name: sample-http-rust
  HTTP base: /
  HTTP path: /demo

% tree .
[drwxr-xr-x abhishekkr 4.0K]  .
├── [-rw-r--r-- abhishekkr  682]  "Cargo.toml"
├── [-rw-r--r-- abhishekkr  414]  "spin.toml"
└── [drwxr-xr-x abhishekkr 4.0K]  "src"/
    └── [-rw-r--r-- abhishekkr  369]  "lib.rs"
```

* [spin.toml](sample-http-rust/spin.toml) defines `[[component]]` with `/demo` route trigger & source for `sample_http_rust.wasm`

* [src/lib.rs](sample-http-rust/src/lib.rs) has `#http_component` macro annotated over `fn sample_http_rust`

* `spin build` to compile source; `spin buil --up` to compile and start the app; `spin up` to just run

> * `http://127.0.0.1:3000/demo` as default serve
>
> * for detailed logs use `export RUST_LOG=spin=trace`

* can add [more components](https://spin.fermyon.dev/configuration) via `spin.toml`

* can have different component under different projects; with an external `spin.toml`; as under [sample-app](./sample-app)

---
