
## Introducing Fermyon's Spin

> [source](https://www.fermyon.com/blog/introducing-spin) | [github](https://github.com/fermyon/spin) | [discord](https://discord.gg/FKFe5mthQB) | [twitter](https://twitter.com/fermyontech)

### WebAssembly Framework

* think of WebAssembly as a [compile target](https://www.fermyon.com/blog/how-to-think-about-wasm); a standardized bytecode formart to execute programs

> * [AssemblyScript](https://www.assemblyscript.org/), [Grain](https://grain-lang.org/) like languages have been designed specifically for compilation to WASM; using [binaryen](https://github.com/WebAssembly/binaryen)
>
> * JS-in-WASM allows sandboxing one Javascript program from another
>
> * WASI (WebAssembly System Interface) is WIP to provide features like filesystem, db, network connection while upholding reliability
>
> * [WAMR](https://github.com/bytecodealliance/wasm-micro-runtime) is a micro lightweight standalone runtime for WebAssembly; [Wasmtime](https://wasmtime.dev/) as well

* Spin is a framework for web apps, microservices & other server like applications. Rust & Go both have robust Spin support, can also use Python/Ruby/AssemblyScript/Grain/C/C++ and others.

> * Fermyon already runs Spin in Production using a WebAssembly based [Bartholomew CMS](https://www.fermyon.com/blog/introducing-bartholomew), built as a [Wagi](https://github.com/deislabs/wagi) app i.e. WebAssembly Gateway Interface
>
> * [Hippo](https://github.com/deislabs/hippo) is an easy way to deploy & serve WebAssembly; for similar purpose there is a Kubernetes' Rust Kubelete [Krustlet](https://github.com/krustlet/krustlet)


### Spin as a Foundation

* Spin provides a framework to easily expose WASM features while creating FaaS style design

* After Wagi, WASM has many updates to pave path for Spin. WASI & WIT (WASM Interface, an experimental textual format defining interfaces) make Spin easy to manage goals like [Security](https://www.fermyon.com/blog/log4sh-and-webassembly)

* Samples in Rust & Go for a simple logic of `HTTP 200 Spin It!` using common HTTP Request/Response model

```rust
#[http_component]
fn hello_world(_req: Request) -> Result<Response> {
    Ok(Response::builder()
        .status(200)
        .body(Some("Spin It!".into()))?)
}
```

```go
func main() {
 spin.HandleRequest(func(w http.ResponseWriter, r *http.Request) {
  fmt.Fprintln(w, "Spin It!")
 })
}
```

> low-level duties like setting up SSL, handling iterrupts or managing network connection gets delegate to Spin; you just `spin up` to run

* Currently (May/2022) it only supports top-level HTTP request/response for few languages. But it ships with 100% Wagi implementation, so almost all WASI supporting languages could be used.

* `spin_sdk` provides support for more components like *Redis*; more are in process of release

* Spin support for [Bindle packaging](https://github.com/deislabs/bindle), [WebAssembly Component Model](https://github.com/WebAssembly/component-model) and [WIT](https://github.com/bytecodealliance/wit-bindgen/blob/main/WIT.md).

> [Spin FileServer](https://github.com/fermyon/spin-fileserver) shows a production-ready spin app

---
