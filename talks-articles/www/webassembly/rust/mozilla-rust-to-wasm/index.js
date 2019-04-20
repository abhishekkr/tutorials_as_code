const js = import("./hello-wasm/pkg/hello_wasm.js");
js.then(js => {
  js.greet("WebAssembly");
});
