
## Node.js Getting Started

> [source](https://nodejs.dev/en/learn/)

### Introduction to Node.js

* built atop V8 js engine, an OSS cross-platform JS runtime for backends written in C++; with JIT compilation

* runs in single process, provides async I/O primitives in stdlib to write flows in non-blocking paradigms

* example http server as below ran as `node script-filename.js`

> could be installed using toolks like `nvm` or your package manager; [guide](https://nodejs.dev/en/learn/how-to-install-nodejs/)

```
const http = require('http');

const server = http.createServer((req, res) => {
  res.statusCode = 200;
  res.setHeader('Content-Type', 'text/html');
  res.end('<html><body><h1>Running on <i>Node.js</i>.</h1></body></html>\n');
});

server.listen('3000', '127.0.0.1', () => {
  console.log(`Server running at http://127.0.0.1:3000/`);
});
```


### JavaScript recommended before deep dive

> just mentioning some key points to refresh context

#### Lexical Structure

* line comments `//`; block comments `/* ... */`

* `var`, `let`, `const`, expression statements, `do..while`, `continue`, `break`, `return`, `throw`, `debugger`, Class field dclarations, `import`, `export` require semicolon at end

#### Expressions and Operators

* `function*` defines a generator function expression, similarly `async function*` defines async generator function

* `?.` optional chaining gives `undefined` instead of error on nullish reference

* `await` to pause/resume async function & wait for promise's return

#### Data Types

* dynamic weak typing allowing implicit type conversions

* Primitives `TYPE, typeof` as `Null, object`, `Undefined, undefined`, `Boolean, boolean`, `Number, number`, `BigInt, bigint`, `String, string` & `Symbol, symbol`

> `null` is absence of object, `undefined` is absence of value itself

* there are `Date`, `Arrays`, Typed Arrays, Maps, Sets, WeakMaps, WeakSets, JSON & [many Global Objects via stdlib](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects)

#### Classes

* decalred and used as below; have Temporal Dead-Zone as `let`

```
// new class with constructor, static field, function and a getter
const SampleConstruct = class {
  constructor(base, times) {
    this.base = base;
    this.times = times;
  }

  static description = "Simply put, this generate math tables for a number."

  get message() {
    return {base: this.base, times: this.times};
  }

  answer(){
    return `${this.base} * ${this.times} = ${this.base * this.times}`;
  }
}

// class inherits above & has static function & generator
const TimesTable = class extends SampleConstruct {
  static result(sc){
    return sc.answer();
  }

  *getNext(){
    while (this.times <= 10) {
      yield `${this.base} * ${this.times} = ${this.base * (this.times++)}`;
    }
  }
}

const times2 = new TimesTable(2, 1);
console.log(TimesTable.description);
console.log(times2.message);
console.log(times2.answer());
console.log(TimesTable.result(times2));
console.log([...times2.getNext()]);
```

#### Variables

* `var x = 'val';` to declare a function/globally scoped variable with optional init value; processed before any code execution

> duplicate variables decalred using `var` will not err even in strict mode neither lose its value unless another assignment is done

* `let` decalres block scoped variables & only available beyond its declaration position; unlike `var` can't use `let` as body of a block like statement

> `var` & `let` also allows destructuring `var {x, y} = foo;`

* `const` creates block-scoped constants behaving much like `let`; needs to be initialized with final value

> `const` objects can't be overwritten but their keys don't don't inherit immutability; for that need `Object.freeze()`

#### Functions

* JS interpreter hoists Functions if declared as `function x() {...}` not with function expression as `const x = function() {...}`, so no TDZ if decalred

* if `return;` or no explicit statement then `undefined` is returned

* allows default value parameters, variadic parameters

* some interesting stdlib functions `isNaN`, `parseInt`, `parseFloat`, `escape`, `unescape`

#### `this` operator

* `this` refer execution context.. in non-strict mode it's reference to an object, in strict mode could be any value

#### Arrow Functions

* always anonymous shorter functions & non-binding of `this` like

```
const values = [1, 2, 3, 4, 5];
console.log( values.map((v) => v*2) );
```

* `(a, b, ...c) => expr`, `(a=1, b=2, c) => expr`, `({a,b} = {a: 1, b: 2}) => expr` are allowed

* allows `async` expressions as well by `async param => expression` syntax

#### Loops

* `for (let i = 0; i < LIMIT; i++) {...}` like FOR loops, or with *in* `for (const i in [1,2,3]) {...}` to iterate over property names & with *of* `for (cost i of [1,2,3]) {..}` iterate over values

* loops with `while (CONDITION) { STATEMENT; }` where if CONDITION is to be an assignment's truthyness it shall be grouped in addition parenthesis & add an explicit comparison to not match `null` or false-like assignment, also `do { ... } while(CONDITION);`

#### Scopes

* Global Scope for all code running in script mode

* Module Scope for code running in module mode

* Function Scope for those declared within a function

* `let` & `const` within curly braces, trigger Block scope.. not `var`

#### Arrays

* resizable, can contain mix of data types; not associative so arbitrary string indices are not available but positive integers

* `myarr[2]` gets coerced via `toString` to `myarr['2']`; provides `length`, `includes()`, `join()`, `slice()`, `indexOf()`, `push()`, `pop()`, `splice()`, `concat()`, `flat()`, `flatMap()`, `forEach()`, `reduce()`, `map()`, `reverse()`, `sort()` & more

#### Template Literals

* '`Template ${Literals}`' for String interpolation, Multi-line strings

#### Strict Mode

* since ES5 `function sample(){ 'use strict'; ... }` a way to opt-out of unsafe features, have more warnings & logical behavior

> like variables need to be declared; all nested functions declared at top-level consecutively; distinguishable param names; avoiding deprecated features; `this` being `undefined` in Browsers for nonmethod functions; not in functions with default parameters

#### ECMAScript 2015 (ES6) & beyond

* `node --v8-options | grep "in progress"` to list all V8 In Progress features if want to play around

* `node --harmony` to enable *staged* features only

* `node -p process.versions.v8` to find V8 version


### Asynchronous Programming

#### Asynchronous programming & callbacks

* Callbacks used to be main way to implement Async Functions; this have tendency to result in deeply nested callbacks for various real-world flows.

#### Timers

* `setTimeout(functionRef, delay)` executes a function on expiry of delay; `setTimeout(funcRef, delay, param1,...,paramN)` allows passing arguments

* it doesn't create pause before moving ahead; also it gets called from separate context from function where `setTimeout` was called thus `this` will default to `window` or `global` object

> * non-number delay values (like `1 second`) get coerced into number
> * some browser enforce minimum timeout delay in background tabs; there are other behavioral changes given the browser and tab state

* Can wrap it in a Promise to have async calls.

#### Promises

* Promises are foundation of modern JS async programming. It's an object returned by async function representing current state of function. Event handlers can be attached to returned Promise to be excuted when operation succeeded/failed. Like `fetch(..).then((r) => {..});` calls.

* Promises can be chained by nesting async calls' Promise; Errors can be managed as well.. as

```
// fetch and response.json() are async
fetch('https://example.com')
  .then((response) => response.json())
  .then((data) => {
    console.log(data[0].name);
  })
  .catch((error) => {
    console.error(`Could not get products: ${error}`);
  });
```

* Multiple promises can be combined with `Promise.all([...])`; with same then & catch flows.

#### Async & Await

* Async function always return a promise, if not explicitly a Promise.. it's wrapped in a promise. They can be `await`ed for a resolution.

```
function resolveAfter2Seconds() {
  return new Promise(resolve => {
    setTimeout(() => {
      resolve('some answer');
    }, 2000);
  });
}

async function asyncCall() {
  console.log('calling');
  const result = await resolveAfter2Seconds();
  console.log(result);
}

asyncCall();
```

#### Closures

* A function within scope of a main function sharing it's scope available to it.

* Used for custom function definitions to be crafted and returned to be used. Emulating private functions for a specific context.

* Creating a custom function within a loop doesn't work as they all will share the same lexical environment.. creates ruckus due to intermixes of `var` scopes.

#### The Event Loop

* Event Loop is JS runtime model, where events are collected & processed by executing queueud sub-tasks.

* Function calls form a stack of frames. Objects allocated in a heap. Runtimes uses a message queue.


### Difference between Node.js and Browser

* DOM elements & Web Platform APIs like Cookies don't exist in Node.js; and Node.js provided modules ain't available in browser

* Node.js supports CommonJS & ES module systems; so can use both `require()` & `import`.. while only `import` in Browser


### Node.js, difference between development & production

* `NODE_ENV=production` keeps logging to essential level, uses more caching to optimize performance; some libraries like `Pug` run in debug unless production mode is set

* available as `process.env.NODE_ENV`, to even have conditional flows for development


### Node.js with TypeScript

* TS is a JS superset adding new capabilities & safety measures, notably static types.

```
type Name = {
  firstname: string;
  lastname: string;
};

function fullname(n: Name): string {
  return `${n.firstname} ${n.lastname}`;
}

const jd: Name = {firstname: "John", lastname: "Doe"};
console.log(fullname(jd));
```

* `npm i -D typescript` to add TS support in your project and `npx tsc sample.ts` to run TS compiler

* `NestJS`, `NuxtJS`, `NextJS`, `TypeORM`, `Prisma`, `RxJS`, `AdonisJS`, `FoalTs` are few popular projects using TS


### Node.js with WebAssembly

* Node.JS provides necessary APIs via `WebAssembly` object to communicate with `.wasm` binary file.

* Sample usage as

```
// Assume dothis.wasm contains a function 'doX' with 2 int arguments

const fs = require('fs');
const wasmBuffer = fs.readFileSync('/path/to/dothis.wasm');
WebAssembly.instantiate(wasmBuffer).then(wasmModule => {
  const { doX } = wasmModule.instance.exports;
  const x = doX(1, 100);
  console.log(x);
});
```

* [Wasmtime](https://docs.wasmtime.dev/) utilizes [WASI](https://wasi.dev/) to help use OS functionality.

---

