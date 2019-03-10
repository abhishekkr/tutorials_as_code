
## Basic Types

[source](https://www.typescriptlang.org/docs/handbook/basic-types.html)


* TypeScript supports much same type as expected in javascript, with enumeration type as well.

### Boolean

* most basic datatype is true/false `let isSuccessful: boolean = true;`


### Number

* all numbers are floating point values

* hexadecimal, decimal, octal and binary are also supported

```
let fpN: number = 10.1;
let hexN: number = 0xf00d;
let decN: number = 7;
let octN: number = 0o754;
let binN: number = 0b1001;
```


### String

* for tectual datatypes, uses double or single quoted data

```
let color: string = "orange";
color = 'black';
```

* can also use `template strings`, can span multiple lines and have embedded expressions; surrounded by backtick with embedded expressions as `${ expr }`

```
let fullName: string = `Jame Bond`;
let house_num: number = 101;

let sentence: string = `Hey, ${ fullName }

Where is House# ${ house_num + 10 }
`
```


### Array

* can be written in 2 ways, using `$elem_type[]` or `Array<$elem_type>`

```
let numList: number[] = [11, 22, 33];

let strList: Array<string> = ["a", "b", "c"];
```


### Tuple

* express array where type of fixed-number elements, all element types need not be same

```
let idTuple: [number, string];
idTuple = [101, "Some User"]; // correct
idTuple = ["Some User", 101]; // error

console.log(idTuple[0].substr(1)); // correct
console.log(idTuple[1].substr(1)); // error, number doesn't have substr

// accessing element outside set of known indices, Union type is used
idTuple[4] = "maybe"; // correct; string can be 'number | string'
console.log(x[5].toString()); // correct; toString available wit number and string

idTuple[6] = true; // error
```


### Enum

* like other languages, friendlier names to numerical values

```
enum Color {Red, Green, Blue}
let c: Color = Color.Green;
```

* by default members start at `0`, can change it by manual config for start or all

```
enum ColorCM {Cyan = 3, Magenta}

enum ColorYK {Yellow = 5, Key = 6}
```

* can also infer name from numeric value

```
let colorName: string = Color[2];
console.log(colorName); // Blue
```


### Any

* when values may come from dynamic content; opt-out of type checking

```
let someInput: any = 4;
someInput = "four";
someInput = true;
someInput.ifItExists(); // correct, method might exist at runtime
someInput.toFixed(); // correct, method exists (but compiler doesn't check)
```

* can expect `Object` similarly, but can only assign values to them and not call methods on them

```
let onlyVal: Object = 4;
onlyVal.toFixed(); // error, not on Object
```

* `any` can also be used in cohesion, like array of mixed types

```
let lst: any[] = [1, "world", false];
lst[1] = 100;
```


### Void

* opposite of `any`, absence of any type at all; can be used as return type

```
function logInfo(msg): void {
  console.log("[info] " + msg);
}
```

* a var of this type can only have `undefined` or `null`


### Null and Undefined

* `undefined` and `null` have types `undefined` and `null` respectively

```
let u: undefined = undefined;
let n: null      = null;
```

* not useful on their own, subtypes of all other types so can be assigned to any of them

* when using `--strictNullChecks` flag, these are only assignable to `void` and respective types

* can use union types, if wanna pass a type or these as `string | null | undefined`

> use `--strictNullChecks` when possible to keep safe


### Never

* `never` type is for values that never occur

* example `never` as return type of function expression or arrow function expr that throws an exception; or one that never returns

* `never` is a subtype and assignable to all, so there is no subtype to `never` except `never`

```
// function returning never must have unreachable end
function error(msg: string) never {
  throw new Error(msg);
}

// inferred return type is never
function fatal() {
  return error("Something unrecoverable just happened");
}

// or just unreachable end point
function runAlways(): never {
  while (true) {
  }
}
```


### Object

* represent non-primitive type; anything not `number|string|boolean|symbol|null|undefined`

* APIs like `Object.create` can be represented

```
// example
declare function create(o: object | null): void;

create({prop: 0}); // correct
create(null); // correct

create(42); // error
create("myname"); // error
create(true); // error
create(undefined); // error
```


### Type assertions

* if using a more undecided type, to be more specific `type assertions` can be used

* a way to tell compiler, it's like type-cast without any restructuring of data or checks

* have 2 forms, `angle-bracket` and `as` syntax; only `as` works with `JSX`

```
let aVal: any = "some value";

// angle-bracket
let aLen: number = (<string>aVal).length;

// as syntax
let bLen: number = (aVal as string).length;
```


### A note about `let`

* `let` gets used instead of `var`; it's a newer JS construct that TypeScript makes available

---
---
