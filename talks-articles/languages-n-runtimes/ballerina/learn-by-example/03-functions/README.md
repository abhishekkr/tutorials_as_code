
## Functions

### Function Pointers

* allows functions to be used as variables, name of function serves as pointer to itself

* example [function-pointer-example.bal](./function-pointer-example.bal) showcases function pointer passed as params and return types

---

### Anonymous Functions

* ballerina has a syntactic shortcut for inline anonymous functions, with signature or assigned to `var` defined

* example [anon-function-example.bal](./anon-function-example.bal) showcases type-d and `var` defined functions

* if anon function contains only return statement in body, can use `arrow function expression` as `function (string) returns (int) X = (s) => int.convert(s)`
> types of input parameters inferred from left side; arrow function is determined by evaluation of expression to right-side of `=>` symbol

---

### Closures

* an inner anon function with visibility to its enclosing scope and global scope

* example [closures-example.bal](./closures-example.bal) showcases basic, multi-level and function pointer using closures

---

### Functional Iteration

* ballerina has set of iterable operations on collections like array, maps, json, xml and others

* example [fn-iteration-example.bal](./fn-iteration-example.bal) showcases map, array types and usage of `map()/filter()/foreach()/average()/max()/min()/sum()`

---

### Functions as Entry Points

* any `public` function can act as an entrypoint; specified with `ballerina run`
> * entrypoint functions can be data-binding with 0 or more params
>
> * could also return a value; can print return value by `--printreturn` option to command

* example [fn-as-entry-example.bal](./fn-as-entry-example.bal) showcases public functions with default and rest params; to be run as following

```
± % ballerina run --printreturn /tmp/a.bal 3 double
doubled:
6

± % ballerina run --printreturn /tmp/a.bal:double 5
10%

± % ballerina run --printreturn /tmp/a.bal:runOp 4 5
error: custom operation error {}
	at ..<init>(a.bal:3)

± % ballerina run --printreturn /tmp/a.bal:runOp -op=add 4 5
9%

± % ballerina run --printreturn /tmp/a.bal:runOp -op=add 4 5 99 2
110
```

---
