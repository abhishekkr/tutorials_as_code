
## Ballerina Basics

### Modules

* program files have extension `.bal` and filenames got no semantics

* source files can be grouped in modules
> module name shall only have alphanumerics, underscore, periods and max len of 256

*  `import ballerina/math;` to import module as is and `import ballerina/math as ganit` to declare explcit alias
> use `:` to infer public symbols of a method as `math:PI` or `io:println`

---

### Variables and Var

* can decalre variables using `var` keyword for inferential typing or directly using type keyword

```
import ballerina/io;

int moduleVar = 10;

public int publicModuleVar = 101;

const int constantModuleVar = 200;

public final int frozenPublicModuleVar = 201;

public function main() {
  var localSomeint = 100;
  string localSomestr = "ABC";
  io:println(localSomeint, " | ", localSomestr, " | ",
    moduleVar, " | ", publicModuleVar, " | ",
    constantModuleVar, " | ", frozenPublicModuleVar);
}
```

---

### Functions

* same as other languages defined using `function` keyword, scope can be made public with `public function`


* `public function main() {...}` is default entry point to ballerina programs

---

### Defaultable Parameters

* functions can have optional parameters with default values, to override need to be passed as `param=val` pairs

* position of default params doesn't matter

```
import ballerina/io;

function fnSome(int uid, string fname="John", string lname="Doe") {
  io:println("uid: ", uid, " | name: ", fname, " ", lname);
}

public function main() {
  fnSome(1);
  fnSome(2, fname="Jack");
  fnSome(3, lname="D.");
  fnSome(4, fname="Paula", lname="Ernst");
  fnSome(5, lname="E.", fname="Jane");
}
```

---

### Rest Parameters

* ballerina allows at most 1 rest param which acts like array of same type, like parameter `friends` in example below

```
import ballerina/io;

function fnSome(int uid, string name="John", string... friends) {
  io:println("uid: ", uid, " | name: ", name, " has ", friends.length(), " friends");
  io:println(friends);
}

public function main() {
  fnSome(1);
  fnSome(2, name="Jack");
  fnSome(3, "Alice");
  fnSome(4, "Alice", "Bob");
  fnSome(5, name="Jack", "Alice", "Bob");
  fnSome(6, "Alice", name="Jill", "Bob");

  string[] friends = ["Alice", "Bob", "Eve"];
  fnSome(7, name="Dory", ...friends);
}
```

---

### Documentation

* allows to document ballerina constructs defined in modules which can later utilized by `Docerina` to generate API doc for the same modules

* there will be runtime warnings if you add wrong documentation, say `return` field for a document that doesn't return anything

* example at [documentation-example.bal](./documentation-example.bal)

---
