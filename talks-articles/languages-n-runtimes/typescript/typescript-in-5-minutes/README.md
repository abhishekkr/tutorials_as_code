
## TypeScript in 5 minutes

[source](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)


### Building your first TypeScript file


```greeter.ts
function greeter(myname) {
  return "Hey " + myname;
}

let myuser = "Jane Doe";

document.body.innerHTML = greeter(myuser);
```

* saving above as [greeter.ts](./greeter.ts) and compiling using `tsc greeter.ts` gets javascript [greeter.js](./greeter.js) as below

```
function greeter(myname) {
    return "Hey " + myname;
}
var myuser = "Jane Doe";
document.body.innerHTML = greeter(myuser);
```


### Type annotations

* can add a type annotation for `myname` as below

```
function greeter(myname: string) {
  return "Hey " + myname;
}
```

* now changing input to greeter to list will cause compile error

```code
let myuser = ['some', 'names']
document.body.innerHTML = greeter(myuser);
```

```error
greeter.ts:7:35 - error TS2345: Argument of type 'string[]' is not assignable to parameter of type 'string'.

7 document.body.innerHTML = greeter(myuser);
```

* similarly calling without params would give error as

```
± % tsc greeter.ts                                                    !30179
greeter.ts:5:27 - error TS2554: Expected 1 arguments, but got 0.

5 document.body.innerHTML = greeter();
                            ~~~~~~~~~

  greeter.ts:1:18
    1 function greeter(myname: string) {
                       ~~~~~~~~~~~~~~
    An argument for 'myname' was not provided.


Found 1 error.
```

> TypeScript offers static analysis based on both code structure and type annotations.


### Interfaces

* can use an interface describing objects without an explicit implements clause, TypeScript types are compatible if their internal structure is compatible

```
interface PeopleName {
  firstName:  string;
  lastName:   string;
  title:      string;
}

function greeter(myname: PeopleName) {
  return "Heya " + myname.title +  " " + myname.firstName + " " + myname.lastName;
}

let someName = { firstName: "James", lastName: "Bond", title: "Mr." };

document.body.innerHTML = greeter(someName);
```


### Classes

* TypeScript supports class-based OOP; classes and interfaces play well together

* use of `public` on arguments to constructor is a shorthand that allows use to automatically create properties with that name

```
class Person {
  title: string;
  fullName: string;
  constructor(public titl: string, public firstName: string, public midName: string, public lastName: string) {
    this.title = titl;
    this.fullName = firstName + " " + midName + " " + lastName;
  }
}
interface PeopleName {
  firstName:  string;
  lastName:   string;
  title:      string;
}

function greeter(myname: PeopleName) {
  return "Heya " + myname.title +  " " + myname.firstName + " " + myname.lastName;
}

let someone = new Person("Mr.", "James", "L.", "Bond");

document.body.innerHTML = greeter(someone);
```


### Running TypeScript web app

* just source the compiled javascript

---
