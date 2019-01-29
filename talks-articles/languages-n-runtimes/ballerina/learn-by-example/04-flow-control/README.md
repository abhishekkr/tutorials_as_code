
## Flow Control

### If/Else

* example [flow-control-example.bal](./flow-control-example.bal) showcases `if`, `if-else`, `if-else-if-else`

```
if (xyz == 100) {
  io:println("simple check");
}

if (xyz == 100) {
  io:println("eq 100");
} else if (xyz > 1000) {
  io:println("big enough");
} else {
  io:println("other case");
}
```

---

### While

* example [flow-control-example.bal](./flow-control-example.bal) showcases `while`

```
while (xyz < 5) {
  xyz = xyz + 1;
  if (xyz == 3) {
    continue;
  }
  if (xyz == 4) {
    break;
  }
  io:println(xyz);
}
```

---

### Foreach

* looping construct for iterable collections

* example [foreach-example.bal](./foreach-example.bal) to showcase foreach over json, xml and range

```
// simple example over array
io:println("iterate over string:");
foreach var person in ["alice", "bob", "eve"] {
  io:println(person);
}

// simple example over map
io:println("iterate over map:");
map <string> ppl = {a: "alice", b: "bob"};
foreach var (k, v) in ppl {
  io:println(k, ": ", v);
}
```

> * iteration over `json` ain't supported, need to be converted to `map`
>
> * `json` in ballerina is a union type for `()` or `null|int|float|decimal|string|json[]|map`
>
> * if type cannot be inferred, then type is `anydata`
>
> * to iterate over JSON array, need to cast into array of json `json[]`

* iteration over `xml` shown in [example](./flow-control-example.bal) fetchs internal elements

---

### Match

* a value based switching construct

* example [match-example.bal](./match-example.bal) to showcase foreach over json, xml and range

```
var value = 3;
match counter {
  0 => io:println("value is: 0");
  1 => io:println("value is: 1");
  2 => io:println("value is: 2");
  3 => io:println("value is: 3");
  4 => io:println("value is: 4");
  5|7 => io:println("value is: 5"); // binary OR expression allowed
  _ => io:println("default"); // final static value match pattern; optional
}
```

---

### Tuple Match

* matchs value based on tuple size, can also place guard checks for value types

* example [tuple-match-example.bal](./tuple-match-example.bal) to showcase foreach over json, xml and range

```
(string, int)|(float, string, boolean)|float tpl = 66.6;

match tpl {
  var (a, b, c) =>io:println("float, string, bool: ", io:sprintf("%s",     tpl));
  var (a, b) =>io:println("string: ", a, " | int: ", b);
  var a =>io:println("float: ", a);
}

match tpl {
  var a if (a is float) =>io:println("float: ", a);
  _ => io:println(io:sprintf("%s", tpl) + " can't be matched");
}
```

---

### Record Match

* record binding pattern match like above simple and with guard

* example [record-match-example.bal](./record-match-example.bal) to showcase foreach over json, xml and range

```
// keep var names like var1, var2, var3 only... or things go shaky sometimes

function recordMatch(r) {
  match r {
    var {var1, var2} if (var1 is string && var2 is RecordB) => io:println("rec with 2: " + io:sprintf("%s",var1) + ", " + io:sprintf("%s",var2));
    var {var1} => io:println("rec with 1: " + io:sprintf("%s",var1));
    _ => io:println("unmatched record", r);
  }
}

type RecordA record {
  string var1;
};
type RecordB record {
  string var1;
  RecordA var2;
};
RecordA a1 = {var1: "something"};
RecordB b1 = {var1: "abc", var2: a1};
```

---

### Type Guard

* type-guard `is` check test type allowing selective code execution

* example [type-guard-example.bal](./type-guard-example.bal) to showcase type guards, custom error, optional return type

---

### Elvis

* conditional operator handles `nil`, uses `?:` to assign existing value if not nil else default

* example [elvis-example.bal](./elvis-example.bal) to showcase elvis

```
string|() s1 = ();

string elvisResult1 = s1 ?: "elvis-default"; // same via elvis
io:println(elvisResult1);
```

---
