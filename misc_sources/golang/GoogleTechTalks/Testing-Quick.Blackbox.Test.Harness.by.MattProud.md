
## testing/quick : Go's little-known blackbox test harness
(14/Oct/2015 Zurich Gopher Meetup, Matt T. Proud)

[found a blog on same](http://blog.matttproud.com/2015/06/testingquick-blackbox-testing-in-go-for.html)

Pre-cursor HomeWork "import testing"
* [How to Write Go Code]()
* [Venerable pkg/testing]()
* [Effective, Idiomatic and Polished Testing]()

---

> import "testing/quick"
Familiar friend to QuickCheck from Haskell world.

---

#### Main()

Go can generate arbitrary value for types, even named types.

code example: _Testing-Quick.Blackbox.Test.Harness.by.MattProud.01.go_

```
func Value(t reflect.Type, rand *rand.Rand) (value reflect.Value, ok bool)
```

To see it in action, checkout: "Testing-Quick.Blackbox.Test.Harness.by.MattProud.00.go"

---

#### Under the Hood

White noise generation in the default case.
Custom strategy if your type fulfills quick.Generator

```
type Stooge int

const (
	Invalid Stooge = iota
	Moe
	Larry
	Shemp
	Curly
	Joe
	CurlyJoe

	nStooges = int(CurlyJoe) + 1
)

func (s Stooge) Generate(rand *rand, size int) reflect.Value {
	return reflect.ValueOf(Stooge(rand.Intn(nStooges)))
}
```

---

#### Example 'TypeOf' Literals

* builtin: int
```
reflect.TypeOf(int(0))
```

* builtin: \*int
```
reflect.TypeOf((*int)(nil))
```

* first-class type: http.Dir
```
reflect.TypeOf(http.Dir(""))
```

* anonymous struct: struct{X,Y int}
```
reflect.TypeOf(struct{ X, Y int }{})
```

* anonymous struct: struct{X,Y int}
```
reflect.TypeOf((*struct{ X, Y int })(nil))
```

---

#### quick.Value Limitations

* exported fields

All fields (including children) must be 'exported identifiers'.

> * Legal ``` type point struct { X, Y int }
> * Illegal ``` type point struct { x, y int }


* channels

Appears to be artifical as opposed to substantive.
Reflection supports it 'reflect.MakeChan'.


* interfaces

Legitimate limitation. ``` quick.Value(reflect.TypeOf(struct{ R io.Reader }{}), rnd) ```

> * Need to access to runtime's registry of types and find fullfilling ones a'la 'Go Oracle'
> * New types cannot be decalred at runtime
> * quick.Value cannot operate on nil values, and  'zero value for interfaces is nil' precluding reflect.New and reflect.Zero


* graph type/structural

A data structure with some structural requirement (recursiveness, cyclical, completeness) might not get fulfill bu quick.Value
quick.Generator comes to rescue, can implement the requirement for the way of generation


* unsafe pointers

No confirmation of what it points to, its type, whether legal, etc.


* foreign code

> * Cgo or SWIG-wrapped
> * Code Generation (e.g. go generate or goprotobuf), look up pbtest.SanitizeGenerated for protobuf
> * Unaudited Vendored Code

---

#### Public API Design Considerations

Opt not to fulfill 'quick.Generator' in your public types:
* Pollutes the public API
* Locks users into only using your implementation

Moral: Be absolutely sure that everyone wants your implementation.
With unexported internals, go right ahead.

#### Fuzz Testing

It's about enforcing the invariant.

* Defining an Invariant

> Given some operation, the conditions that will be always true.
> eg.
> Commutative property of addition 'a + b == b + a'
> Let's test this in Go.

```
// pass use-case
add1 := func(a, b int) int {return a + b}
add2 := func(a, b int) int {return b + a}
fmt.Println("Counter examples against commutativity:", quick.CheckEqual(add1, add2, nil))

div1 := func(a, b int) int {return a / b}
div2 := func(a, b int) int {return b / a}
fmt.Println("Counter examples against commutativity:", quick.CheckEqual(div1, div2, nil))
```

* The Principles
> Disecting 'func CheckEqual(f, g interface{}, config *Config) (err error)'
> 'f' and 'g' need to be same in function signature
> arguments must be generatable by quick.Value


##### Equality

```
package ex

import (
 "strings"
 "testing"
 "testing/quick"

    )

// Don’t do this at home, kids.
func MyJoin(parts []string, delim string) (out string) {
  for i, part := range parts {
    if i != 0 {
   out = out + delim + part
  
    } else {
   out = part
  
    }
 
  }
 return out

}

func TestEquivalence(t *testing.T) {
 // execute f(…) and g(…) M times, and compare the output after each execution.
 f, g := strings.Join, MyJoin
   if err := quick.CheckEqual(f, g /* configuration */, nil); err != nil {
      t.Error(err)
   }
}
```

If function has an ungeneratable argument in its signature, you can always wrap it in an inner function that offloads the heavy lifting to 'quick.Value'.
```
package ex

import (
 "testing"
 "testing/quick"
)

func TestTransform(t *testing.T) {
  f := func(input []string) {
    ch := make(chan string, 1) // ch is "ungenerateable"
    existingTransform(input, ch)
  }
  g := func(input []string) {
    ch := make(chan string, 1) // ch is "ungenerateable"
    newTransform(input, ch)
  }
  if err := quick.CheckEqual(f, g, nil); err != nil {
    t.Fatal(err)
  }
}

// Prototype Stubs
func existingTransform(input []string, dest chan *string) {} // Old API
func proposedTransform(input []string, dest chan *string) {} // New API
```

---

##### Ensuring Error are Comprehensible

use testing.T's fatalf and other methods

code example: _Testing-Quick.Blackbox.Test.Harness.by.MattProud.02.go_
---

#### Configuration

* Test Sizes and Iteration Cycles

Type 'quikc.Config' can be used to configure 'quick.Check' and 'quick.CheckEqual'.
For 'quick.Value', it doesn't provide support.

To Control the number of iterations per test, either one can set 'quick.Config.MaxCount' or set via flag.

---

#### Traps and Pitfalls

* caveats for equality testing with quick.CheckEqual
> Powered by reflect.DeepEqual, so '[]T(nil) != []T{}' and 'map[T]T(nil) != make(map[T]T)'
> as in no slice/map and empty slice/map are different
> Arguments to 'f' and 'g' are passed as same value, so mutations in 'f' or 'g' to values are propogated from one to another.

code example: _Testing-Quick.Blackbox.Test.Harness.by.MattProud.03.go_

* cleaning up after execution

```
// a pattern like
func f(){
  defer func(){/*clean-up f*/}
  //perfrom f work
}
```

---

#### quick.Check
> Swiss-Army Knife of Fuzz Checkers

```
func Check(f interface{}, config *Config) (err error)
```

Similar to quick.CheckEqual, but 'f' must conform to function signature, 'ok' indicates whether invariants were upheld.

```
func f(x0,x1....,xn) (ok bool)
```

---

#### Invariant testing with quick.Check

t value is the local state of actual test context

```
func TestFoo(t *testing.T) {
  satisfies := func(/* arguments */) bool {
    // Setup Test Context
    // Exercise System under Test using Context and Arguments
    // Validate Invariants
    return invariantsSatisfied
  }
  if err := quick.Check(satisfies, nil); err != nil {
    t.Error(err)
  }
}
```

* Applicability

> * End-to-End Encoder and Decodder Pipelines (Protocol Buffer Examples)
> * Testing Well-Defined Requirements for Discrete Units
> * Public API
> * Hardening for Malicious Input
> * Probabalistic Systems (Streaming Estimator Example)
> * High count child test cases for Known Inputs and Expected Outcomes

---

### Similar Efforts

* Dmitry Vyukov authored 'go-fuzz', very handy and useful.
* Stochastic Monkey Testing, using quick.Check to drive 'monkey test' of system tests' state transition.

---

