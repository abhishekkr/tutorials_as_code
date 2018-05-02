
# Chapter.2 Drinking from Clojure Firehose

prompt `user>` in REPL session indicates default top-level namespace

```
user=> 21 ; semicolon starts a comment in REPL to a newline
21
user=> (+ 1 2)
3
```


## Scalars

Clojure provides several categories of scalar types such as integers, floats, rationals, symbols, keywords, strings, characters, booleans and regex patterns representing single unit of data.

Symbols refer to some other value in a given context. Keywords are similar, except they always evaluate to themselves.

---

## Putting things together: collections

Lists, Vectors, Maps and Sets are primary collections. Any empty collection ain't same as nil. Collections are heterogeneous in terms of elements.

First item in list will be resolved to function, macro or special form. Can store all kinds of elements. Some list examples `(1 2 3 4 5)` and `(:bob rosa)`.

> A `form` is any Clojure object meant to be evaluated. A `special form` is a form with special syntax or eval rules not implemented using base Clojure forms.

Vectors store series of values, use square bracket syntax `[1 2 :a :b :c]`. Vectors evaluate each item in order.

Maps store unique keys and one value per key, similar to dictionaries. Commas are frequently used between pairs like `{ 1 "one", "two" 2, "threeitems" (1 2 3) }` with curly braces.

Sets contain zero or more unique items. They are written using curly braces with leading hash, such as `#{1 2 "three"}`.

---

## Making things happen: functions

Functions can be stored in vars, held in collections and passed as values to-from other functions.
Uses infix notation.

#### Defining functions

An example of a function taking 2 elements that returns a set of those elements would be `(fn mk-set [x y] #{x y})`
Anonymous clojure function can be defined as a special form. Above could be used as `((fn [x y] #{x y}) 1 2)`.

To except either one or two arguments. 

```
(fn
  ([x] #{x})
  ([x y] #{x y}))
```

But to take any number of arguments above a count, utilize `&` symbol. Any argument before `&` be bound one to one, any additional will be aggegated as sequence.

```
((fn [x y & z]
   [x y z])
   1 2)
;=>[1 2 nil]

((fn [x y & z]
   [x y z])
   1 2 3 4)
;=>[1 2 (3 4)]
```

The `def` special form is a way to assign a symbolic name to a piece of Clojure data.

```
(def make-set (fn [x y] #{x y}))
```

Using `defn` macro is convenient and allows an additional documentation string

```
(defn make-set
  "turns params to set"
  ([x]    #{x})
  ([x y]  #{x y})
  )
```

Shorthand notation for creating anonymous function using `#()` reader feature. It can also accept arguments implicitly declared through use of special symbols prefixed with `%`.
```
(def mak-a-lst_ #(list %))
(def mak-a-lst1 #(list %1))
(def mak-a-lst2 #(list %1 %2))
(def mak-a-lst2+ #(list %1 %2 %&))
```

> `reader features` are analogous to pre-processor directives, some given form be replaced by other.

---

## Vars

Var is named by symbol and holds a single value.

* declaring bindings using `def` as `(def x 10)`, creates what's known `root binding` i.e. a binding that's same across all threads (unless otherwise rebound to specific threads)

* vars don't need value, we can just declare them as `(def x)` and defer their value binding responsibility to threads

---

## Locals, loops and blocks

#### blocks

* use `do` form when have a series or block of expressions, all will be evaluated but only last will be returned

```
(do 10
  (+ 5 5)
  (- 2 1))
;=> 1  ;; the middle bits here are typically where side-effects occur
```

#### locals

* there are no local variables, but there are locals created using `let` form

* starts with a vector defining the binding, followed by any number of expressions that make up the body

* all of these pairing are available for remainder of the `let` body, sometimes described as implicit-do as similarly all expressions are evaluated but only last one returned

```
(let [r           5
      pi          3.1415
      r-squared   (* r r)]
  (println "radius is" r)
  (* pi r-squared))
```

* complex binding expressions can be used to pull apart expressions and return collections... `destructuring`

* as they are immutable, can't be used to accumulate results


#### loops

* `recur` is specifically for tail recursion, here x is rebind to new value and control returned to top

```
(defn list-down-from [x]
  (when (pos? x)          ;; use when to perform side-effects
    (println x)           ;; this got side-effect
    (recur (dec x))))

;; without side effect, build up accumulator and return final result
(defn sum-down-from [sum x]
  (if (pos? x)
    (recur (+ sum x) (dec x))   ;; then block
    sum))                       ;; else block
```

* if function has multiple arguments, then so does `recur`, expression here are first eval and then binded

* `loop` form acts like `let` but provides a target for `recur` to jump to, used like

```
(defn sum-down-from [initial-x]
  (loop [sum 0, x initial-x]
    (if pos? x)
       (recur (+ sum x) (dec x)) ;; a recur always loops back to closest loop/fn
        sum))                    ;; here it's loop
```

* `recur` form can only appear in tail position, else Clojure compiler will remind you

---

## Preventing things from happening: quoting

* Clojure has 2 quoting forms: `quote` and `syntax-quote`

* primary ways of including literal scalars and composites, without eval as code

#### using 'quote'

```
(cons 1 [2 3])
;=> (1 2 3)

quote (cons 1 [2 3])
;=> (cons 1 [2 3])

(cons 1 (2 3))
; gives error as tries interpret 2 as function

(cons 1 (quote (2 3)))
;=> (1 2 3)

(cons 1 '(2 3))    ; shortcut single quote could be used as well
;=> (1 2 3)
```

#### using 'syntax-quote', written as single back-quote

```
`(1 2 3)
;=> (1 2 3)
```

* `syntax-quote` doesn't expand to simple form but whatever set of expression is required to support features like 'symbol auto-qualification'

* a symbol can begin wth namespace and a slash, can be called `qualified symbols`... `syntax-quote` will qualify all unqualified symbols in arguments

```
`map
;=> clojure.core/map

`Integer
;=> java.lang.Integer

`(map even? [1 2 3])
;=> (clojure.core/map clojure.core/even? [1 2 3])
```

#### unquote

* to evaluate a sub-form under `quote`, use tilda `~` to unquote

```
`(+ 10 ~(* 3 2))
;=> (clojure.core/+ 10 6)

(let [x '(2 3)] `(1 ~x))
;=> (1 (2 3))
```

#### unquote-splicing

* `~@` tells Clojure to unpack sequence x, splicing it intp result rather than inserting as nested list

```
(let [x '(2 3)] `(1 ~@x))
;=> (1 2 3)

(let [x '(2 3)] `((4 5) ~@x))
;=> ((4 5) 2 3)

(let [x '(2 3)] `(4 5 ~@x))   ; with splice
;=> (4 5 2 3)

(let [x '(2 3)] `(4 5 ~x))    ; without splice
;=> (4 5 (2 3))
```

#### auto-gensym

* when you need an unqualified symbol, such as for a parameter or let local name

```
`newvar#
;=> newvar__5863__auto__
```

---

## Leveraging Java via interop

* access static class members using a syntax like accessing namespace-qualified var

```
(Math/sqrt 9)
;=> 3.0
```

* `new` special form closely mirror Java class instance model

```
(new java.util.HashMap {"foo" 42 "bar" 9 "baz" "quux"})
;=> {"bar" 9, "baz" "quux", "foo" 42}

;; idiomatic form is class name followed by dot signifying constructor call
(java.util.HashMap. {"foo" 42 "bar" 9 "baz" "quux"})
;=> {"bar" 9, "baz" "quux", "foo" 42}
```

* access Java instance members with the `.` operator

```
(.x (java.awt.Point. 10 20))
;=> 10  ;; return value of field x from Point instance given
```

* setting java instance properties, first arg to `set!` is instance member access form

```
(let [origin (java.awt.Point. 0 0)]
    (set! (.x origin) 15)
    (str origin))
;=> "java.awt.Point[x=15,y=0]"
```

* the `..` macro to simplify chained method calls

```
new java.util.Data().toString().endsWith("2010")     /* java code */

(.endsWith (.toString (.java.util.Data.)) "2010")    ; Clojure code

(.. (java.util.Data.) toString (endsWith "2010"))    ;; with .. macro
```

* the `doto` macro to streamline set of mutators

```
/* java code */
java.util.HashMap props = new java.util.HashMap();
props.put("Dead", "Pool");
props.put("Iron", "Man");

;; Clojure code
(doto (java.util.HashMap.)
  (.put "Dead" "Pool")
  (.put "Iron" "Man"))
```

* `reify` and `deftype` macros provide ways to create realizatios of Java interfaces

* a macro named `proxy` can be used to implement interfaces and extend base classes on fly

* `gen-class` macro to generate statically named classes

---

## Exceptional circumstances

* Clojure provides `throw` and `catch` forms to handle exceptions

* throwing exception is really easy

```
(throw (Exception. "I threw."))
;=> Exception I threw.  user/eval5826 (NO_SOURCE_FILE:1)
```

* syntax for catching exception is similar to java

```
(defn throw-catch [f]
  [(try
      (f)
      (catch ArithmeticException e "your math is disorienting")
      (catch Exception e (str "you know what you did " (.getMessage e)))
    (finally (println "i haz returned...")) )])

;
(throw-catch #(/ 10 2))
; i haz returned...
;=> [5]

(throw-catch #(/ 10 0))
;i haz returned...
;=> ["your math is disorienting"]

(throw-catch #(throw (Exception. "foo")))
;i haz returned...
;=> ["you know what you did foo"]
```

---

## Namespaces

* is a construct to package related functions together

* create a namespace using `(ns joy.ch2)`; clojure provides a Var `*ns*` that holds the value of current namespace

* in REPL, any created symbol is binded to namespace then

* Clojure provides convenience directive `:require` to load namespaces

```
(ns joy.a
  (:require clojure.set))

(clojure.set/intersection #{1 10 100} #{1 6 11})
```

* use `:as` directive to create an alias

```
(ns joy.a
  (:require clojure.set :as set))

(set/intersection #{1 10 100} #{1 6 11})
```

* namespace form looks similar to static class method; though it can't be referenced independently unlike class symbol

* can create mappings from other namespace to your own, to avoid qualifier with `:use` directive

```
(ns joy.a
  (:use [clojure.string :only [capitalize]]))

(map capitalize ["abc", "dEf"])

(defn plus1 [x] (+ 1 x))
```

* the `:exclude` directive with `:use` has inverse impact of `:only`, in case of clash the last Var definition wins

* a `:refer` directive  works like `:use`, though it only create mmappings for loaded libraries

```
(ns joy.b
  (:refer joy.a))

(plus1 9)
;=> 10
```

* `:rename` directive could be used with `:use` and `:refer` directive to expose imported function with other name

```
(ns joy.b
  (:refer joy.a :rename {plus1 plusone}))

(plusone 9)
;=> 10
```

* to use unqualified Java classes within any given namespace, they should be imported via `:import` directive

```
(ns j
  (:import [java.util.HashMap]
           [java.util.concurrent.atmoic.AtomicLong]))

(HashMap. {"y/n" "y"})

(AtomicLong. 1)
```

* any classes in Java's `java.lang` package are auto imported when namespaces are created

---
