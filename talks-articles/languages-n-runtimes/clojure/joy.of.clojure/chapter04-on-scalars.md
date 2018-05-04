
# Chapter.4 On Scalars

## Understanding Precision

* Truncation; clojure truncates floating-point number by default. If high-precision is required then do explicit typing with literal `M`.

* Promotion is automatic within clojure, to accomodate larger values.

* Overflow error will cause wrap around of bits in Int and Long values.

* Underflow is where a value collapses to zero when it's a really small floating point.

* Rounding errors

---

## Trying to be rational

* Clojure's rationals allow arbitrarily large numerators and denominators.

* Be rational to avoid floating-point corruption in calculations.

* Clojure provides `ratio?`, `rational?` and `rationalize`

* though accurate, isn't as fast

---

## When to use keywords

> kin'of similar to Ruby's Symbol Type

* keywords always refer to themselves, like `:alice` always has value `:alice`

* symbol `alice` can refer to any clojure value or reference

* keywords are popularly used as `keys` in a map; as enumeration values; dispatch values

* keywords can provide a directive like

```
(defn pour [lb ub]
  (cond                                ; takes a keyword toujours which will instead return
    (= ub :toujours) (iterate inc lb)  ; an infinite lazy range starting with first to "forever"
    :else (range lb ub)))  ; cond uses a keyword :else as truthy to mark default conditional case

(pour 1 10)
;=> (1 2 3 4 5 6 7 8 9)
```

* keywords don't belong to specific namespace, keywords could be created with prefix though `:haunted/house`

---

## Symbolic resolution

* Clojure Symbols are roughly analogous to identifiers in other languages.

* Symbols aren't unique based o just name alone `(identical? 'id1 'id1) ; gives false as both different object`

* Clojure can attach metadata to various objects, `with-meta` function take object and a map and returns another object with metadata attached

```
(let [x (with-meta 'animal {:kind 'dog'})
      y (with-meta 'animal {:kind 'cat'})]
    [(= x y)
     (identical? x y)
     (meta x)
     (meta y)])

;=> [true false {:kind dog'} {:kind cat'}]
```

* Like keywords, symbols don't belong to any namespace either. Symbols qualification is a characteristic of evalutation and not ingerent in symbol.

#### Lisp-1

* Clojure is Lisp-1 i.e. it uses same name resolution for function and value bindings.

* Lisp-2 like Common-Lisp have name resolution performed depending on context of symbol.

* In Lisp-1, same name resolution scheme is used for functions and arguments... existing functions can get shadowed with other locals or Vars.

---

## Regular Expressios - the second problem

* a literal regexp in Clojure looks like `#"a pattern"`, this get compiled to `java.util.regex.Pattern` at read time

* escape using blackslash, in double quotes special characters' backslash need be escaped

```
(java.util.regex.Pattern/compile "\\d")
;=> #"\d"
```

* regexp mode could be set with `(?<flag>)` like `#"(?i)yes"` would match case-insensitive "yes"

* regex flags available are

```
flag | name             | description
--------------------------------------
 d   | UNIX_LINES       | ., ^ and $ match only unix line terminator \n
 i   | CASE_INSENSITIVE | ascii characters with disregard for their case
 x   | COMMENTS         | whitespace and comments in pattern are ignored
 m   | MULTILINE        | ^ and $ match near line terminators
 s   | DOTALL           | . matches any char incl. line terminator
 u   | UNICODE_CASE     | causes i:flag to use Unicode case insensitivity
```

#### Functions

* Java regex pattern object has several methods to be used, `split` is most common

```
(seq (.split #"." "java.util.regex"))
;=> ("java" "util" "regex")
```

* `re-seq` returns a lazy sequence of all matches

```
(re-seq #"\w+" "ay-bee/cee")
;=> ("ay" "bee" "cee")
```

* if regexp has capturing groups, match are vector not matched strings

```
(re-seq #"(\w)\w*" "ay-bee/cee")
;=> (["ay" "a"] ["bee" "b"] ["cee" "c"])
```

#### Beware of mutable matchers

Java's regexp engine includes a `Matcher` object that mutates in non thread-safe way.

This object is available in Clojure via `re-matcher` function, can be used as an argument to `re-groups` and single parameter form of `re-find`.

---

