
# Chapter.1 Clojure Philosophy

## The Clojure Way

broad goals the Rich Hickey had in mind

* Simplicity ~ let's you tackle complexity with data modeling, concurrent threads and independent libraries

* Freedom of Focus ~ almost everything can be redefined, even at runtime

* Empowerment ~ direct, wrapper-free, compiling to same bytecode... utilized JVM to max

* Clarity ~ the containment of constructs in their scope

* Consistency ~ syntax and data structures

---

## Why a(nother) Lisp?

* Beauty

John McCarth's essay `Recursive Functions of Symbolic Expressions and Their Computation by Machine` define whole language in terms of only seven functions and 2 special forms: `atom`, `car`, `cdr`, `cond`, `cons`, `eq`, `quote`, `lambda` and `label`.


* Extreme Flexibility

Easy to create DSLs to solve narrow application-specific problems.


* Code is data

Homoiconicity.
When language is represented as inherent data structures, language itself can manipulate its own structure and behavior. (Graham 1995)

---

## Functional Programming

* functions of a language be able to be stored, passed and returned

* Clojure's style of FP includes purity, immutability, recursion, laziness, referential transparency

---

## Why Clojure isn't esp. OO

* immutability lies at the cornerstone

* focuses on distinction between state, time and identity

* provides polymorphism via multimethods and protocols

* provides subtyping and interface-oriented programming

* allows encapsulation, group functions with their supporting data using namespace

---

