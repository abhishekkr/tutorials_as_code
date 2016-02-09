## Introduction

A purist approach to FP is programming using Mathematical Functions.

Here we'll follow expressions are important than statements. It's supported specifically by supporting Lambda Expressions.

Example: summing integers 1-to-10
In imperative style, computation method is variable assignment
```
total = 0
for i=1; i<=10; i++ {
  total += i
}
```

In functional style, computation method is function approach
```
sum [1..10]
```
---

> Historical Background
> * Alonzo Church developed Lambda Calculus in 1930s.
> * John McCarthy developed Lisp in 1950s.
> * In 1960s, Peter Landin developed ISWIM (If You See What I Mean). The first FP language based strongly on lambda calculas without any assignments.
> * In 1970s, John Backus develops FP. A functional language emphasizing on higher-order functions and reasoning about programs. He developed Fortran around same time as Lisp.
> * Also in 1970s, Robert Milner and others developed ML. It was first hybrid language, which introduced type inference and polymorphic types. Was originally designed as scripting language to write proofs.
> * 1970s-1980s David Turner developed number of lazy func. lang. culminating in Miranda system.
> * 1987 Haskell got initiated, 2003 Haskell98 Report published

---

A Taste of Haskell

```QuickSort.hs
f [] = []
f (x:xs) = f ys ++ [x] ++ f zs
           where
             ys = [a | a <- xs, a <= x]
             zs = [b | b <- xs, b > x]
```

---
