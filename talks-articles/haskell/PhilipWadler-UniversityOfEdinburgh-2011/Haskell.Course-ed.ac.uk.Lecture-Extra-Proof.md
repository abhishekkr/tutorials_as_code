Informatics 1 - Functional Programming
at University of Edinburgh
By Phil Scott

Covered From Lecture09
* What is a Proof
* Rule of Leibniz (with idea Proof was Mechanical)
* Natural Number Proofs
* List reversal

---

[Lecture 09 on 25/October/2011]()

## Lecture.09 - Proof

### What is a Proof
```
import Test.QuickCheck

squares :: Integer -> Integer
squares x = x * x

squares_prop :: Integer -> Integer -> Bool
squares_prop x y =
  squares (x+y) == x * x + 2 * x * y + y * y

-- quickCheck squares_prop
```

> Convince a Computer


### Rule of Leibniz (with idea Proof was Mechanical)

* Indiscenrability of Identity
* Identity of Indiscenrability 
* Equality is Reflexive
* Equals may be substituted for Equals

> but; 'i++ != i++'

[TRIVIA: Gottlob Frege played a major role in modern logic.]

* Using 'Algebraic Rules' for re-writing Proofs

* Natural Numbers
```
data Nat = Zero
         | Suc Nat

(+) :: Nat -> Nat -> Nat
x + Zero = x
x + Suc y = Suc (x + y)

(*) :: Nat -> Nat -> Nat
x * Zero = Zero
x * Suc y = x + (x * y)

one = Suc Zero
two = Suc one
three = Suc two
four = Suc three

{-
two + two
-- Suc (Suc Zero) + Suc (Suc Zero)
-- Suc (Suc (Suc (Suc Zero)))
four
-}
```

* Proving Arithmetic Identity and Commutativity

* Lists
```
data [a] = []
         | a : [a]

(++) : [a] -> [a] -> [a]
[] ++ xs      = []
(x:xs) ++ ys  = x : (xs ++ ys)

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]
```

---
---
