Informatics 1 - Functional Programming
at University of Edinburgh
By Philip Wadler

Covered From Lecture00-06
* Introduction
* Functions
* Lists and Recursions
> * Part I : Comprehension
> * Part II : Recursion
> * Part III : Using conditions
> * Part IV : Filtering (Guards)
> * Part V : Accumulation
> * Part VI : Putting it all Together
* More Fun with Recursion
> * Part.I Booleans and Characters
> * Part.II Conditionals and Associativity
> * Part III - Append
> * Part IV - (Not Covered)
> * Part V - Counting
> * Part VI : Zip and Search
> * Part.VII Select, Take and Drop

---

[Lecture 00 on 26/September/2011]()
> Linux Basics

## Introduction
[Lecture 01 on 27/September/2011]()

FP operates on DataStructures as a whole. Good for concurrency.

Haskell designed by committee in 1989.
* Paul Hudak (Yale University)
* John Huges (Chalmers University)
* Simon Peyton Jones (Microsoft Research)
* Philip Wadler (Edinburgh University)

[Lecture 02 on 28/September/2011]()
> * Functional Family
> > Examples: Erlang, F#, Haskell, Hope, JS, Miranda, O'Caml, Racket, Scala, Scheme, SML
> * OOP Family
> > Examples: C++, F#, Java, JS, O'Caml, Perl, Python, Ruby, Scala

[TRIVIA: Philip Wadler and Martin Odesky worked on getting Generics into Java.]

FP In real world:
* Google MapReduce, Sawzall
* Ericsson AXE Phone Switch
* Perl 6
* DARCS
* XMonad
* Yahoo
* Twitter
* Facebook

Features from FP into other worlds:
* Garbage Collection (Java, C#, Python, Perl, Ruby, JS)
* Higher-Order Functions (Java, C#, Python, Perl, Ruby, JS)
* Generics (Java, C#)
* List Comprehensions (C#, Python, Perl6, JS)
* Type Classes (C++ as concepts)


## Functions

```
 f(x) = x'
```

```
let double p = p + p
let triple p = p + p + p
```

```
invert :: Picture -> Picture
knight :: Picture

-- beside to render beside
let doubleKnight p = beside p p
let tripleKnight p = beside p (beside p p)
let tripleKnight' p = beside (beside p p) p
let tripleKnight'' p = (p `beside` p) `beside` p

invert knight
```
---

## Lists and Recursions
[Lecture 03 on 03/October/2011]()

### Part I : Comprehension

* List Comprehensions -- Generators
```
[ x*x | x <- [1,2,3] ]

[ toLower c | c <- "Hey, Hey! My, My!" ]

[ (x, even x) | x <- [1,2,3] ]

```

Here 'element <- [ListElements]' is called a 'Generator'.
And '<-' is pronounced 'drawn from'.


* List Comprehensions -- Gaurds

```
[ x | x <- [1,2,3], odd x ]

[ toLower c | c <- "Hey, Hey! My, My!", isAlpha c ]
```

Condition for using an element from list like 'odd x' is Guard.


* List - existing accumulators

```
sum [1,2,3]
sum []
sum [ x*x | x <- [1,2,3], odd x ]

product [1,2,3]
product []

-- factorial :: [Integer] -> Integer
let factorial n = product [1..n]
factorial 5
factorial 0
```

Example Code 'lecture03.example00.hs' with QuickCheck checking list accumulator sample code.


### Part II : Recursion

* Cons and Append

Cons '(:)', constructs to list. Append '(++)', appends to list.
```
(:) :: a -> [a] -> [a]
(++) :: [a] -> [a] -> [a]
```

```
1 : [2,3]       -- [1,2,3]
[1] ++ [2,3]    -- [1,2,3]
[1,2] ++ [3]    -- [1,2,3]

'F' : 'irst'    -- "First"
"Th" ++ "irst"  -- "Thirst"

[1] : [2,3]     -- type error;
1 ++ [2,3]      -- type error;
[1,2] : [2,3]   -- type error;
[1,2] ++ 3      -- type error;
"1" : "2,3"     -- type error;
'1' ++ "2,3"     -- type error;

-- every list is syntactic sugar of cons
-- [1,2,3] = 1 : (2 : (3 : []))
-- "abc" = 'a' : ('b' : ('c' : []))
```

A recursive definition, a list is either
> * null, written '[]'; or
> * cons, written x:xs with head x (an element) and tail xs (a list)

```
null [1,2,3]        -- False
head [1,2,3]        -- 1
tail [1,2,3]        -- [2,3]
head [3]            -- 3
tail [3]            -- []

null []             -- True
head []             -- Exception: empty list
tail []             -- Exception: empty list
```

Square style with recursion
```
squareRec :: [Integer] -> [Integer]
squareRec [] = []
squareRec (x:xs) = x*x : squareRec xs

-- also using pattern matching for function definition
```

### Part III : Using conditions

[Lecture 04 on 04/October/2011]()

Squares recursion style in Conditionals with Bindings (same compute as squareRec)
```
squareCond :: [Integer] -> [Integer]
squareCond ws =
  if null ws then
    []
  else
    let
      x = head ws
      xs = tail ws
    in
      x*x : squareCond xs:w

```

Can use QuickCheck to test it

```
import Test.QuickCheck

prop_squareRecCond :: [Integer] -> Bool
prop_squareRecCond xs = squareRec xs == squareCond xs
```

### Part IV : Filtering (Guards)

Recursive for 'odds'
```
oddsRec :: [Integer] -> [Integer]
oddsRec []                  = []
oddsRec (x:xs) | odd x      = x : oddsRec xs
               | otherwise  = oddsRec xs

-- if multiple matches, always first match is picked
```

[TRIVIA: Actual implementation of list is done using Pointers.]

### Part V : Accumulation

```
sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: [Integer] -> Integer
product' [] = 1                     -- empty list To Identity
product' (x:xs) = x + product' xs
```

### Part VI : Putting it all Together

```
-- sum of square of odd numbers in Recursive alternative to lecture03.example00.hs

sumSqOddRec :: [Integer] -> Integer
sumSqOddRec []                  = 0
sumSqOddRec (x:xs) | odd x      = x*x + sumSqOddRec xs
                   | otherwise  = sumSqOddRec xs

```

[TRIVIA: Gottfried Wilhelm Leibniz (1646-1716) discovered calculus (independently of Newton) and introduced term 'Monad' to philosophy. 'Equality'.]

---
[Lecture 05 on 10/October/2011]()

## More Fun with Recursion

### Part.I Booleans and Characters

* Boolean Operators

```
not :: Bool -> Bool
not False = True
not True = False

(&&), (||) :: Bool -> Bool -> Bool
False && False  = False
False && True   = False
True  && False  = False
True  && True   = True

False || False  = False
False || True   = True
True  || False  = True
True  || True   = True
```

* Defining boolean operations on Characters

```
isLower :: Char -> Bool
isLower x = 'a' <= x && x <= 'z'

isUpper :: Char -> Bool
isUpper x = 'A' <= x && x <= 'Z'

isDigit :: Char -> Bool
isDigit x = '0' <= x && x <= '9'

isAlpha :: Char -> Bool
isAlpha x = isLower x || isUpper x
```

* Defining conversion operations on Characters

```
digitToInt :: Char -> Int
digitToInt c | isDigit c = ord c - ord '0'
             | otherwise = c

intToDigit :: Int -> Char
intToDigit d | 0 <= d && d <= 9 = chr (ord '0' + d)
             | otherwise        = error "It need to be a single digit number."

toLower :: Char -> Char
toLower c | isUpper c = chr ((ord c - ord 'A') + ord 'a')
          | otherwise = c

toUpper :: Char -> Char
toUpper c | isLower c = chr ((ord c - ord 'a') + ord 'A')
          | otherwise = c
```

### Part.II Conditionals and Associativity

* Conditional Equations
```
max :: Int -> Int -> Int
max x y | x >= y = x
        | y >= x = y

max3 :: Int -> Int -> Int -> Int
max3 x y z | x >= y && x >= z = x
           | y >= x && y >= z = y
           | z >= x && z >= y = z
```

* Conditional Equations with otherwise
```
max :: Int -> Int -> Int
max x y | x >= y    = x
        | otherwise = y

max3 :: Int -> Int -> Int -> Int
max3 x y z | x >= y && x >= z = x
           | y >= x && y >= z = y
           | otherwise        = z
```

* Conditional Expressions
```
max :: Int -> Int -> Int
max x y = if x >= y then x else y

max3 :: Int -> Int -> Int -> Int
max3 x y z = if x >= y && x >= z then x
             else if y >= x && y >= z then y
             else z

-- also --fast-but-bad-to-extend
max3' :: Int -> Int -> Int -> Int
max3' x y z = if x >= y then
                if x >= z then x else z
              else
                if y >= x then y else z

-- better way
max3'' :: Int -> Int -> Int -> Int
max3'' x y z = max (max x y) z
```

* Infix notation, better to read and extend

```
max :: Int -> Int -> Int
max x y | x >= y    = x
        | otherwise = y

max3 :: Int -> Int -> Int -> Int
max3 x y z = x `max` y `max` z

-- can check Associativity by QuickCheck
prop_max_assoc :: Int -> Int -> Int -> Bool
prop_max_assoc x y z =
  (x `max` y) `max` z == x `max` (y `max` z)
```

* Key Properties about Operators
> Associativity, Identity, Commutativity, Distributivity, Zero, Idempotence


### Part III - Append

```
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- "abc" ++ "de"  == ('a' : ('b' : ('c' : "de")))
```

* Quickcheck for Append
```
prop_append_assoc :: [Int] -> [Int] -> [Int] -> Bool
prop_append_assoc xs ys zs =
  (xs ++ ys) ++ zs == xs ++ (ys ++zs)

prop_append_ident :: [Int] -> Bool
prop_append_ident xs =
  xs ++ [] == xs && xs == [] ++ xs

prop_append_cons :: Int -> [Int] -> Bool
prop_append_cons x xs =
  [x] ++ xs == x : xs
```


* Efficiency of Append is length 'n' of first list 'xs'


* Associativity and Efficiency : Left vs Right
> Computing associated to Left
> (((x1 ++ x2) ++ x3) ++ x4)
> steps taken are (n1 + (n1 + n2) + (n1 + n2 + n3) ...); for m lists of length n takes m-square-n
> Computing associated to Right
> x1 ++ (x2 ++ (x3 ++ x4))
> steps taken are (n1 + n2 + n3 + ...); for m lists of length n takes mn
> Associated to Right manifolds faster.


* Associativity and Efficiency : Sequential vs Parallel
> Sequential takes (m-1) steps for m lists in Right associated.
> Parallel takes log-base2-m.
> > (((xs1 ++ xs2) ++ (xs3 ++ xs4)) ++ ((xs5 ++ xs6) ++ (xs7 ++ xs8)))


[Lecture 06 on 11/October/2011]()

### Part IV - List Comprehensions, revisited
covered later in Lecture.07


### Part V - Counting
[TRIVIA: Think of everything in Haskell as function, even if it doesn't look like one.]

```
[1..10]

{-
-- [m..n] stands for 'enumFromTo m n'
-- with Recursion
enumFromTo :: Int -> Int -> [Int]
enumFromTo m n | m > n  = []
               | m <= n = m : enumFromTo (m+1) n
-}
enumFromTo 1 10
```

* Example of above Technique - Factorial
```
-- Library function
factorial :: Int -> Int
factorial n = product [1..n]

-- Recursion
factorialRec :: Int -> Int
factorialRec n = fact 1 n
  where
  fact :: Int -> Int -> Int
  fact m n | m > n   = 1
           | m <= n  = m * fact (m+1) n
```

* Counting Forever, it can evaluate head and tail later, Lazy Evaluation
```
[0..]

{-
-- [m..] sounds for enumFrom m
enumFrom :: Int -> [Int]
enumFrom m = m : enumFrom (m+1)
-}
enumFrom 1
```


### Part VI : Zip and Search

```
{-
-- zip lists don't have to be of same type
-- -- Liberal, handles lists of different length
zip :: [a] -> [b] -> [(a,b)]
zip [] ys         = []
zip xs []         = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

-- -- Conservative, errors of lists with different length; Defensive Programming
zipHarsh :: [a] -> [b] -> [(a,b)]
zipHarsh [] []         = []
zipHarsh (x:xs) (y:ys) = (x,y) : zipHarsh xs ys

-}

zip [0,1] "abc"         -- [(0,'a'), (1,'b')]
zip [0,1,2] "ab"        -- [(0,'a'), (1,'b')]
zip [0,1] "ab"          -- [(0,'a'), (1,'b')]
zip [0..] "words"       -- [(0,'w'), (1,'o'), (2,'r'), (3,'d'), (4,'s')]

zipHarsh [0,1] "ab"    -- [(0,'a'), (1,'b')]
zipHarsh [0,1,2] "ab"    -- error
```

* Dot Product of 2 Lists
```
-- Comprehensions and Library functions
dot :: Num a => [a] -> [a] -> [a]
dot xs ys = sum [ x*y | (x,y) <- zip xs ys ]

-- Recursion
dotRec :: Num a => [a] -> [a] -> [a]
dotRec [] []          = 0
dotRec (x:xs) (y:ys)  = x*y + dotRec xs ys
```


* Search
```
{-
-- Comprehensions and Library functions
search :: Eq a => [a] -> a -> [Int]
search xs y = [i | (i,x) <- zip [0..] xs, x==y]

-- Recursion
searchRec :: Eq a => [a] -> a -> [Int]
searchRec xs y = srch xs y 0
  where
  srch :: Eq a => [a] -> a -> Int -> [Int]
  srch [] y i         = []
  srch (x:xs) y i
    | x == y      = i : srch xs y (i+1)
    | otherwise   = srch xs y (i+1)
-}

search "bookshop" 'o'
```


### Part.VII Select, Take and Drop

```
-- Select
"words" !! 3               -- selects index-3 element 'd'

-- Take
take 3 "words"             -- take first 3 elements "wor"

-- Drop
drop 3 "words"             -- drops first 3 elements and takes "ds"


{-
-- Comprehensions functions 
-- but using Infinite Lists, will return here cuz of zip and Lazy

(!!) :: [a] -> Int -> a
xs !! i = the [ x | (j,x) <- zip [0..] xs, j == i ]
  where
  the [x] = x

take :: Int -> [a] -> [a]
take i xs = [x | (j,x) <- zip [0..] xs, j < i]

drop :: Int -> [a] -> [a]
drop i xs = [x | (j,x) <- zip [0..] xs, j >= i]

-- Recursion
(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! (i+1) = xs !!! i

takeRec :: Int -> [a] -> [a]
takeRec 0 xs = []
takeRec i [] = []
takeRec (i+1) (x:xs) = x : takeRec i xs

dropRec :: Int -> [a] -> [a]
dropRec 0 xs      = xs
dropRec i []      = []
dropRec (i+1) (x:xs) = dropRec i xs


-- Conditional with binding for 'take'
takeCond :: Int -> [a] -> [a]
take j ws =
  if j == 0 || null ws then
    []
  else
    let
      x   = head ws
      xs  = tail ws
      i   = j - 1
    in
      x : takeCond i ws


-- with Guards 'take'
takeGuards :: Int -> [a] -> [a]
takeGuards 0 xs = []
takeGuards i [] = []
takeGuards i (x:xs) | i > 0 = x : takeGuards (i-1) xs
-}
```

* Natural Numbers - Recursive Defining
> Every Natural Number can be written using only (+1) and 0.
> A Natural number is either
> * zero, written (0), or
> * successor, written (n+1); with predecessor n (a natural number)

---
---
