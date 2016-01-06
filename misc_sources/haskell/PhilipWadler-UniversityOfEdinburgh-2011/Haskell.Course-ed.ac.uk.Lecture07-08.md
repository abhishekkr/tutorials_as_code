Informatics 1 - Functional Programming
at University of Edinburgh
By Philip Wadler

Covered From Lecture08
* Extra Part : List Comprehension - revisited
* Lecture.7 : Map, Filter, Fold
> * Part.I Map
> * Part.II Filter
> * Part.III Fold
> * Part.IV Map,Filter,Fold together
> * Part.VI Currying
* Lecture.8 : Lambda and some revisited
> * Extra Part : Foldr, Currying Revisited
> * Part.VII Lambda Expressions ',\'
> * Part.VIII Sections
> * Part.IX Composition
> * Part.X Variables and Bindings
> * Part.XI Functions - binding
> * Part.XII Variable in a 'where' clause and binding
> * Part.XIII Functions in a 'where' clause and binding
> * Part.XIV Lambda Expressions and binding
> * Part.XV Lambda Expressions explain binding

---

[Lecture 07 on 17/October/2011]()

### Extra Part : List Comprehension - revisited

* Evaluating a list comprehension with two generators
```
[ (i,j) | i <- [1..3], j <- [i..3] ]              -- [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]

[ (i,j) | i <- [1..3], j <- [1..3], i <=j ]       -- [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]

[ (i,j) | i <- [1..3], j <- [1..3], i < j ]       -- [(1,2),(1,3),(2,3)]

[ (i,j) | i <- [1..3], j <- [1..3], i < j, * ]    -- [(1,2),(1,3),(2,3)]
```

---

## Lecture.7 : Map, Filter, Fold

### Part.I Map

* Squares, Ords (again with list comprehension)
```
squares :: [Int] -> [Int]
squares xs = [ x*x | x <- xs ]

ords :: [Char] -> [Int]
ords xs = [ord x | x <- xs]

-- Mapping out common functionality
map :: (a -> b) -> [a] -> [b]
map f xs = [ f x | x <- xs ]
```

* Squares, Ords (again with recursion)
```
squares :: [Int] -> [Int]
squares []      = []
squares (x:xs)  = x*x : squares xs

ords :: [Char] -> [Int]
ords []     = []
ords (x:xs) = ord x : ords xs

-- Mapping out common functionality
map :: (a -> b) -> [a] -> [b]
map f []      = []
map f (x:xs)  = f x : map f xs
```

* Squares, Ords revisited with map
```
squares :: [Int] -> [Int]
squares xs = map square xs
  where
  square x = x*x

ords :: [Char] -> [Int]
ords xs = map ord xs
```

Map could be either of deinitions, its already defined.
But the same pattern can be used for other functionalities.


### Part.II Filter

```
filter :: (a -> Bool) -> [a] -> [b]
filter p xs = [ x | x <- xs, p x ]
-- also
filter :: (a -> Bool) -> [a] -> [b]
filter p []                   = []
filter p (x:xs) | p x         = x : filter p xs
                | otherwise   = filter p xs
```

* Positives, Digits (list comprehension and recursion)
```
positivesComp :: [Int] -> [Int]
positivesComp xs = [ x | x <- xs , x > 0 ]

digitsComp :: [Char] -> [Char]
digitsComp xs = [ x | x <- xs, isDigit x ]


positivesRec :: [Int] -> [Int]
positivesRec []                   = []
positivesRec (x:xs) | x > 0       = x : positivesRec xs
                    | otherwise   = positivesRec xs

digitsRec :: [Char] -> [Char]
digitsRec []                      = []
digitsRec (x:xs)  | isDigit x     = x : digitsRec xs
                  | otherwise     = digitsRec xs
```

* Positives, Digits (with Filter)
```
positivesFilter :: [Int] -> [Int]
positivesFilter xs = filter positive xs
  where
  positive x = x > 0

digitsFilter :: [Char] -> [Char]
digitsFilter xs = filter isDigit xs
```


### Part.III Fold

```
foldr :: (a -> a -> a) -> a -> [a] -> a
foldr f a []      = a
foldr f a (x:xs)  = f x (foldr f a xs)

foldl :: (a -> a -> a) -> a -> [a] -> a
foldl f a []      = a
foldl f a (x:xs)  = f (foldl f a xs) x
```

* Sum, Product, Concatenate (recursion)
```
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

product :: [Int] -> Int
product [] = 1
product (x:xs) = x * product xs

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss
```

* Sum, Product, Concatenate with Foldr
```
sum :: [Int] -> Int
sum xs = foldr add 0 xs
  where
  add x y = x + y

product :: [Int] -> Int
product xs = foldr multiply 1 xs
  where
  multiply x y = x * y

concat :: [[a]] -> [a]
concat xs = foldr append 1 xs
  where
  append x y = x ++ y
```


### Part.IV Map,Filter,Fold together

* Sum of Squares of Positives
```
sumSqPositives :: [Int] -> Int
sumSqPositives xs = foldr add 0 (map square (filter positive xs))
  where
  add x y     = x + y
  square x    = x * x
  positive x  = x > 0
```


### Part.VI Currying

```
add :: Int -> Int -> Int
add x y = x + y

{-
is also

add :: Int -> (Int -> Int)
(add x) y = x + y

-- 'add' applied to 'x', applied to 'y'
-- 'add' is a function that take an 'Int' to return a function,
-- which takes an 'Int' to return an (last) 'Int'

-- say 'add 3 4' == '(add 3) 4', here '(add 3)' is a function that adds 3 to things,
-- here gets applied to '4' but could be applied to anything

-- so also
-}

add' :: Int -> (Int -> Int)
add' x = f
  where
  f y = x + y
```

* Putting curry to work
```
sum :: [Int] -> Int
sumFoldr xs = foldr add 0 xs
  where
  add x y = x + y

-- defining sum without thinking of argument at all
sumCurry :: [Int] -> Int
sumCurry = foldr add 0
  where
  add x y = x + y

-- similarly
productCurry :: [Int] -> Int
productCurry = foldr multiply 1
    where
    multiply x y = x * y

concatCurry :: [[a]] -> [a]
concatCurry = foldr append []
  where
  append xs ys = xs ++ ys
```

This type of definition allows you to think functions as functions only.
Parenthesis always lean to left in a definition if priorities match.

---

[TRIVIA: 'haskell' named after 'Haskell Curry' (1900-1982); 'curry' named after work of 'Moses Schonfinkel' (1889-1942) and 'Gottlob Frege' (1848-1925)]

[Lecture 08 on 18/October/2011]()

## Lecture.8 : Lambda and some revisited

### Extra Part : Foldr, Currying Revisited

* Using infix operators as method
```
-- with infix notation
foldr :: (a -> a -> a) -> a -> [a] -> a
foldr f a []       = a
foldr f a (x:xs)   = x `f` (foldr f a xs)

-- so things like sum
sum :: [Int] -> Int
sum xs = foldr (+) 0 xs
```

* Using currying to shorten 'sum'
```
-- there have been list comprehension, recursion, foldr... now foldr with currying
sum :: [Int] -> Int
sum = foldr (+) 0

-- similarly other in 'pointless style' i.e. without datapoints
product :: [Int] -> Int
product = foldr (*) 1

concat :: [[a]] -> [a]
concat = foldr (++) []
```

### Part.VII Lambda Expressions ',\'

* Sum of Squares of Positives with Lambda and currying
```
sumSqPositives :: [Int] -> Int
sumSqPositives xs = foldr (+) 0 (map square (filter positive xs))
  where
  square x    = x * x
  positive x  = x > 0

{-
-- though it cannot be written as
sumSqPositives xs = foldr (+) 0 (map (x*x) (filter (x>0) xs))
-- as 'x' is not known
-- but with lambdas
sumSqPositives xs = foldr (+) 0 (map (\x -> x * x) (filter (\x -> x > 0) xs))
-}

sumSqPositives xs = foldr (+) 0
                      (map (\x -> x * x)
                        (filter (\x -> x > 0) xs))
```

[TRIVIA: Lambda Calculus is due to logician Alonzo Church (1903-1995). Turing went to study with Church. JohnMcCarthy had a student of Church.]

* Lambda Expressions
```
(\x -> x>0) 3     -- True
(\x -> x*x) 3     -- 9
```

* Lambda and Currying
```
(\x -> \y -> x + y) 3 4     - 7
```

### Part.VIII Sections
```
'(> 0)' is shorthand for '(\x -> x > 0)'
'(2 *)' is shorthand for '(\x -> 2 * x)'
'(+ 1)' is shorthand for '(\x -> x + 1)'
'(2 ^)' is shorthand for '(\x -> 2 ^ x)'
'(^ 2)' is shorthand for '(\x -> x ^ 2)'
```

* Sum of Squares of Positives with Sections
```
sumSqPositives :: [Int] -> Int
sumSqPositives xs = foldr (+) 0
                      (map (^ 2)
                        (filter (> 0) xs))

```


### Part.IX Composition

* Compositions
```
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)
```

* Compare and Contrast
```
sqr :: Int -> Int
sqr x = x*x

pos :: Int -> Bool
pos x = x > 0

posSqr :: Int -> Bool
posSqr x = pos(sqr x)

posSqrComposition :: Int -> Bol
posSqrComposition = pos . sqr
```

* Thinking Functionally for 'Sum of Squares of Positives with Sections'
```
sumSqPositives :: [Int] -> Int
sumSqPositives xs = foldr (+) 0 . map (^ 2) . filter (> 0) xs)
```


### Part.X Variables and Bindings

```
x = 2       -- Binding occurence for x; can never happen again for x
y = x+1     -- Bound occurence for x; Binding occurence for y
z = x+y*y   -- Bound occurence for x&y; Binding occurence for z
-- scope of binding
```


### Part.XI Functions - binding
```
g x y = x+y*y     -- Binding of g; Scope-of-Binding of x&y is only within 'g' definition
f x = g x (x+1)   -- Binding of f; Scope-of-Binding of x within this 'f'

-- for 'f', 'x' is 'Formal paramter'
-- which gets replaced by 'Actual parameter' when called
```


### Part.XII Variable in a 'where' clause and binding
```
y = 0

f x = z
  where
  y = x + 1
  z = x + y * y

-- so variable defined in 'where' clause have Scope-of-Binding entire function definiton
-- even if there is a higher-level 'y' defined, one referred here is in-function for 'z'
-- so inner-def always shadow outer-def
```


### Part.XIII Functions in a 'where' clause and binding
```
g z =  z + 1

f x = g (x+1)
  where
    g y = x+y*y

-- variable 'x' is still in scope g
-- variable  'y' is in scope of g
-- scope of 'g' is only body of function definiton, again inner 'g' def is used
-- similarly if 'g' was also named 'f', the 'where' will still pick inner 'f' and not be recursive
```


### Part.XIV Lambda Expressions and binding
```
-- 'x' in both lambdas are different Scope-of-Binding Variables
sqPositives :: [Int] -> [Int]
sqPositives xs = map (\x -> x * x) . (filter (\x -> x > 0) xs)
```


### Part.XV Lambda Expressions explain binding
```
-- Actual:N; Formal:M

-- Variable Binding
(N where x = M) = (\x, N)M

-- Function Binding
(M where f x = N) = (M where f = \x,N)
```

* Lambda Expressions and Binding Constructs
```
f x = x+y*y
  where
    y = x+1

-- f 2
-- -- f = \x -> (x+y*y where y = x+1)
-- -- -- f = \x -> ((\y -> x+y*y) (x+1))
-- -- -- (\f -> f 2) (\x -> ((\y -> x+y*y) (x+1)))

-- so scope of a variable/function is body of Lambda Definition
```

---

> 10th Video is of ClassTest

---
---
