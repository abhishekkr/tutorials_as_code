Informatics 1 - Functional Programming
at University of Edinburgh
By Philip Wadler

Covered From Lecture11-12
* Lecture.11 Algebraic Data Types
> * Part.I Everything is an algebraic type
> * Part.II Boolean
> * Part.III Seasons
> * Part.IV Shape
> * Part.V Lists
> * Part.VI Naturals
> * Part.VII Expression Trees
* Lecture.12 Algebraic Data Types (Continued)
> * Part.VIII Propositions


---

[Lecture 11 on 31/October/2011]()

## Lecture.11 Algebraic Data Types

### Part.I Everything is an algebraic type
> data Bool = False | True
> data Season = Winter | Spring | Summer | Fall
> data Shape = Circle Float | Rectangle Float Float
> data Exp = Lit Int | Add Exp Exp | Mul Exp Exp
> data List a = Nil | Cons a (List a)
> data Nat = Zero | Succ Nat
> data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a)
> data Maybe a = Nothing | Just a
> data Pair a b = Pair a b
> data Sum a b = Left a | Right b

### Part.II Boolean
```
-- building
data Bool = False | True

not :: Bool -> Bool
not  True = False
not False = True

(&&) :: Bool -> Bool -> Bool
False && q = False
True  && q = q

(||) :: Bool -> Bool -> Bool
False && q = q
True  && q = True
```

* eq and show
```
eqBool :: Bool -> Bool -> Bool
eqBool False False  = True
eqBool False True   = False
eqBool True False   = False
eqBool True True    = True

showBool :: Bool -> String
showBool False = "False"
showBool True  = "True"
```


### Part.III Seasons

* Seasons 'next'
```
data Season = Winter | Spring | Summer | Fall

next :: Season -> Season
next Winter = Spring
next Spring = Summer
next Summer = Fall
next Fall   = Winter 
```

* Seasons - eq and show
```
eqSeason :: Season -> Season -> Bool
eqSeason Winter Winter  = True
eqSeason Spring Spring  = True
eqSeason Summer Summer  = True
eqSeason Fall   Fall    = True
eqSeason x y = False

showSeason :: Season -> String
showSeason Winter = "Winter"
showSeason Spring = "Spring"
showSeason Summer = "Summer"
showSeason Fall   = "Fall"
```

* Season and Integers
```
toInt :: Season -> Int
toInt Winter = 0
toInt Spring = 1
toInt Summer = 2
toInt Fall   = 3

fromInt :: Int -> Season
fromInt 0 = Winter
fromInt 1 = Spring
fromInt 2 = Summer
fromInt 3 = Fall  

next :: Season -> Season
next x = fromInt ((toInt x + 1) `mod` 4)

eqSeason :: Season -> Season -> Bool
eqSeason x y = (toInt x == toInt y)
```


### Part.IV Shape

* Shape
```
-- type abbreviations
type Radius = Float
type Width  = Float
type Height = Float

-- constructors can be followed by Zero or more other types
-- here Circle has 1 and Rect has 2
data Shape = Circle Radius
           | Rect Width Height

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect w h) = w * h
```

* Shape - eq and show
```
eqShape :: Shape -> Shape -> Bool
eqShape (Circle r) (Circle r')  = (r == r')
eqShape (Rect w h) (Rect w' h') = (w == w') && (h == h')
eqShape x          y            = False

showShape :: Shape -> String
showShape (Circle r) = "Circle " ++ showF r
showShape (Rect w h) = "Rect " ++ showF w ++ " " ++ showF h

showF :: Float -> String
showF x | x >= 0    = show x
        | otherwise = "(" ++ show x ++ ")"
```

* Shape - tests and selectors
```
isCircle :: Shape -> Bool
isCircle (Circle r) = True
isCircle (Rect w h) = False

isRect :: Shape -> Bool
isRect (Circle r) = False
isRect (Rect w h) = True

radius :: Shape -> Radius
radius (Circle r) = r

width :: Shape -> Width
width (Rect w h) = w

height :: Shape -> Height
height (Rect w h) = h
```

* Shape - area with Conditionals
```
area :: Shape -> Float
area s =
  if isCircle s then
    let
      r = radius s
    in
      pi * r^2
  else if isRect s then
    let
      w = width s
      h = height s
    in
      w * h
  else error "naah! not gonna happen"
```


### Part.V Lists

* Lists as Algebraic DataStructures
```
-- with declarations
data List a = Nil
            | Cons a (List a)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x (append xs ys)

-- with built-in notation
(++) :: [a] -> [a] -> [a]
[] ++ ys              = ys
(x:xs) ++ yss         = x : (xs ++ ys)
```

### Part.VI Naturals

* Naturals as Algebraic DataStructures
```
-- with names
data Nat  = Zero
          | Succ Nat

power :: Float -> Nat -> Float
power x Zero      = 1.0
power x (Succ n)  = x * power x n

-- with built-in notation -- no built-in notation of Naturals
(^^) :: Float -> Int -> Float
x ^^ 0            = 1.0
x ^^ (n+1)        = x * (x ^^ n)
```

* Naturals - add, mul
```
-- with declarations
add :: Nat -> Nat -> Nat
add m Zero      = m
add m (Succ n)  = Succ (add m n)

mul :: Nat -> Nat -> Nat
mul m Zero        = Zero
mul m (Succ n)    = add (mul m n) m

-- with built-in notation
(+) :: Int -> Int -> Int
m + 0      = m
m + (n + 1)  = (m + n) + 1

(*) :: Int -> Int -> Int
m * 0         = 0
mul m (n + 1) = (m * n) + m
```

### Part.VII Expression Trees

* Expression Trees
```
data Exp  =   Lit Int
          |   Add Exp Exp
          |   Mul Exp Exp

evalExp :: Exp -> Int
evalExp (Lit n)   = n
evalExp (Add e f) = evalExp e + evalExp f
evalExp (Mul e f) = evalExp e * evalExp f

&showExp :: Exp -> String
showExp (Lit n)   = show n
showExp (Add e f) = par (showExp e ++ "+" ++ showExp f)
showExp (Mul e f) = par (showExp e ++ "*" ++ showExp f)

par :: String -> String
par s = s "(" ++ s ++ ")"
```

* Forming Expression Trees
```
e0, e1 :: Exp
e0 = Add (Lit 2) (Mul (Lit 3) (Lit 3))    -- (2+(3*3))
e1 = Mul (Add (Lit 2) (Lit 3)) (Lit 3)    -- ((2+3)*3)
```

* Expression Trees, Infix
```
e0, e1 :: Exp
e0 = Lit 2 `Add` (Lit 3 `Mul` Lit 3)
e1 = (Lit 2 `Add` Lit 3) `Mul` Lit 3
```

> Haskell's variables may start with smallCase and Constructors with UpperCase.
> If operator begins with a colon(:) is a Cosntructor.

* Expression Trees, Symbol
```
data Exp  =   Lit Int
          |   Exp :+: Exp
          |   Exp :*: Exp

evalExp :: Exp -> Int
evalExp (Lit n)   = n
evalExp (e :+: f) = evalExp e + evalExp f
evalExp (e :*: f) = evalExp e * evalExp f
--- and similarly
```
---

[Lecture 12 on 01/November/2011]()

## Lecture.12 Algebraic Data Types (Continued)


### Part.VIII Propositions

* Propositions
```
type Name = String
data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          | Prop :&: Prop
          deriving (Eq, Ord)

type Names = [Name]
type Env   = [(Name,Bool)]
```

* Showing a Propositions
```
showProp :: Prop -> String
showProp (Var x)    = x
showProp F          = "F"
showProp T          = "T"
showProp (Not p)    = par ("^" ++ showProp p)
showProp (p :|: q)  = par (showProp p ++ "|" ++ showProp q)
showProp (p :&: q)  = par (showProp p ++ "&" ++ showProp q)

par :: String -> String
par s = "(" ++ s ++ ")"
```

* Names in Propositions
```
names :: Prop -> Names
names (Var x)    = [x]
names F          = []
names T          = []
names (Not p)    = names p
names (p :|: q)  = nub (names p ++ names q)
names (p :&: q)  = nub (names p ++ names q)
-- 'nub' takes a list and removes duplicate elements
```

* Evaluating a Proposition in an Environment
```
eval :: Env -> Prop -> Bool
eval e (Var x)      = lookUp e x
eval e F            = False
eval e T            = True
eval e (Not p)      = not (eval e p)
eval e (p :|: q)    = eval e p || eval e q
eval e (p :&: q)    = eval e p && eval e q

lookUp :: Eq a => [[a,b]] -> a -> b
lookUp xys x = the [ y | (x',y) <- xys, x == x' ]
  where
  the [x] = x
```

* Propositions, example
```
p0 :: Prop
p0 = (Var "a" :&: Var "b") :|:
     (Not (Var "a") :&: Not (Var "b"))

e0 :: Env
e0 = [("a", False), ("b", False)]

show p0         -- ((a&b)|((^a)&(^b)))
names p0        -- ["a","b"]
eval e0 p0      -- True
lookUp e0 "a"   -- False
```

* Defining Environments
```
--- all possible environments
envs :: Names -> [Env]
envs []     = [[]]
envs (x:xs) = [ (x,False):e | e <- envs xs ] ++
              [ (x,True):e | e <- envs xs ]

--- alternative
envs :: Names -> [Env]
envs []     = [[]]
envs (x:xs) = [ (x,b):e | b <- bs, e <- envs xs ]
  where
    bs = [False, True]
```

* Satisfiable
```
satisfiable :: Prop -> Bool
satisfiable p = or [ eval e p | e <- envs (names p) ]
--- even if a single env evals to True, or will True the result
```

* Simplifying a Proposition
```
isSimple :: Prop -> Bool
isSimple (Not p)        = isSimple p && not (isOp p)
  where
    isOp (Not p)    = True
    isOp (p :|: q)  = True
    isOp (p :&: q)  = True
    isOp p          = False
isSimple (p :|: q)      = isSimple p || isSimple q
isSimple (p :&: q)      = isSimple p && isSimple q
isSimple p              = True

-- to

simplify :: Prop -> Bool
simplify (Not p)        = knot (simplify p)
  where
    knot (Not p)    = p
    knot (p :|: q)  = knot p :&: knot q
    knot (p :&: q)  = knot p :|: knot q
    knot (p :&: q)  = True
    knot p          = Not p
simplify (p :|: q)      = simplify p || simplify q
simplify (p :&: q)      = simplify p && simplify q
simplify p              = p


p2 :: Prop
p2 = Not p0

showProp p2               -- (^((a&b)|((^a)&(^b))))
isSimple p2               -- False
isSimple (simplify p2)    -- True
```

* Using QuickCheck to check Proposition
```
import Data.Monad
import Test.QuickCheck

-- allow QuickCheck to generate arbitrary values of type Prop
instance Arbitrary Prop where
  arbitrary = sized prop
    where
        prop 0 =
          oneof [return F,
                 return T,
                 liftM Var arbitrary]
        prop n | n > 0 =
          oneof [return F,
                 return T,
                 liftM Var arbitrary,
                 liftM Not (prop (n-1)),
                 liftM2 (:&:) (prop (div n 2) (div n 2)),
                 liftM2 (:|:) (prop (div n 2) (div n 2))]
```

* Checking
```
prop_simplify :: Prop -> Bool
prop_simplify p = isSimple (simplify p)

prop_eval_simplify :: Prop -> Bool
prop_eval_simplify p =
  and [ eval e p == eval e (simplify p)
      | e <- envs (names p)]
```


---
---
