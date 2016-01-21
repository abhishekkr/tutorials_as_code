Informatics 1 - Functional Programming
at University of Edinburgh
By Philip Wadler

Covered From Lecture15-16


---

[Lecture 15 on 14/November/2011]()

## Lecture.15 : Type Classes

* Element, defined to work only for types which can have equality

```
elem :: Eq a => a -> [a] -> Bool

-- comprehension
elem x ys     = or [ x == y | y <- ys ]

-- recursion
elem x []     = False
elem x (y:ys) = x == y || elem x ys

-- higher-order
elem x ys     = foldr (||) False (map (x ==) ys)
```
Need Equality to define it, in any structure.

So wouldn't work for something like
```
elem (\x -> x) [(\x -> -x), (\x -> -(-x))]
-- No instance for (Eq (a -> a))
```


* Equality Type Class

```
class Eq a where
  (==) :: a -> a -> Bool

instance Eq Int where
  (==) = eqInt

instance Eq Char where
  x == y                =   ord x == ord y

instance (Eq a, Eq b) => Eq (a,b) where
  (u,v) == (x,y)        =   (u == x) && (v == y)

instance Eq a => Eq [a] where
  [] == []              = True
  [] == y:ys            = False
  x:xs == []            = False
  x:xs == y:ys          = (x == y) && (xs == ys)
```


* Element, translation
```
data EqDict a     = EqD (a -> a -> Bool)

eq :: EqDict a -> a -> a -> Bool
eq (EqDict f)     = f

elem :: EqD a -> a -> [a] -> Bool

-- comprehension
elem d x ys       = or [ eq d x == y | y <- ys ]

-- recursion
elem d x []       = False
elem d x (y:ys)   = eq d x y || elem x ys

-- higher-order
elem d x ys       = foldr (||) False (map (eq d x) ys)
```
> Can define TypeClasses in Haskell by giving a translation into Haskell without type classes.
> 'EqDict a' is an equality dictionary, an equality function packaged up into a new type.
> 
> eq extracts the equality function from an equality dictionary
> We can then define elem with an extra argument 'd' denoting how to compute equality on 'a'.
> Instead of 'x == y', we write 'eq d x y'


* TypeClasses, translation
```
dInt      :: EqDict Int
dInt      = EqD eqInt

dChar     :: EqDict Char
dChar     = EqD f
  where
  f x y   = eq dInt (ord x) (ord y)

dPair     :: (EqDict a, EqDict b) -> EqDict (a,b)
dPair (da,db)     = EqD f
  where
  f (u,v) (x,y)   = eq da u x && eq db v y

dList     :: EqDict a -> EqDict [a]
dList d           = EqD f
  where
  f [] []         = True
  f [] (y:ys)     = False
  f (x:xs) []     = False
  f (x:xs) (y:ys) = eq d x y && eq (dList d) xs ys

-- build up dictionaries with other dictionaries
-- each INSTANCE declaration creates a dictionary
```


* Using 'element', translation

```
elem dInt 1 [2,3,4]       -- False

elem dChar 'o' "word"     -- True

elem (dPair dInt dChar) (1, 'o') [(0,'w'),(1,'o')]    -- True

elem (dList dChar) "word" ["list","of","word"]        -- True
```
> Uses of 'elem' then require the appropriate dictionary as an explicit argument.
> Haskell does all of this automatically.



### Part.II - Eq, Ord, Show

> Eq, Ord and Show are built-in type classes.

* Eq
```
-- Eq has 2 functions, == & /=
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

-- minimum definition: (==)
x /= y = not (x == y)
-- you can define a default for some functions, instances can override defaults
```

* Ord
```
-- Ord extends Eq; default definition of '<' requires 'Equality'
class (Eq a) => Ord a where
  (<)     :: a -> a -> Bool
  (<=)    :: a -> a -> Bool
  (>)     :: a -> a -> Bool
  (>=)    :: a -> a -> Bool

-- minimum definition: (<=)
x < y   = x <= y && x/= y
x > y   = y < x
x >= y  = y <= x
```

* Show
```
-- need a way of converting value to String
class Show a where
  show :: a -> String
```


### Part.III Boolean, Tuples, List

* Instance for Booleans
```
instance Eq Bool where
  False == False = True
  False == True  = False
  True  == False = False
  True  == True  = True

instance Ord Bool where
  False <= False = True
  False <= True  = True
  True  <= False = False
  True  <= True  = True

instance Show Bool where
  show False = "False"
  show True  = "True"
```

* Instance for Tuples
```
instance (Eq a, Eq b) => Eq (a,b) where
  (x,y) == (x',y')    = x == x' && y == y'

instance (Ord a, Ord b) => Ord (a,b) where
  (x,y) <= (x', y')   = x < x' || (x == x' && y <= y')

instance (Show a, Show b) => Show (a,b) where
  show (x,y)          = "(" ++ show x ++ "," ++ show y ++ ")"
```

* Instance of List
```
instance Eq a => Eq [a] where
  []    ==    []            = True
  []    ==    y:ys          = False
  x:xs  ==    []            = False
  x:xs  ==    y:ys          = x == y && xs == ys

instance Ord a => Ord [a] where
  []    <=    y:ys          = True
  x:xs  <=    []            = False
  x:xs  <=    y:ys          = x < y || (x == y && xs <= ys)
  

instance Show a => Show [a] where
  show []                   = "[]"
  show (x:xs)               = "[" ++ showSep x xs ++ "]"
    where
      showSep x []      = show x
      showSep x (y:ys)  = show x ++ "," ++ showSep y ys
```

* Deriving Clauses
```
-- Haskell uses types to write code for you
-- can get definitions of instances of Eq, Ord, Show for free for algebraic types

data Bool = False | True
  deriving (Eq, Ord, Show)

data Pair a b = MkPair a b
  deriving (Eq, Ord, Show)

data List a = Nil | Cons a (List a)
  deriving (Eq, Ord, Show)
```

---

[Lecture 16 on 15/November/2011]()


### Patr.IV Sets, revisited

```
-- this differs from derived instance
instance Ord a => Eq (Set a) where
  s == t = s `equal` t
```


### Part.V Numbers

* Numerical Classes
```
class (Eq a, Show a) => Num a where
  (+),(-),(*)   ::  a -> a -> a
  negate        ::  a -> a
  fromInteger   ::  Integer -> a
  -- minimum definition
  negate x    = fromInteger 0 - x

class (Num a) => Fractional a where
  (/)           ::  a -> a -> a
  recip         ::  a -> a
  fromRational  ::  Rational -> a
  -- minimum definition
  recip       = 1/x

class (Num a, Ord a) => Real a where
  toRelational  :: a -> Rational

class (Real a, Enum a) => Integral a where
  div, mod      :: a -> a -> a
  toInteger     :: a -> Integer
```

* A built-in numerical type
```
instance Num Float where
  (+)             =   builtInAddFloat
  (-)             =   builtInSubtractFloat
  (*)             =   builtInMultiplyFloat
  negate          =   builtInNegateFloat
  fromInteger     =   builtInFromIntegerFloat

instance Fractional Float where
  (/)             =   builtInDivideFloat
  fromRational    =   builtInFromRationalFloat
```

* Naturals - let's define our own numerical types
```
module Natural(Nat) where

import Test.QuickCheck

data Nat = MkNat Integer    -- abstraction, introducing a non-exported Constructor
{-
if defining more than one Constructor
or a Constructor with more than one field
then must use 'data'
here
'newtype Nat = MkNat Integer'
would also work
-}

invariant :: Nat -> Bool
invariant (MkNat x)   =   x >= 0

instance Eq Nat where
  MkNat x == MkNat y  =   x == y 

instance Ord Nat where
  MkNat x <= MkNat y  =   x <= y

instance Show Nat where
  show (MkNat x)      =   show x

instance Num Nat where
  MkNat x + MkNat y   = MkNat (x + y)
  MkNat x - MkNat y
    | x >= y          = MkNat (x - y)
    | otherwise       = error (show (x-y) ++ " is negative")
  MkNat x * MkNat y   = MkNat (x * y)
  fromInteger x
    | x >= 0          = MkNat x
    | otherwise       = error (show x ++ "is negative")
  negate              = undefined

-- check

prop_plus :: Integer -> Integer -> Property
prop_plus m n =
  (m >= 0) && (n >= 0) ==> (m+n >= 0)

prop_times :: Integer -> Integer -> Property
prop_times m n =
  (m >= 0) && (n >= 0) ==> (m*n >= 0)

prop_minus :: Integer -> Integer -> Property
prop_minus m n =
  (m >= 0) && (n >= 0) && (m >= n) ==> (m-n >= 0)
```

Using it
```
module TestNatural where
import Natural

m,n :: Nat
m = fromInteger 2
n = fromInteger 3
```


### Part.VI Seasons

* Seasons
```
data Season = Winter | Spring | Summer | Fall

-- 'next' same as defined earlier

warm :: Season -> Bool
warm Winter = False
warm _      = True

-- defining Eq,Ord,Show

instance Eq Season where
  Winter == Winter    = True
  Spring == Spring    = True
  Summer == Summer    = True
  Fall   == Fall      = True
  _      == _         = False

instance Ord Season where
  Spring <= Winter    = False
  Summer <= Winter    = False
  Fall   <= Winter    = False
  Summer <= Spring    = False
  Fall   <= Spring    = False
  Fall   <= Summer    = False
  _      <= _         = True

instance Show Season where
  show Winter = "Winter"
  show Spring = "Spring"
  show Summer = "Summer"
  show Fall   = "Fall"
```

##### Enum Class

* Enum class

```
class Enum a where
  toEnum          ::  Int -> a
  fromEnum        :: a -> Int
  succ, pred      :: a -> a
  enumFrom        :: a -> [a]               -- [x..]    -- these are
  enumFromTo      :: a -> a -> [a]          -- [x..y]   -- syntactic sugar
  enumFromThen    :: a -> a -> [a]          -- [x,y..]
  enumFromThenTo  :: a -> a -> a -> [a]     -- [x,y..z]

  -- minmum definition
  succ x          = toEnum (fromEnum x + 1)
  pred x          = toEnum (fromEnum x - 1)
  enumFrom x
    = map toEnum [fromEnum x ..]
  enumFromTo x y
    = map toEnum [fromEnum x .. fromEnum y]
  enumFromThen x y
    = map toEnum [fromEnum x, fromEnum y ..]
  enumFromThenTo x y z
    = map toEnum [fromEnum x, fromEnum y .. fromEnum z]
```

* Enumerating Int

```
instance Enum Int where
  toEnum x              = x
  fromEnum x            = x
  succ x                = x+1
  pred x                = x-1
  enumFrom x            = iterate (+1) x
  enumFromTo x y        = takeWhile (<= y) (iterate (+1) x)
  enumFromThen x y      = iterate (+(y-x)) x
  enumFromThenTo x y z  = takeWhile (<= z) (iterate (+(y-x)) x)

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p []              = []
takeWhile p (x:xs)  | p x         = x : takeWhile p xs
                    | otherwise   = []
```

> Now we can declare Int as an instance of Enum


* Enumerating Season

```
-- Season defined as an instance of Enum
instance Enum Season where
  fromEnum Winter = 0
  fromEnum Spring = 1
  fromEnum Summer = 2
  fromEnum Fall   = 3

  toEnum 0 = Winter
  toEnum 1 = Spring
  toEnum 2 = Summer
  toEnum 3 = Fall
```

* Deriving Seasons
```
-- using Types to do the work
data Season = Winter | Spring | Summer | Fall
              deriving (Eq, Ord, Show, Enum)
```

* Seasons, revisited
```
next :: Season -> Season
next x = toEnum  ((fromEnum x + 1) `mod` 4)

warm :: Season -> Bool
warm x = x `elem` [Spring .. Fall]

--- [Spring .. Fall] =[Spring, Summer, Fall]
```


### Part.VII Shape

* Shape
```
type Radius   = Float
type Width    = Float
type Height   = Float

data Shape  = Circle Radius
            | Rect Width Height

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect w h) = w * h
```

* Eq, Ord, Show for Shape
```
-- Shape an instance of Eq, Ord and Show
instance Eq Shape where
  Circle r == Circle r'   = r == r'
  Rect w h == Rect w' h'  = w == w' && h == h'
  _        == _           = False

instance Ord Shape where
  Circle r <= Circle r'     = r < r'
  Circle r <= Rect w' h'    = True
  Rect w h <= Rect w' h'    = w < w' || (w == w' && h <= h')
  _        <= _             = False

instance Show Shape where
  show (Circle r)           = "Circle " ++ showN r
  show (Rect w h)           = "Rect " ++ showN w ++ " " ++ showN h

showN :: (Num a) => a -> String
showN x | x >= 0      = show x
        | otherwise   = "(" ++ show x ++ ")"
```


### Part.VIII Expressions

* Expression Trees
```
data Exp  = Lit Int
          | Exp :+: Exp
          | Exp :*: Exp

eval :: Exp -> Int
eval (Lit n)      = n
eval (e :+: f)    = eval e + eval f
eval (e :*: f)    = eval e * eval f
```

usage
```
*Main> eval (Lit 2 :+: (Lit 3 :*: Lit 3))
11
*Main> eval ((Lit 2 :+: Lit 3) :*: Lit 3)
15
```

* Exp as an instance of Eq, Ord, Show
```
instance Eq Exp where
  Lit n == Lit n'         = n == n'
  e :+: f == e' :+: f'    = e == e' && f == f'
  e :*: f == e' :*: f'    = e == e' && f == f'
  _       == _            = False

instance Ord Exp where
  Lit n <= Lit n'         = n < n'
  Lit n <= Lit n'         = True
  Lit n <= Lit n'         = True
  e :+: f <= e' :+: f'    = e < e' || (e == e' && f <= f')
  e :+: f <= e' :*: f'    = True
  e :*: f <= e' :*: f'    = e < e' || (e == e' && f <= f')
  _     <= _              = False

instance Ord Exp where
  show (Lit n)            = "Lit " ++ showN n
  show (e :+: f)          = "(" ++ show e ++ ":+:" ++ show f ++ ")"
  show (e :*: f)          = "(" ++ show e ++ ":*:" ++ show f ++ ")"
```

* Deriving Expressions
```
data Exp  = Lit Int
          | Exp :+: Exp
          | Exp :*: Exp
          deriving (Eq, Ord, Show)
```


### Extra Part

* Sample-Lecture17
```
data Nat = Zero | Succ Nat deriving (Eq,Ord,Show)

test =
  let
    add = \x -> \y -> case x of
                        Zero    -> y
                        Succ u  -> Succ (add u y)

  in
    let
      two = Succ (Succ Zero)
    in
      add two two
```


* Micro-Haskell (Haskell in Haskell)

> Writing a Micro-Haskell to write the above 'test' logic in that language
> * Code: [lecture16.example00.hs](./lecture16.example00.hs)


---
---
