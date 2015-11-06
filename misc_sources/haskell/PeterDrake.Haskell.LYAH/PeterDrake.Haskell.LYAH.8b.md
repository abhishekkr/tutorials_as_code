
### 8b. Defining TypeClasses

* code sample to be shown usage at

```
import Data.List
import Data.Maybe

data Quad a = Quad a a a a

instance (Show a) => Show (Quad a) where
  show (Quad a b c d) = (show a) ++ " " ++ (show b) ++ "\n" ++
                        (show c) ++ " " ++ (show d)

instance Functor Quad where
  fmap f (Quad a b c d) = Quad (f a) (f b) (f c) (f d)

class Floppable a where
  flop :: a -> a

instance Floppable (Quad a) where
  flop (Quad a b c d) = Quad a c b d

instance Floppable [a] where
  flop = reverse

data Expression t = Literal t | Variable String | Operation String [Expression t]

instance (Show t) => Show (Expression t) where
  show (Literal n) = show n
  show (Variable x) = x
  show (Operation op rands) = "(" ++ (concat (intersperse "" (op : (map show rands)))) ++ ")"

type AssocList k v = [(k,v)]

evalF :: (Floating t) => Expression t -> AssocList String t -> t
evalF (Literal n) table = n
evalF (Variable x) table = fromJust (lookup x table)
evalF (Operation op rands) table = case op of
                                       "+" -> a + b
                                       "-" -> a - b
                                       "*" -> a * b
                                       "/" -> a / b
                                       "log" -> log a
                                       where a = evalF (rands !! 0) table
                                             b = evalF (rands !! 1) table


evalB :: Expression Bool -> [(String, Bool)] -> Bool
evalB (Literal n) table = n
evalB (Variable x) table = fromJust (lookup x table)
evalB (Operation op rands) table = case op of
                                       "and" -> a && b
                                       "or" -> a || b
                                       "not" -> not a
                                       where a = evalB (rands !! 0) table
                                             b = evalB (rands !! 1) table
```

---

* usage

```
*Main> 
*Main> Quad 1 2 3 4
1 2
3 4
*Main> Quad (+) (-) (*) (/)

<interactive>:24:1:
    No instance for (Show (a0 -> a0 -> a0))
      (maybe you haven't applied enough arguments to a function?)
      arising from a use of ‘print’
    In the first argument of ‘print’, namely ‘it’
    In a stmt of an interactive GHCi command: print it
*Main> 
*Main> fmap (*10) (Quad 1 2 3 4)
10 20
30 40
*Main>
*Main> let q = Quad 1 2 3 4
*Main> q
1 2
3 4
*Main> flop q
1 3
2 4
*Main> flop [1,2,3,4]
[4,3,2,1]
*Main>
*Main> Operation "+" [(Operation "*" [(Literal 2), (Variable "x")]), (Literal 1)]
(+(*2x)1)
*Main>
*Main> evalF (Operation "+" [(Operation "*" [(Literal 2), (Variable "x")]), (Literal 1)]) [("x", 4), ("y",6)]
9.0
*Main> evalF (Operation "log" [Literal 10]) []
2.302585092994046
*Main> 
*Main> evalB (Operation "or" [(Operation "and" [(Literal True), (Variable "x")]), (Literal True)]) [("x",True), ("y",False)]
True
*Main> 

```

---
