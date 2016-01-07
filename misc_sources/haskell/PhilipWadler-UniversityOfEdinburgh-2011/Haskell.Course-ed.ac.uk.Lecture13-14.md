Informatics 1 - Functional Programming
at University of Edinburgh
By Philip Wadler

Covered From Lecture13-14
* Lecture.13 Algebraic Types and Type Classes
> * Part.I Complexity
> * Part.II Sets as list (without Abstractions)
> * Part.III Sets as Ordered list (without Abstractions)
* Lecture.13 Algebraic Types and Type Classes (Continued)
> * Part.IV Sets as Trees (without Abstractions)
> * Part.V Sets as Balanced Trees (without Abstractions)
> * Part.VI Complexity, Revisited
> * Part.VII Data Abstraction


---

[Lecture 13 on 07/November/2011]()

> Data Abstraction, most important after Lambda Expressions.
> Giving capability to optimize when required.

## Lecture.13 Algebraic Types and Type Classes

### Part.I Complexity

> O(1), O(log n), O(n), O(n log n), O(n^2)


### Part.II Sets as list (without Abstractions)

* Sets as list (without Abstractions)
```SetList.hs
module SetListNotAbstract(Set, element, nil, insert, set, equal, check) where

data Set a = [a]

nil :: Set a
nil = []

insert :: a -> Set a -> Set a
insert x xs = x:xs                             -- O(1)

set :: [a] -> Set a
set xs = xs                                               -- O(1)

element :: Eq a => a -> Set a -> Bool                           -- O(n)
x `element` xs  = x `elem` xs

equal :: Eq a => Set a -> Set a -> Bool
xs `equal` ys = xs `subset` ys && ys `subset` xs
  where
    xs `subset` ys = and [ x `elem` ys | x <- xs ]              -- O(n^2)

showRep :: Show a => Set a -> String
showRep xs = "Set " ++ show xs

test0 :: Bool
test0 =
  2 `element` s0 &&
  not (3 `element` s0) &&
  showRep s0 == "Set [2,7,1,8,2,8]"
  where
      s0 = set [2,7,1,8,2,8]

--- check

prop_element :: [Int] -> Bool
prop_element ys =
  and [ x `element` s == odd x | x <- ys ]
  where
    s = set [ x | x <- ys, odd x ]

check =
  quickCheck prop_element
```

* Using the 'Sets as List without Abstraction' as module
```
module TestSetList where
import SetListNotAbstract

test :: Int -> Bool
test n =
  s `equal` t
  where
  s = set [1,2..n]
  t = set [n,n-1..1]

-- checking to see if these are equal, comes true

-- since set<->list, can use List functions
-- breakAbstraction
setHead :: Set a -> a
setHead = head

-- 'head' isn't a function on Sets but would work
-- and would give false result for equal Sets if unordered
```


### Part.III Sets as Ordered list (without Abstractions)

* Sets as OrderedList
```
module SetOrderedListNotAbstract(Set, element, nil, insert, set, equal, check) where

import Data.List (nub,sort)
import Test.QuickCheck

data Set a = [a]

invariant :: Ord a => Set a -> Bool
invariant xs =
  and [x < y | (x,y) <- zip xs (tail xs)]

nil :: Set a
nil = []

insert :: Ord a => a -> Set a -> Set a                        -- O(n)
insert x []                = [x]
insert x (y:ys)  | x < y   = x : y : ys
                 | x == y  = y : ys
                 | x > y   = y : insert x ys

set :: Ord a => [a] -> Set a
set = nub (sort xs)               -- O(n log-n)

set' :: Ord a => [a] -> Set a
set' = foldr insert nil           -- O(n log-n)


element :: Ord a => a -> Set a -> Bool                        -- O(n)
x `element` xs  = x `elt` xs
  where
  x `elt` []                    = False
  x `elt` (y:ys) | x < y        = False
                 | x == y       = True
                 | x > y        = x `elt` ys

equal :: Eq a => Set a -> Set a -> Bool                       -- O(n)
xs `equal` ys = xs == ys

showRep :: Show a => Set a -> String
showRep xs = "Set " ++ show xs

-- another definition
element' :: Ord a => a -> Set a -> Bool
element' x xs = not (null ys) && x == head ys
  where
    ys = dropWhile (< x) xs

insert' :: Ord a => a -> Set a -> Set a
insert' x xs =
  ys ++ [x | null zs || x /= head zs ] ++ zs
  where
    ys = takeWhile (< x) xs
    zs = dropWhile (< x) xs


test0 :: Bool
test0 =
  2 `element` s0 &&
  not (3 `element` s0) &&
  showRep s0 == "Set [2,7,1,8,2,8]"
  where
      s0 = set [2,7,1,8,2,8]

-- checks

prop_invariant :: [Int] -> Bool
prop_invariant xs = invariant (set xs)

prop_element :: [Int] -> Bool
prop_element ys =
  and [ x `element` s == odd x | x <- ys ]
  where
    s = set [ x | x <- ys, odd x ]

prop_element' :: [Int] -> Bool
prop_element' ys =
  and [ x `element` s == x `element'` s ]

prop_insert' :: [Int] -> Bool
prop_insert' ys =
  and [ x `insert` s == x `insert'` s ]

check =
  quickCheck prop_invariant >>
  quickCheck prop_element
```

* Using the 'Sets as List without Abstraction' as module
```
module TestSetOrderedList where
import SetOrderedListNotAbstract

test :: Int -> Bool
test n =
  s `equal` t
  where
  s = set [1,2..n]
  t = set [n,n-1..1]

-- since set<->list, can use List functions
setHead :: Set a -> a
setHead = head

-- but now running set's equal on List will break, it will run but erroneous
-- breakAbstraction
badtest :: Int -> Bool
badtest n =
  s `equal` t
  where
  s = [1,2..n]    -- no call to set
  t = [n,n-1..1]  -- no call to set
```

---

[Lecture 14 on 08/November/2011]()


### Part.IV Sets as Trees (without Abstractions)

* Sets as OrderedTrees
> if done properly membership goes from O(n) to O(log-n)

```
module SetTreeNotAbstract
  (Set (Nil,Node), null, insert, set, element, equal, check) where
import Test.QuickCheck

-- a set is a tree either Empty(Nil) or a node with data value having left and right subtree
data  Set a = Nil | Node (Set a) a (Set a)

list :: Set a -> [a]
list Nil            = []
list (Node l x r)   = list l ++ [x] ++ list r       -- in-order traversal

-- says every node has lesser value send to Left subtree and greater to Right subtree
invariant :: Ord a => Set a -> Bool
invariant Nil           = True
invariant (Node l x r)  =
  invariant l && invariant r &&
  and [ y < x | y <- list l ] &&
  and [ y > x | y <- list r ]

null :: Set a
null = Nil

insert :: Ord a => a -> Set a -> Set a
insert x Nil           = Node Nil x Nil
insert x (Node l y r)
  | x == y             = Node l y r
  | x < y              = Node (insert x l) y r
  | x > y              = Node l y (insert x r)

-- converting list to a tree
set :: Ord a => [a] -> Set a
set = foldr insert null

-- at each node we can ignore a subtree for element
-- O(log-n)
element :: Ord a => a -> Set a -> Bool
x `element` Nil     = False
x `element` (Node l y r)
  | x == y             = True
  | x < y              = x `element` l
  | x > y              = x `element` r

-- O(n)
equal :: Ord a => Set a -> Set a -> Bool
s `equal` t = list s == list t

--- check

prop_invariant :: [Int] -> Bool
prop_invariant xs = invariant s
  where
    s = set xs

prop_element :: [Int] -> Bool
prop_element ys =
  and [ x `element` s == odd x | x <-- ys ]
  where
  s = set [ x | x <- ys, odd x ]

check =
  quickCheck prop_invariant >>
  quickCheck prop_element
```

* Using the 'Sets as Tree without Abstraction' as module
```
module TestSetTreeNotAbstract where
import SetTreeNotAbstract

test :: Int -> Bool
test n =
  s `equal` t
  where
  s = set [1,2..n]
  t = set [n,n-1..1]

-- breakAbstraction, gets equal to give wrong answer
badtest :: Int -> Bool
badtest =
  s `equal` t
  where
  s = set [1,2,3]
  t = Node (Node  Nil 3 Nil) 2 (Node Nil 1 Nil)       -- breaks invariant
```


### Part.V Sets as Balanced Trees (without Abstractions)

> AVL Trees (Invented 1962 by Adelson-Velski and Landis)

* Sets as Balanced Trees
```
module BalancedSetTreeNotAbstract
  (Set (Nil,Node), null, insert, set, element, equal, check) where
import Test.QuickCheck

type Depth  = Int
data  Set a = Nil | Node (Set a) a (Set a) Depth

node :: Set a -> a -> Set a -> Set a
node l x r = Node l  x r (1 + (depth l `max` depth r))

depth :: Set a -> Int
depth Nil            = 0
depth (Node _ _ _ d) = d

list :: Set a -> [a]
list  Nil                = []
list (Node l x r _)      = list l ++ [x] ++ list r

invariant :: Ord a => Set a -> Bool
invariant Nil                 = True
invariant (Node l x r d)      =
  invariant l && invariant r &&
  and [ y < x | y <- list l ] &&
  and [ y > x | y <- list r ] &&
  abs (depth l - depth r) <= 1 &&
  d == 1 + (depth l `max` depth r)

null :: Set a
null = Nil

insert :: Ord a => a -> Set a -> Set a
insert x Nil       = node empty empty
insert x (Node l y r _)
  | x == y    = node l y r
  | x <  y    = rebalance (node (insert x l) y r)
  | x >  y    = rebalance (node l y (insert x r))

set :: Ord a => [a] -> Set a
set   = foldr insert null

-- rebalancing
--- unbalanced.1 -- Node (Node a x b) y c               -->   Node a x (Node b y c)
--- unbalanced.2 -- Node (Node a x (Node b y c) z d)    -->   Node (Node a x b) y (Node c z d)
rebalance :: Set a -> Set a

rebalance (Node (Node a x b _) y c _)                 -- unbalanced.1
  | depth a >= depth b && depth a > depth c
  = node a x (node b y c)

rebalance (Node a x (Node b y c _) _)                 -- unbalanced.1
  | depth c >= depth b && depth c > depth a
  = node (node a x b) y c

rebalance (Node (Node a x (Node b y c _) _) z d _)    -- unbalanced.2
  | depth (node b y c) > depth d
  = node (node a x b) y (node c z d)

rebalance (Node a x (Node (Node b y c _) z d _) _)    -- unbalanced.2
  | depth (node b y c) > depth a
  = node (node a x b) y (node c z d)

rebalance a = a


element :: Ord a => a -> Set a -> Bool
x `element` Nil             = False
x `element` (Node l y r _)                    -- O(log-n) as balanced tree
  | x == y  = True
  | x <  y  = x `element` l
  | x >  y  = x `element` r


equal :: Ord a => Set a -> Set a -> Bool
s `equal` t  = list s == list t

-- check

prop_invariant :: [Int] -> Bool
prop_invariant xs = invariant (set xs)

prop_element :: [Int] -> Bool
prop_element ys =
  and [ x `element` s == odd x | x <- ys ]
  where
  s = set [ x | x <- ys, odd x ]

check =
  quickCheck prop_invariant >>
  quickCheck prop_element
```


* Using the 'Sets as Tree without Abstraction' as module
```
module TestBalancedSetTreeNotAbstract where
import BalancedSetTreeNotAbstract

test :: Int -> Bool
test n =
  s `equal` t
  where
  s = set [1,2..n]
  t = set [n,n-1..1]

-- breakAbstraction, still can break invariant
badtest :: Int -> Bool
badtest =
  s `equal` t
  where
  s = set [1,2,3]
  t = (Node Nil 1 (Node Nil 2 (Node Nil 3 Nil 1) 2) 3)

```

### Part.VI Complexity, Revisited

```
  --------------------------------------------------------------------
  --------------------------------------------------------------------
  |Structure    |  insert    |     set    |   element    |  equal    |
  --------------------------------------------------------------------
  --------------------------------------------------------------------
  |List         | O(1)       |  O(1)      |  O(n)        | O(n^2)    |
  |             |            |            |              |           |
  --------------------------------------------------------------------
  |OrderedList  | O(n)       |  O(n log-n)|  O(n)        | O(n)      |
  |             |            |            |              |           |
  --------------------------------------------------------------------
  |Tree         |avg:O(log-n)|avg:O(nlogn)|avg:O(log-n)  |   O(n)    |
  |             |wst:O(n)    |wst: O(n^2) |wst: O(n)     |           |
  --------------------------------------------------------------------
  |BalancedTree |  O(log-n)  | O(n log-n) |  O(log n)    | O(n)      |
  |             |            |            |              |           |
  --------------------------------------------------------------------
  --------------------------------------------------------------------

```


### Part.VII Data Abstraction

> Use 'Data Constructors' and carefully denote who gets to use them.

* List with Abstraction
```SetList.hs
module SetListWithAbstraction
  (Set, element, nil, insert, set, equal, check) where

import Test.QuickCheck

newtype Set a = MkSet [a]   -- including Constructor 'MkSet'

nil :: Set a
nil = MkSet []

insert :: a -> Set a -> Set a
insert x (MkSet xs) = MkSet (x:xs)`                             -- O(1)

set :: [a] -> Set a
set xs = MkSet xs                                               -- O(1)

element :: Eq a => a -> Set a -> Bool                           -- O(n)
x `element` (MkSet xs)  = x `elem` xs

equal :: Eq a => Set a -> Set a -> Bool
MkSet xs `equal` MkSet ys = xs `subset` ys && ys `subset` xs
  where
    xs `subset` ys = and [ x `elem` ys | x <- xs ]              -- O(n^2)

showRep :: Show a => Set a -> String
showRep (MkSet xs) = "MkSet " ++ show xs

test0 :: Bool
test0 =
  2 `element` s0 &&
  not (3 `element` s0) &&
  showRep s0 == "MkSet [2,7,1,8,2,8]"
  where
      s0 = set [2,7,1,8,2,8]

--- check

prop_element :: [Int] -> Bool
prop_element ys =
  and [ x `element` s == odd x | x <- ys ]
  where
    s = set [ x | x <- ys, odd x ]

check =
  quickCheck prop_element
```


* Using the 'Sets as List with Abstraction' as module
```
module TestSetListWithAbstraction where

import SetListWithAbstraction
import Test.QuickCheck

test :: Int -> Bool
test n =
  s `equal` t
  where
  s = set [1,2..n]
  t = set [n,n-1..1]

-- no longer type-checks
-- breakAbstraction :: Set a -> a
-- breakAbstraction = head
```

* Hiding the Secret of Abstraction, not exporting it

When exported in head-definiton
```
module SetListWithAbstraction
  (Set, element, nil, insert, set, equal, check) where
```

and not
```
module SetListWithAbstraction
  (Set (MkSet), element, nil, insert, set, equal, check) where
```

So MkSet doesn't get exported and nothing else gets hold of it.
Gurantees none breaks abstraction.

Similarly to abtract Tree, don't export 'Node,Nil' as here
```
module BalancedSetTreeNotAbstract
  (Set (Nil,Node), null, insert, set, element, equal, check) where
```

but rather
```
module BalancedSetTreeNotAbstract
  (Set, null, insert, set, element, equal, check) where
```


* Preserving the invariant
> You can ensure that invariant holds by checking it holds for all functions in the module that produce values of type Set.

```
module TestSetListWithAbstraction where

import SetListWithAbstraction
import Test.QuickCheck

prop_invariant_nil = invariant nil

prop_invariant_insert x s =
  invariant s ==> invariant (insert x s)

prop_invariant_set xs = invariant (set xs)

check =
  quickCheck prop_invariant_nil >>
  quickCheck prop_invariant_insert >>
  quickCheck prop_invariant_set
```


---
---
