-- https://prajitr.github.io/quick-haskell-syntax
-- this how comment looks

{-
and this is Block Comment
-}

--- | run ghci for repl
--- | load existing haskell script with ':l haskellScript.hs'

{-

7 -- numbers
3.0
True -- booleans
False 
'a' -- characters
'z'
"Believe in Church, use Lambda." -- strings
[1, 2, 3] -- lists

7 + 3               -- 10
10 - 3              -- 7
7 * 3               -- 21
7 / 3               -- 2.3333333333333335

7 `div` 3           -- 2

not True            -- False ('not' is a function)
1 == 10             -- False
1 /= 10             -- True (this is 'Not Equals')
True && False       -- False
True || False       -- True
10 > 1              -- True
10 <= 1             -- False

True                -- []
'C' : "at"          -- Cat
1 : 2 : 3 : []      -- [1,2,3]

-}

-- | defining basic id function
id :: a -> a
id x = x

-- | signature's last '-> type' is return type
pickFirst :: a -> b -> a
pickFirst x y = x

-- | few more functions, actually doing something
concat3Str :: String -> String -> String -> String
concat3Str x y z = x ++ y ++ z

-- | this function restricts domain of types 'a' that belong to class Eq
all3Equal :: (Eq a) => a -> a -> a -> Bool
all3Equal x y z = (x == y) && (y == z)

-- | anonymous functions
-- | (\x -> x + x)
-- | (\x y -> x + y)
-- | leading with '\' then arguments delimited by space and then computation past '->
tenPlusFnPassed5 = (\x -> 10 + x) 5

-- | since functions are first class
fOFgOFx :: (String -> String) -> (String -> String) -> String -> String
fOFgOFx f g x = f x ++ "-" ++ g x
-- | fOFgOFx head last "abcd"
-- | alternatively composition function
f . g = (\x -> f (g x))

{-
($) :: (a -> b) -> a -> b
f $ x = f x

-- '$' is used not for computation but reducing amount of parenthesis needed
-- 'not $ 3 < 5' is same as 'not (3 < 5)'
-}

-- | some short 'currying' samples
add x y = x + y
add3 = add 3
threePlusSeven = add3 7              -- 10

times10 = (*) 10
times5 = (*) 5
times2 = (*) 2
timesTwoTimesFiveEqualsTimesTen = (times2 $ times5 10) == (times10 10)


-- | using local variables within function definition via 'where...' & 'let...in'
timesTwoTimesFiveEqualsTimesTenWithLetIn =
    let timesTwoTimesFive = (times2 $ times5 10)
        timesTen = times10 10
    in  timesTwoTimesFive == timesTen

timesTwoTimesFiveEqualsTimesTenWithWhere =
    timesTwoTimesFive == timesTen
    where
        timesTwoTimesFive = (times2 $ times5 10)
        timesTen = times10 10

-- | different ways to define 'fibonacci'
fib01 0 = 1
fib01 1 = 1
fib01 n = fib01 (n - 1) + fib01 (n - 2)

fib02 n                                             -- using Guard expressions
    | n < 2     = 1                                 -- there is no '=' symbol
    | otherwise = fib02 (n - 1) + fib02 (n - 2)

fib03 n =                                           -- can use 'case' even in
    case n of                                       -- middle of computation
      0 -> 1
      1 -> 1
      _ -> fib03 (n - 1) + fib03 (n - 2)

fib04 n =                                           -- simple 'if-else' conditional
    if n < 2                                        -- every 'if' must have 'else'
       then 1
       else fib04 (n - 1) + fib04 (n - 2)

-- | ------------------------------- map, filter, foldr, foldl
-- | map
{-
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = (f x) : (map f xs)
-}
oneToFive = map (1 +) [0,1,2,3,4]
-- | filter
{-
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter valid (x:xs)
    | valid x = x : filter valid xs
    | otherwise = filter valid xs
-}
lessThanFive = filter (< 5) [1,3,5,7,9]

concatWithFoldr = foldr (++) "K" ["A", "B"]         -- ABK
concatWithFoldl = foldl (++) "K" ["A", "B"]         -- KAB
concatWithFoldrPlus = foldr (+) 1 [1,2,3]               -- 7


-- | ---------------------------- types
-- | type synonyms must begin with Capital Letter (like in-built String)
-- | they are just for better documentation
type Email = String
type Name  = String
type BigFunction a b c d = (c -> d) -> (b -> c) -> (a -> b) -> (a -> d)

validForms :: Name -> Email -> Bool     -- String -> String -> Bool
validForms name email
    | name == "" || email == "" = False
    | otherwise                 = True

-- crazyFunction :: BigFunction -> BigFunction -> Bool

-- | ----------------------------- algebraic data-types
{-
-- this says 'Bool' part of type signatures
-- {True,False} Value cosntructors for function computation
data Bool = True | False

-- 'Maybe a' is a type parameter allowing type to be polymorphic
data Maybe a = Nothing | Just a

not :: Bool -> Bool
not True = False
not False = True

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just y) = y


data Tree a = Empty | Node (Tree a) a (Tree a)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Empty = Empty
treeMap f (Node l v r) = Node (treeMap f l) (f v) ( treeMap f r)
-}


-- | using "record syntax", instead of pattern matching generates function
-- | it makes easier to work with value constructors
data Tree' a = Empty | Node { left :: Tree' a, value :: a, right :: Tree' a  }
{-
-- left :: Tree' a -> Tree' a
-- value :: Tree' a -> a
-- right :: Tree' a -> Tree' a
-}


-- | newtype generate thin wrapper over another type
-- | with only one constructor having one field
{-
[subtleties]
Type declared with 'data' are lifted, i.e. they contain their own 'bottom, _|_' distinct from others.
Even if Pattern Match seems to not fail, must check whether value is 'bottom' or 'Any y' for some 'y'.

To allow exact Isomorphisms to be preserved. 'newtype' contructor doesn't do any work for pattern matching as there is no separate 'bottom _|_' and so every value in type is wrapped in constructor.
-}


-- | treat TypeClass as interface
-- | if Rectangle is part of 'Eq' typeclass, must implement '==' or '/='
data Rectangle = Rect Int Int

instance Eq Rectangle where
    (Rect w1 h1) == (Rect w2 h2) = (w1 == w2) && (h1 == h2)


-- | can use typeclass in type signatures to enforce specific behaviors
isEqual :: (Eq a) => a -> a -> Bool
isEqual a b = a == b
