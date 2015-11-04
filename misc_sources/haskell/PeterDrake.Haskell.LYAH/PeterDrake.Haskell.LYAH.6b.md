
### 6b. Lambdas and Folds

lambda example, lambda gets specified by forward slash '\'
``` map (\x -> x + 1) [1..5] ```

In below code-example addAll and multAll are almost same methods, except for empty result and operator used for computation.
OOPs try solve redundancy with inheritance.
FPs answer to this is Higher Order Functions.

* example usage of foldl and foldr

```
foldl (/) 100 [10,5,2]
--- -- is same as
( ( (100/10) /5) /2)

--- and

foldr (/) 100 [10,5,2]
--- -- is same as
10/(5/(2/100))
```

thus adding/multiplying all from list can be done as

```
foldl (+) 0 [10,5,2]
foldl (*) 1 [10,5,2]
```

---

* sample code 6b.1.hs

```
addAll :: (Num a) => [a] -> a
addAll [] = 0
addAll (x:xs) = x + addAll xs
--- -- using higher order functions
addAll' :: (Num a) => [a] -> a
addAll' x = foldl (+) 0 x

multAll :: (Num a) => [a] -> a
multAll [] = 1
multAll (x:xs) = x * multAll xs

firstThat :: (a -> Bool) -> a -> [a] -> a
firstThat f = foldr (\x acc -> if f x then x else acc)

lastThat :: (a -> Bool) -> a -> [a] -> a
lastThat f = foldl (\acc x -> if f x then x else acc)

argmax :: (Ord b) => (a -> b) -> [a] -> a
argmax f [x] = x
argmax f (x:xs) = if f x > f (argmax f xs)
                    then x
                    else (argmax f xs)

--- -- re-writing argmax using foldl
argmax' :: (Ord b) => (a -> b) -> [a] -> a
argmax' f = foldl (\acc x -> if f x > f acc then x else acc)
```

---

* usage

firstThat, lastThat and argmax checking numbers

```
*Main> firstThat (>100) 1 [-3,5,7,-2]
1
*Main> firstThat (>10) 1 [-3,5,7,-2]
1
*Main> firstThat (>1) 1 [-3,5,7,-2]
5
*Main> firstThat (>1) 0 [-3,5,7,-2]
5

*Main> lastThat (>10) 0 [-3,5,7,-2]
0
*Main> lastThat (>1) 0 [-3,5,7,-2]
7

*Main> argmax (>2) [-3,5,7,-2]
7
*Main> argmax (>5) [-3,5,7,-2]
7
```

---

* using dot '.' to re-write lambda expressions

```
*Main> map (\x -> 1 + (2 * x)) [1..4]
[3,5,7,9]
*Main> 
*Main> map ((1+) . (2*)) [1..4]
[3,5,7,9]
*Main> 
```

---

* re-writing Tic-Tac-Toe from 'PeterDrake.Haskell.LYAH.5.md' using this

```
--- the empty board
emptyBoard = ["...", "...", "..."]

firstThat :: (a -> Bool) -> a -> [a] -> a
firstThat f = foldr (\x acc -> if f x then x else acc)

--- RE-WRITE
--- returns the winner
winner :: [[Char]] -> Char
winner [[a,b,c],[d,e,f],[g,h,i]]
       |  line /= []                          = head line
       |  '.' `elem` [a,b,c,d,e,f,g,h,i]      = '?' -- Game not over
       |  otherwise                           = '-' -- Tie
         where line = firstThat (\[x,y,z] -> x == y && y == z && z /= '.')
                                []
                                [[a,b,c],[d,e,f],[g,h,i],[a,d,g],[b,e,h],[c,f,i],[a,e,i],[c,e,g]]
--- /RE-WRITE

--- replace Ith element of a list
replace :: Int -> a -> [a] -> [a]
replace 0 a (x:xs) = a:xs
replace i a (x:xs) = x:(replace (i - 1) a xs)

--- replaces character at position r,c in board
play :: Int -> Int -> Char -> [[Char]] -> [[Char]]
play r c a board = replace r (replace c a (board !! r)) board

--- RE-WRITE
--- returns a list of succeeding board when player plays
successors :: Char -> [[Char]] -> [[[Char]]]
successors player board = [play r c player board | r <- [0..2], c <- [0..2], board !! r !! c == '.']

-- returns the value of board with player to play using 'minimax tree search'
value :: Char -> [[Char]] -> Int
value player board
      | w == 'X'        = 1
      | w == '0'        = -1
      | w == '-'        = 0
      | player == 'X'   = maximum (map (value '0') (successors 'X' board))
      | otherwise       = minimum (map (value 'X') (successors '0' board))
        where w = winner board
--- /RE-WRITE

bestOf :: [[[Char]]] -> [[Char]]
bestOf [x] = x
bestOf (x:xs)
       | value '0' x > value '0' bxs = x
       | otherwise                   = bxs
         where bxs = bestOf xs

bestMove :: [[Char]] -> [[Char]]
bestMove board = bestOf [play r c 'X' board | r <- [0..2], c <- [0..2], board !! r !! c == '.']

```

---

