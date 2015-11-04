
### 5. Recursion

```simple-regression.hs
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: (Num a) => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
         | x > mx     = x
         | otherwise  = mx
           where mx = maximum' xs
```

---

* X-0 Board Game

```
--- the empty board
emptyBoard = ["...", "...", "..."]

--- returns the winner
winner :: [[Char]] -> Char
winner [[a,b,c],[d,e,f],[g,h,i]]
       |  a == b  &&  b == c  &&  a /= '.'    = a
       |  d == e  &&  e == f  &&  d /= '.'    = d
       |  g == h  &&  h == i  &&  g /= '.'    = g
       |  a == d  &&  d == g  &&  a /= '.'    = a
       |  b == e  &&  e == h  &&  b /= '.'    = b
       |  c == f  &&  f == i  &&  c /= '.'    = c
       |  a == e  &&  e == i  &&  a /= '.'    = a
       |  c == e  &&  e == g  &&  c /= '.'    = c
       |  '.' `elem` [a,b,c,d,e,f,g,h,i]      = '?' -- Game not over
       |  otherwise                           = '-' -- Tie

--- replace Ith element of a list
replace :: Int -> a -> [a] -> [a]
replace 0 a (x:xs) = a:xs
replace i a (x:xs) = x:(replace (i - 1) a xs)

--- outputs
--- -- *Main> winner ["XXX", "...", ".00"]
--- -- 'X'
--- -- *Main> replace 3 'p' "Snakes on a plane"
--- -- "Snapes on a plane"

--- replaces character at position r,c in board
play :: Int -> Int -> Char -> [[Char]] -> [[Char]]
play r c a board = replace r (replace c a (board !! r)) board
--- explaination of above one-liner
--- -- ( board !! r ) gets r-th row of board
--- -- (replace c a (board !! r)) replaces c char in r-th row with a
--- -- (replace r (...) board) replaces r-th row of board with new version

-- returns the value of board with player to play using 'minimax tree search'
value :: Char -> [[Char]] -> Int
value player board
      | w == 'X'        = 1
      | w == '0'        = -1
      | w == '-'        = 0
      | player == 'X'   = maximum [value '0' (play r c 'X' board) | r <- [0..2], c <- [0..2], board !! r !! c == '.']
      | otherwise       = minimum [value 'X' (play r c '0' board) | r <- [0..2], c <- [0..2], board !! r !! c == '.']
        where w = winner board

--- run sample of value
--- -- *Main>  value 'X' ["0..", ".0.","XX."]
--- -- 1
--- -- *Main>  value 'X' ["...", ".0.","XX0"]
--- -- 0

--- returns froma  list of boards where X has just moved
bestOf :: [[[Char]]] -> [[Char]]
bestOf [x] = x
bestOf (x:xs)
       | value '0' x > value '0' bxs = x
       | otherwise                   = bxs
         where bxs = bestOf xs

--- returns after X's best move
bestMove :: [[Char]] -> [[Char]]
bestMove board = bestOf [play r c 'X' board | r <- [0..2], c <- [0..2], board !! r !! c == '.']

--- -- 'it' always the last thing evaluated

--- playing game with minmax AI
--- -- *Main>  emptyBoard 
--- -- ["...","...","..."]
--- -- *Main> bestMove emptyBoard 
--- -- ["...","...","..X"]
--- -- *Main> play 1 2 '0' it
--- -- ["...","..0","..X"]
--- -- *Main> it
--- -- ["...","..0","..X"]
--- -- *Main> bestMove it
--- -- ["...","..0",".XX"]
--- -- *Main> play 2 0 '0' it
--- -- ["...","..0","0XX"]
--- -- *Main> bestMove it
--- -- ["...",".X0","0XX"]
--- -- *Main> play 0 1 '0' it
--- -- [".0.",".X0","0XX"]
--- -- *Main> bestMove it
--- -- ["X0.",".X0","0XX"]
--- -- *Main> winner it
--- -- 'X'
--- -- *Main> 

```
---

