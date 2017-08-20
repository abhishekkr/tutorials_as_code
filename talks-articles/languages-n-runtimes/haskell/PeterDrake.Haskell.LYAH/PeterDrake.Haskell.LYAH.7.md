
### 7. Modules

Some important example of Modules are Data.List, Data.Char, Data.Map, Data.Set.

```Utilities.hs
module Utilities (replace) where

--- Replace i-th item of a list
replace :: Int -> a -> [a] -> [a]
replace i item ls = x ++ (item:ys) where (x, _:ys) = splitAt i ls

```
---

Re-written Tic-Tac-Toe with Data.List and Utilities from above

```TicTacToe.hs
import Data.List
import Data.Maybe
import Utilities

--- the empty board
emptyBoard = ["...", "...", "..."]

--- returns the winner
winner :: [[Char]] -> Char
winner [[a,b,c],[d,e,f],[g,h,i]]
       |  line /= Nothing                     = head (fromJust line)
       |  '.' `elem` [a,b,c,d,e,f,g,h,i]      = '?' -- Game not over
       |  otherwise                           = '-' -- Tie
         where line = find (\[x,y,z] -> x == y && y == z && z /= '.')
                           [[a,b,c],[d,e,f],[g,h,i],[a,d,g],[b,e,h],[c,f,i],[a,e,i],[c,e,g]]


--- replaces character at position r,c in board
play :: Int -> Int -> Char -> [[Char]] -> [[Char]]
play r c a board = replace r (replace c a (board !! r)) board

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

