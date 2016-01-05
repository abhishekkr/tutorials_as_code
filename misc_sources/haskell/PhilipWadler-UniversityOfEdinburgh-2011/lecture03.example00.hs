{-

sumSqOdd.hs

$ cabal install quickcheck

$ ghci $0
*Main> quickCheck prop_sumSqOdd

-}

import Test.QuickCheck

squares :: [Integer] -> [Integer]
squares xs = [ x*x | x <- xs ]

odds :: [Integer] -> [Integer]
odds xs = [ x | x <- xs, odd x ]

sumSqOdd :: [Integer] -> Integer
sumSqOdd xs = sum [ x*x | x <- xs, odd x ]

sumSqOdd' :: [Integer] -> Integer
sumSqOdd' xs = sum (squares (odds xs))

prop_sumSqOdd :: [Integer] -> Bool
prop_sumSqOdd xs = sumSqOdd xs == sumSqOdd' xs
