-- MicroQuickCheckTest

module MicroQuickCheckTest where

import MicroQuickCheck
import Control.Monad
import Proposition

{--
data Proposition    = Var String
                    | F
                    | T
                    | Not Proposition
                    | Proposition :|: Proposition
                    | Proposition :&: Proposition
                    deriving (Eq, Ord, Show)
-}

instance Arbitrary Proposition where
    arbitrary = sized expr
        where
            expr 0 =
                oneof[return F,
                     return T,
                     liftM Var (elements ["p", "q", "r", "s", "t"])]
            expr n | n > 0 =
                oneof [return F,
                      return T,
                      liftM Var (elements ["p", "q", "r", "s", "t"]),
                      liftM Not (expr (n - 1)),
                      liftM2 (:&:) (expr (n `div` 2)) (expr (n `div` 2)),
                      liftM2 (:|:) (expr (n `div` 2)) (expr (n `div` 2)) ]

size :: Proposition -> Int
size (Var x)    = 1
size F          = 1
size T          = 1
size (Not p)    = size p + 1
size (p :|: q)  = size p + size q + 1
size (p :&: q)  = size p + size q + 1

prop_small :: Proposition -> Bool
prop_small p = size p < 15

prop_parse :: Proposition -> Bool
prop_parse p = read (show p) == p

main =
    quickCheck prop_small >>
    quickCheck prop_parse
