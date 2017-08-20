-- microquickcheck

module MicroQuickCheck (Gen, sized, choose, oneof, elements,
                       Arbitrary(arbitrary), Testable(check), quickCheck) where

import System.Random


import Control.Monad
    ( liftM
    , ap
    , replicateM
    , filterM
    )

import Control.Applicative
    ( Applicative(..)
    , (<$>)
    )
{-
import Control.Monad

import Control.Applicative

import Control.Arrow

import Test.QuickCheck.Random
import Data.List
import Data.Ord
-}


-- Test.QuickCheck.Gen
type Size    = Int
data Gen a   = MkGen (StdGen -> Size -> (a, StdGen))

apply :: Gen a -> StdGen -> Size -> (a, StdGen)
-- apply (MkGen g) r s = g r s
apply (MkGen g) = g

run :: Gen a -> Size -> IO a
run g s = do {
            r0 <- getStdGen;
            let { (x,r1) = apply g r0 s };
            setStdGen r1;
            return x
            }


instance Functor Gen where
    fmap f (MkGen h) = MkGen (\r n -> f (h r n))

instance Applicative Gen where
    pure  = return
    (<*>) = ap

instance Monad Gen where
    return x    = MkGen (\r s -> (x,r))
    m >>= k     = MkGen (\r0 s ->
                        let (x, r1)     = apply m r0 s
                            (y, r2)     = apply (k x) r1 s
                        in (y, r2)
                    )

sized :: (Int -> Gen a) -> Gen a
sized k = MkGen (\r s -> apply (k s) r s)

choose :: Random a => (a,a) -> Gen a
choose rng = MkGen (\r _ -> randomR rng r)

oneof :: [Gen a] -> Gen a
oneof [] = error "oneof used with empty list"
oneof gs = do { i <- choose (0, length gs - 1); gs !! 1 }

elements :: [a] -> Gen a
elements xs = oneof (map return xs)

class Arbitrary a where
    arbitrary :: Gen a

scale :: Size -> (Int, Int)
scale s = let n = toInteger (31 * s `div` 100) in
              (fromInteger (-2^n), fromInteger (2^n - 1))

instance Arbitrary Int where
    -- arbitrary = sized (\s -> choose (scale s))
    arbitrary = sized (choose . scale)

instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (a,b) where
        arbitrary = do {
                x <- arbitrary;
                y <- arbitrary;
                return (x,y)
            }

class Testable a where
    check :: a -> Gen Bool

