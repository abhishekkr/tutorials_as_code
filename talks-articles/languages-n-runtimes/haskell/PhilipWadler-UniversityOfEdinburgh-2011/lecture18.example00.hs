import Control.Monad -- MonadPlus


pairs   :: Int  ->  [(Int,  Int)]
pairs n   = [(i,j) | i <- [1..n], j <- [(i+1)..n]]

-- - Equivalently
pairsWithDo  :: Int  ->  [(Int, Int)]
pairsWithDo n  = do {
              i <- [1..n];
              j <- [(i+1)..n];
              return (i,j)
            }

-- -- - With Guard

pairsGuard   :: Int  ->  [(Int,  Int)]
pairsGuard n   = [(i,j) | i <- [1..n], j <- [1..n], i < j]

-- MonadPlus is a monad with extra structure
{-
-- MonadPlus
class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a


instance MonadPlus [] where
  mzero = []
  mplus = (++)


guard :: MonadPlus m => Bool -> m ()
guard False   = mzero       -- non-deterministic result; 'mzero = [] = no possible result'
guard True    = return ()   -- 'return () = [()] = one possible result'

msum :: MonadPlus m => [m a] -> m a
msum  = foldr mplus mzero   -- generalization of concat
-}

-- - Equivalently
pairsWithDoGuard  :: Int  ->  [(Int, Int)]
pairsWithDoGuard n  = do {
                        i <- [1..n];
                        j <- [1..n];
                        guard (i < j);
                        return (i,j)
                      }
