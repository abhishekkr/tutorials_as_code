-- chapter.5 Recursion
--

{-
    recursion is important to haskell n 'edge condition' to recursion
    and because of strength of Recursion haskell got no while/for loops
-}

-- def of max from list using recursion with power of pattern matching
xMax :: Ord a => [a] -> a
xMax [] = error "no max in empty list"
xMax [x] = x
xMax (x:xs)
    | x > mTail = x
    | otherwise = mTail
    where mTail = xMax xs

-- a clearer way writing logic for above
yMax :: Ord a => [a] -> a
yMax [] = error "no max in empty list"
yMax [y] = y
yMax (y:ys) = max y (yMax ys)

-- a 'let' way writing logic for above
zMax :: Ord a => [a] -> a
zMax [] = error "no max in empty list"
zMax [z] = z
zMax (z:zs) = let zz = zMax zs in (if z > zz then z else zz)


-- def for replicate
xReplicate :: (Num i, Ord i) => i -> x -> [x]
xReplicate n x
    | n <= 0 = []
    | otherwise = x:xReplicate (n-1) x


-- def for take
xTake :: (Num i, Ord i) => i -> [a] -> [a]
xTake n [] = []
xTake n (x:xs)
    | n <= 0 = []
    | otherwise = x:xTake (n-1) xs


-- def of reverse
xRev :: [a] -> [a]
xRev [] = []
xRev (x:xs) = xRev xs ++ [x]


-- def of repeat, 'cuz haskell is lazy this works beautifully'
xRepeat :: a -> [a]
xRepeat x = x:xRepeat x


-- def of zip
xZip :: [a] -> [b] -> [(a,b)]
xZip _ [] = []
xZip [] _ = []
xZip (x:xs) (y:ys) = (x,y):xZip xs ys


-- def of elem
xElem :: (Eq a) => a -> [a] -> Bool
xElem _ [] = False
xElem a (x:xs)
    | a == x      = True
    | otherwise = xElem a xs


{- Quick Sort
    
-}

qSort :: (Ord a) => [a] -> [a]
qSort [] = []
qSort (x:xs) =
    let smallerSorted = qSort [a | a <- xs, a <= x]
        biggerSorted  = qSort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

-- another implementation
qqSort :: (Ord a) => [a] -> [a]
qqSort [] = []
qqSort (x:xs) = smallerSorted ++ [x] ++ biggerSorted
    where smallerSorted = [a | a <- xs, a <= x]
          biggerSorted  = [a | a <- xs, a > x]

---
