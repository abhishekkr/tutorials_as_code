-- chapter.4 Syntax in Functions
--

-- implementing nuAdd for add
nuAdd :: (Num a) => a -> a -> a
nuAdd x y = x + y

{- Pattern Matching
-- Specifying patterns to which data should conform.
-}

-- while defining functions, define separate function definition for
-- different input patterns say datatypes

amITheOne :: (Integral a) => a -> Bool
amITheOne 1 = True
amITheOne x = False

-- above pattern matching could fail if there is no las line to match
-- generic input cases


-- recursion within multiple defs of function
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)


-- pattern matching can be used in tuples
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)
-- will work same as
addVectorsX :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectorsX (x1, y1) (x2, y2) = (x1+x2, y1+y2)


-- sample definition for common methods
-- fetch first element from a triplet tuple
xfirst :: (a, b, c) -> a
xfirst (x, _, _) = x
--
-- fetch second element from a triplet tuple
xsecond :: (a, b, c) -> b
xsecond (_, y, _) = y
--
-- fetch third element from a triplet tuple
xthird :: (a, b, c) -> c
xthird (_, _, z) = z
--
-- fetch first element of a list
xhead :: [a] -> a
xhead [] = error "can't call head on an empty list"
xhead (x:_) = x
--
-- length of a list
xlen :: (Num b) => [a] -> b
xlen [] = 0
xlen (_:xs) = 1 + xlen xs
--
-- sum of all numbers in a list
xsum :: (Num a) => [a] -> a
xsum [] = 0
xsum (x:xs) = x + xsum xs

--
-- following func tries to handle all input cases from emoty to n lenght
-- by matching with _
tell :: (Show a) => [a] -> String
tell [] = "an empty least, ah"
tell (x:[]) = "list got one element: " ++ show x
tell (x:y:[]) = "list got two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "list got '" ++ show x ++ "', '" ++ show y ++ "' and more elements"

--
-- things called 'patterns' are handy way of breaking up thing as per
-- pattern and still keep binding to original form like 'xs@(y:ys)' and use as
xName :: String -> String
xName "" = "noname"
xName all@(x:xs) = "X-" ++ [x] ++ " is x-name for " ++ all


{-
Guards

instead of using chained if-else statements, represent simply in Guards
    can be further simplified using 'where' or 'let'

'where' bindings, a construct to let you bind variables at end of a
    function and whole function can see them, including all guards
-}

-- guards with simple case like flow
httpMsg :: (Ord a, Num a) => a -> String
httpMsg x
    | x < 200     = "what"
    | x < 300     = "ok"
    | x < 400     = "redir"
    | x < 500     = "bad client"
    | otherwise    = "bad server"

-- guards with computation within condition
xOddEven :: (Integral a) => a -> String
xOddEven x
    | x `rem` 2 == 0   = "even"
    | x `rem` 2 == 1   = "odd"
    | otherwise        = "nah not gonna happen"

-- where !!!
-- say for abstraction of equation from Guard
xpcent :: (Num a, Ord a, Fractional a) => a -> a -> String
xpcent x y
  | xyp <= 50   = "not enough"
  | xyp <= 85   = "enough"
  | xyp <= 97   = "good enough"
  | otherwise   = "great"
  where xyp = (x/y)*100

-- where with more abstractions on comparators
xypcent :: (Num a, Ord a, Fractional a) => a -> a -> String
xypcent x y
  | xyp <= bad   = "not enough"
  | xyp <= nice   = "enough"
  | xyp <= good   = "good enough"
  | otherwise   = "great"
  where xyp = (x/y)*100
        bad = 50
        nice = 85
        good = 97

-- where with more abstractions on comparators as tuple
xyzpcent :: (Num a, Ord a, Fractional a) => a -> a -> String
xyzpcent x y
  | xyp <= bad   = "not enough"
  | xyp <= nice   = "enough"
  | xyp <= good   = "good enough"
  | otherwise   = "great"
  where xyp = (x/y)*100
        (bad, nice, good) = (50, 85, 97)

-- another trivial pattern matching
initials :: String -> String -> String
initials fsname lsname = [f] ++ "." ++ [l] ++ "."
    where (f:_) = fsname
          (l:_) = lsname


-- can define foo in where blocks as well
-- xPairL :: Num a => [a] -> [a]
-- xPairL []       = []
-- xPairL [x]      = [x]
-- xPairL (x:y:xs) = [xpair x y | x, y <- x:y:xs ]
--    where xpair x y = x * y


{-
Let

let <bindings> in <expression>

Similar to 'where' bindings, 'let' bind to variables anywhere
  in function and are expressions themselves but 'very local'.
  So doesn't span across guards. They can be used for pattern matching.

'let' bindings are expression themselves,
    'where' bindings are just syntactic constructs
-}

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2* pi * r * h
        topArea = pi * r^2
    in sideArea + 2 * topArea

-- as 'if else expression' can fit in almost anywhere
xIfLst = [if 5 > 3 then "Bing" else "Bang", if 'a' > 'b' then "Foo" else "Bar"]
xIfTpl = 3 * (if 10 > 5 then 10 else 5) + 2
-- similarly 'let expression' could as well
--
-- functions in a local scope
xLetLstTpl = [let sqr x = x*x in (sqr 5, sqr 3, sqr 2)]
xLetTpl = (let a=1; b=11; c=111 in a*b*c, let x="hey"; y="ho" in x++ " " ++ y)

xSome = (let (a,b,c) = (1,11,111) in a+b+c) * 10

-- let in list comprehensions
xTplSum :: Num a => [(a,a)] -> [a]
xTplSum xys = [tplSum | (x,y) <- xys, let tplSum = x+y]
-- notice 'in' part is skipped in here
xTplSumBig :: (Num a, Ord a) => [(a,a)] -> [a]
xTplSumBig xys = [tplSum | (x,y) <- xys, let tplSum = x+y, tplSum > 10]


{- Case Expression

since these are case with 'expression', we can pattern match in here as well
-}

-- re-writing head with case
caseHead :: [a] -> a
caseHead xs = case xs of []    -> error "no head for empty"
                         (x:_) -> x

-- append from case, and use of otherwise in it
describeXS :: [a] -> String
describeXS xs = "the given list " ++ case xs of []   -> "is empty"
                                                [x]  -> "is singleton"
                                                otherwise  -> "got few elements"
-- in above def otherwise is easily interchangeable with _
describeYS :: [a] -> String
describeYS ys = "the given list " ++ case ys of []   -> "is empty"
                                                [x]  -> "is singleton"
                                                _  -> "got few elements"


{- use of whatX and 'where'
 
mimicking case usage
-}

describeZS :: [a] -> String
describeZS zs = "the given list " ++ what zs
    where what []   = "is empty"
          what [x]  = "is singleton"
          what   _  = "got few elements"









--
---------------------------------------
