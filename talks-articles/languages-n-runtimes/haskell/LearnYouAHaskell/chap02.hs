-- chapter.2 Starting Out

-- |in 'ghci' use 'let ...' to define stuff

doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
                         then x
                         else x*2
-- |Haskell's if mandate and else
-- |cuz in Haskell every expression and function must return something
doubleSmallNumberPlus1 x = (if x > 100 then x else x*2) + 1
doubleSmallNumberPlus1' x = (if x > 100
                            then x
                            else x*2) + 1
-- |Apostrophe(') doesn't have any significance than other characters
don'tChange x = x

-- |---------------------------- Intro to Lists
-- |Lists are homogeneous
listOfSomeNumbers = [0,1,1,2,3,5,8,13,21,34,55,89]
-- |"strings" are lists of characters ['s', 't', 'r', 'i', 'n', 'g', 's']
-- | double-quotes are syntactice sugar, list functions can be used on strings

-- |concat lists ++
concatList thisList thatList = thisList ++ thatList
-- |pre-pending singleton to a list (faster way)
prepentToList someElement someList = someElement:someList

-- |getting element by index use '!!'
theFifthElement someList = someList !! 5

-- |lists can be compared if their elements can be, in lexicographical order

-- |keywords for lists
headOfList = head [1..10]
lastOfList = last [1..10]
initOfList = init [1..10]
tailOfList = tail [1..10]
lengthOfList = length [1..10]
isListNull = null [1..10]

reverseOfList = reverse [1..10]
take2OfList = take 2 [1..10]
drop2OfList = drop 2 [1..10]
maximumOfList = maximum [1..10]
minimumOfList = minimum [1..10]
sumOfList = sum [1..10]
is2InList = 2 `elem` [1..10]

-- |ranges for enumerated Numbers and Characters
numOneToTwenty = [1..20]
smallcaseAToG = ['a'..'g']
capitalcaseIToM = ['I'..'M']
numTwentyToOne = [20,19..1]
numOneToTwentyEven = [2,4..20]
numOneToTwentyOdd = [1,3..20]

-- |infinite list and action
numAllOdd = [1,3..]
numAllFives = repeat 5
first10OddFromInfinite = take 10 (cycle numAllOdd)
numTensOfFives = take 10 numAllFives
numTensOfFives' = replicate 10 5


-- |---------------------------- List Comprehension

-- | for comprehension like; S = {2.x | x -E- N, x <= 10}
doubleOfFirstTenNaturalNumbers = [x*2 | x <- [1..10]]
dForDivBy5 = [x*2 | x <- [1..10], mod x 5==0]

tripleForXElvn = [x*3 | x <- [1,11..], x `mod` 11 == 0 ]
fiveFromTripleForXElvn = take 5 tripleForXElvn

boomBangs xs = [if odd x then "BOOM!" else "BANG!" | x <- xs]

numDivBy5And7And13 = [x | x <- [1..], mod x 5 == 0, mod x 7 == 0, mod x 13 == 0 ] -- multiple checks
multiplyLists = [x * y | x <- [1..3], y <- [7..9], x*y > 15] -- multiple input lists with checks

nouns = ["alice", "bob", "eve", "milton", "trudy"]
adjectives = ["lazy", "smart", "studious", "nosy", "intrusive"]
nounAdj = [adj ++ " " ++ noun | adj <- adjectives, noun <- nouns]

calcLengthOfList xs = sum [1 | _ <- xs] -- use underscore for variable placeholder not to be used

-- | string using list comprehension

pickUppercase st = [ c | c <- st, elem c ['A'..'Z'] ]

-- | nested list using list comprehension
xxs = [[1..5], [6..10], [11..15]]
xxsEven = [[x | x <- xs, even x] | xs <- xxs]


-- |---------------------------- Tuples
-- | Tuples are heterogeneous
-- | tuples are of fixed length but don't have to have homogeneous typed elem
-- | it's like parameters use-case, where you expect a specific count of elems
tupleOneToTwo = (1,2)
tupleOneToFive = (1,2,3,4,5)

-- |fst,snd for only tuples with 2 elements
firstElement = fst tupleOneToTwo
secondElement = snd tupleOneToTwo


-- |---------------------------- Zip 2 lists into pairs of list of tuples

-- |zip-ping lists (not tuples)
zipFirst10With5 = zip [1..10] (replicate 10 5)   -- [(1,5),(2,5),(3,5),(4,5),(5,5),(6,5),(7,5),(8,5),(9,5),(10,5)]
zipColors = zip ["azul", "amarilla", "blanca"] ["blue", "yellow", "white"]

-- | can zip easily with infinite ranges, it being Lazy
zipDifferentTypes = zip [1..] ["manzana", "oranges"] -- [(1, "manzana"), (2, "oranges")]


-- |---------------------------- Problem: Triangles

triangles = [(a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]

rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

rightTrianglesBig = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c>20]

rightTrianglesX trixxs = [(a,b,c) | (a,b,c) <- trixxs, (a^2 + b^2) ==  (c^2)]

-- |----------------------------

