-- chapter.3 Types and Typeclasses

-- `:t identifier` gives type signature
-- `::` read as 'has type of'

justAddXAndY :: Int -> Int -> Int
justAddXAndY x y = x + y

-- | Int typed Integer is bounded
-- | Integer typed Integer is unbounded
-- | Float a real floating point with single precision
-- | Double a real floating point with double precision
-- | Bool a boolean
-- | Char for character denoted in single quotes and list of Char is string
-- | Tuples are types but depend on length and type of elements, empty tuple is also a type

-- | 'type variable' used by methods where need generic feature, like 'head'
-- | 'typeclass' is sort of interface defining some behavior

{-Eq and Ord Typeclasses

Type signature of '==' function
Prelude> :t (==)
(==) :: Eq a => a -> a -> Bool

Pretty much all operators like +,*,/,- are functions.
If function is just made of special characters, by default interprested as infix operator.

To use it as prefix function, surround it in paranthesis.

For type definition we see a new symbol '=>'. Everything before it is a class constraint.

The types supported by '==' must be a member of Eq class.
The Eq typeclass provides an interface for testing equality. The functions its members implement are '==' and '\='.
All Haskell types except IO and functions are a part of Eq typeclass.

Similarly Ord Class is for types that have an ordering. Ord covers all standard comparing functions such as '>,<,>= and <='.

-}

{-Show and Read

Members of Show can be presented as strings. All types covered as of now except functions are a part of Show.

    Prelude> show 3
    "3"
    Prelude> show 'a'
    "'a'"
    Prelude> show True
    "True"
    Prelude> show 1.234
    "1.234"


Read is opposite of Show. It takes a string and returns it of type for second argument which if member of Read.

    Prelude> :t read
    read :: Read a => String -> a
    Prelude> read "True" || False
    True
    Prelude> read "7" + 3
    10
    Prelude> read "[1,2,3]" ++ [4,5]
    [1,2,3,4,5]
    Prelude> 


Just providing a string to read results to error as target type is not known

    Prelude> read "[1,2,3]"
    *** Exception: Prelude.read: no parse


Type annotations can be used to explicitly mention type for read target

    Prelude> read "100" :: Int
    100
    Prelude> (read "100" :: Int) + 1
    101
    Prelude> read "[1,2,3]" :: [Int]
    [1,2,3]
    Prelude> read "(1,'a')" :: (Int, Char)
    (1,'a')

-}

{- Enum

Enum numbers are sequentially ordered types. Enum typeclass can be used in list ranges, they have successors and predecessors with succ and pred respectively.

Types in this class:
(), Bool, Char, Ordering, Int, Integer, Float and Double
    Prelude> 
    Prelude> ['a'..'e']
    "abcde"
    Prelude> [LT .. GT]
    [LT,EQ,GT]
    Prelude> 
    Prelude> [3..5]
    [3,4,5]
    Prelude> succ 'B'
    'C'
    Prelude> pred 'B'
    'A'
    Prelude> 


Bounded members have an upper and lower bound
    Prelude> 
    Prelude> minBound :: Int
    -9223372036854775808
    Prelude> maxBound :: Int
    9223372036854775807
    Prelude> minBound :: Bool
    False
    Prelude> maxBound :: Bool
    True
    Prelude> minBound :: (Char,Int,Bool)
    ('\NUL',-9223372036854775808,False)
    Prelude> maxBound :: (Char,Int,Bool)
    ('\1114111',9223372036854775807,True)
    Prelude> 

-}


{- Num Typeclass

Num is a numeric typeclass. Whole number are polymorphic constants as they can act like type that's a member of Num Typeclass.
Until type is fixed for types of number in expression, they will adjust to best available type under Num Typeclass.

-}

-- | fromIntegral takes a specific typed Integral(Int,Integer) value and returns a more general number.
-- | useful for cases where return value is Integral as in 'length'
lengthPlus5Dot2 = fromIntegral(length [1..10]) + 5.2

---------------------------------------
