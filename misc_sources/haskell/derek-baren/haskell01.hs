-- single line comment

{-
 - multi-line comment
 -
 - to use interactive ghc session for it
 - $ ghci
 - > :l haskell01
 -}

import Data.List
import System.IO

always5 :: Int
always5 = 5

always7 = 7

{-
 - uses type inference ; this is what inspired golang
 - Int -2^63 to 2^63
 - Integer is unbounded whole number, size depends on memory
 - Float
 - Double with precision upto 11points
 - Bool True False
 - Char
 - Tuple
 -}

maxInt = maxBound :: Int
minInt = minBound :: Int

{-
 - can reload this module on 'ghci' by running
 - > :r
 - > maxInt
 -
 - this will print value to maxInt
 -}

sumOfNums = sum[1..1000]

addEx = always5 + always7

-- commonly using as a prefix operator
modEx = mod always7 always5
-- same can be written with an infix operator style
modEx2 = always7 `mod` always5


negNumEx = always7 + (-2)

{-
 - to checkout what goes on with 'sqrt' function
 - *Main> :t sqrt
 -}

num9 = 9 :: Int
-- since sqrt requires floating point integer, using 'fromIntegral'
sqrtOf9 = sqrt (fromIntegral num9)

-- more built-in math function
piVal = pi
ePow9 = exp 9
logOf9 = log 9
squared9 = 9 ** 2
truncateVal = truncate 9.999
roundVal = round 9.999
ceilingVal = ceiling 9.999
floorVal = floor 9.999
--- Also sin,cos,tan,asin,atan,acos,sinh,tanh,cosh,asinh,atanh,acosh

trueAndFalse = True && False
trueOrFalse = True || False
notTrue = not(True)

--- can check help on operators as well; *Main> :t (+)

--- haskell02.hs is list onwards
