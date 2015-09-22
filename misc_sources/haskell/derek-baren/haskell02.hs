import Data.List
import System.IO

--- can keep on hitting ':r' to reload it in ghci

primeNums = [3,5,7,11]

morePrimes = primeNums ++ [13,17,19,23,29]

--- can combine numbers into a list as following
favNums = 2 : 7 : 21 : 66 :[]

--- list in a list
multiList = [[1,2,3],[4,5,6]]

--- adding elements to start of a list
morePrimes2 = 2 : morePrimes

lengthOfFavNums = length favNums

revPrime = reverse morePrimes

isListEmpty = null morePrimes2

secondPrime = morePrimes2 !! 1
firstPrime = head morePrimes2
lastPrime = last morePrimes2

--- everything but last value
primeInit = init morePrimes2

--- get first 3 values
first3Primes = take 3 morePrimes2

--- get remaining values other than first 3
removedPrimes = drop 3 morePrimes2

--- check element in list
is7InList = elem 7 morePrimes2

--- maximum and minimum from list
maxPrime = maximum morePrimes2
minPrime = minimum morePrimes2

newList = [1,2,3]
--- product of a list
sumList = sum newList
prodList = product newList

--- list generation, multi-colored/type
zeroToTen = [0..10]
evenList = [2,4..20]
letterlist = ['A', 'C' .. 'Z']
--- not actually infinite, just calculates to what you need when you need... lazy
infiniPow10 = [10,20..]

many2s = take 10 (repeat 2)

many3s = replicate 10 3

cyclelist = take 10 (cycle [1,2,3,4,5])

listTimes2 = [x * 2 | x <- [1..10]]
listMany3s = [x * 2 | x <- many3s]
--- making elements a part on condition
listTimes3 = [x * 2 | x <- [1..10], x * 3 <= 50]
--- multiple filters
diviseBy9N13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

--- sorted list
sortedList = sort [10,3,5,2,6,5]

--- zipWith is very powerful
sumOfLists = zipWith (+) [1,2,3] [4,5,6]

listBiggerThen5 = filter (>5) morePrimes2

--- one of the uses of infinite list
evensUpto20 = takeWhile (<= 20) [2,4..]

--- foldl
multOfList = foldl (*) 2 [1,2]
multOfListR = foldr (*) 1 [1,2]

pow3List = [3^n | n <- [1..10]]
multTable = [[x * y | y <- [1..10]] | x <- [1..10]]

--- haskell03.hs will have tuples and onwards
