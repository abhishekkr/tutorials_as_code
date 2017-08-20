
### 6a. Higher Order Functions

* example code 6a.1.hs

``` 6a.1.hs
--- simple function
doubleIt :: Int -> Int
doubleIt x = x * 2

add1 :: Int -> Int
add1 x = x + 1

add1ToEach :: [Int] -> [Int]
add1ToEach [] =[]
add1ToEach (x:xs) = add1 x : add1ToEach xs

f1 :: (Int  -> Int) -> Int
f1 x = 3

f2 :: (Int  -> Int) -> Int
f2 x = x 3

f3 :: Int -> (Int -> Int)
f3 x
   | x < 10     = doubleIt
   | otherwise  = add1

f4 :: Int -> Int -> Int
f4 x y = x + y

f5 :: (Int -> Int) -> Int
f5 x = x(5)

sum3 :: Int -> Int -> Int -> Int
--- could be written as, but would unfold same
--- -- sum3 :: Int -> (Int -> (Int -> Int))
sum3 a b c = a + b + c

from :: Int -> Int -> Int
from = flip (-)

--- zipWith higher order function can be used to apply an operator on two lists
--- resultant list is of length from smaller list
```

---

* usage

```
*Main> f2 doubleIt 
6
*Main> f1 doubleIt 
3
*Main> map doubleIt [1,2,3,4,5]
[2,4,6,8,10]
```

check signature of f3 and f4, it's same though declaration is different
```
*Main> :t f3
f3 :: Int -> Int -> Int
*Main> :t f4
f4 :: Int -> Int -> Int
```

how the function unfolds here
```
*Main> (f3 3) 4
8
*Main> f3 3 4
8
```

We can partially apply the f3 method like following

```
*Main> f3 3
    <interactive>:69:1:
    No instance for (Show (Int -> Int))
      (maybe you haven't applied enough arguments to a function?)
      arising from a use of ‘print’
    In a stmt of an interactive GHCi command: print it
*Main> let f3mm = f3 3

*Main> f3mm 4
8
*Main> f3mm 7
14
*Main> f3mm 1
2
*Main> f3mm 1
2

*Main> let f4mm = f4 3
*Main> f4mm 1
4
*Main> f4mm 7
10
*Main> 
```

partially applying map

```
--*Main> map (max 3) [1,2,3,4,5]
--[3,3,3,4,5]
```

how unfolding of sum3 works can be understood by following usage example
this is "currying"

```
*Main> sum3 4 5 6
15
*Main> ((sum3 4) 5 6)
15
*Main> (((sum3 4) 5) 6)
15
```

using zipWith

```
*Main> zipWith (+) [1,2,3,4,5] [9,8..5]
[10,10,10,10,10]
```

flip operator flips the passing order of values

```
*Main> from 5 8
3
*Main> 5 `from` 8
3
*Main> 8 `from` 5
-3
*Main>
```

example of filter and takeWhile higher-order functions

```
*Main> filter (>5) [1..10]
[6,7,8,9,10]
*Main> 
*Main> takeWhile (<10) [1,3..]
[1,3,5,7,9]
*Main> 
```

---

