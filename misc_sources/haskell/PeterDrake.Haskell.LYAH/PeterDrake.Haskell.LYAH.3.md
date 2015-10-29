

### 3 :: Types, Typeclasses

* Types are sets of values
* Typeclasses are set of types

```
Analogy to Java

  Haskell            Java
[TypeClass]       [Interface]
    /^\               /^\
[   Type  ]       [  Class  ]
    /^\               /^\
[  Value  ]       [ Object  ]
```

```ghci
GHCi, version 7.10.1: http://www.haskell.org/ghc/  :? for help
Prelude> :t True
True :: Bool
Prelude> :t "A"
"A" :: [Char]
Prelude> :t "abcdef"
"abcdef" :: [Char]
Prelude> :t 'a'
'a' :: Char
Prelude> :t 1
1 :: Num a => a
Prelude> :t 1.0
1.0 :: Fractional a => a

Prelude> let x = 1
Prelude> :t x
x :: Num a => a
Prelude> 
Prelude> 1 :: Int
1

Prelude> :t head
head :: [a] -> a

Prelude> :t (+)
(+) :: Num a => a -> a -> a

Prelude> :t zip
zip :: [a] -> [b] -> [(a, b)]


Prelude> let f ls = head ls + length ls
Prelude> :type f
f :: [Int] -> Int


Prelude> let dividesEvenly x y = (y / x) * x == y
Prelude> dividesEvenly 2 5
True
Prelude> dividesEvenly 2 4
True
Prelude> --- BUG
Prelude> :t dividesEvenly 
dividesEvenly :: (Eq a, Fractional a) => a -> a -> Bool

Prelude> --- --- declaring typing intention is good before defining, also helps readers
Prelude> --- dividesEvenl :: Int -> Int -> Bool

Prelude> let dividesEvenl x y = (y `div` x) * x == y
Prelude> dividesEvenl 2 5
False
Prelude> dividesEvenl 2 4
True
Prelude> 


```
