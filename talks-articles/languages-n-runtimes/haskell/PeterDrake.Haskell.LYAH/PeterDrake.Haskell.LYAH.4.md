
### 4. Function Syntax

* Recursion

```increasing01.hs
increasing :: (Ord a) => [a] -> Bool
increasing xs = if xs == []
                then True
                else if tail xs == []
                     then True
                     else if head xs <= head (tail xs)
                          then increasing(tail xs)
                          else False
```

```
Prelude> :l increasing01.hs
[1 of 1] Compiling Main             ( increasing01.hs, interpreted )
*Main> increasing [1,2,3,4,5]
True
*Main> increasing [10,2,3,4,5]
False
*Main> increasing [1,2,30,4,5]
False
*Main> increasing [1,2,3,4,5,10]
True
*Main> 
```

* Using pattern matching

```increasing02.hs
increasing :: (Ord a) => [a] -> Bool

increasing [] = True
increasing [x] = True
increasing (x:y:ys) = x <= y && increasing(y:ys)
--- in line above paranthesis are required for pattern when matching several things at once
--- '_' underscore matches everything
```

can be re-writtern as

```increasing03.hs
increasing :: (Ord a) => [a] -> Bool

increasing (x:y:ys) = x <= y && increasing(y:ys)
--- '_' underscore matches everything
increasing _ = True
```

---

* example to remove vowels from word(s)

```noVowels01.hs
noVowels :: [Char] -> [Char]
noVowels word = if word == ""
                then ""
                else if head word `elem` "aeiouAEIOU"
                     then noVowels (tail word)
                     else (head word) : noVowels (tail word)

--- usage: noVowels "Kabhi Kabhi"
```

can be re-written with pattern matching

```noVowels02.hs
noVowels :: [Char] -> [Char]

noVowels "" = ""
noVowels (x:xs)
         | x `elem` "aeiouAEIOU" = noVowels xs
         | otherwise             = x : noVowels xs
```

---

* another example, watch

```watch01.hs
watch :: Int -> [Char]
watch n = if n == 7
          then "7 o'clock and ... SHARKNADO!"
          else show n ++ " o'clock and all's well."

--- usage: watch 2
--- usage: watch 7
```

can get rid of small redundancy using 'where' and pattern matching

```
watch :: Int -> [Char]
watch n = show n ++ " o'clock and " ++ message n
          where message 7 = "... SHARKNADO!"
                message _ = "all's well"
```

re-written using case

```
watch :: Int -> [Char]
watch n = show n ++ " o'clock and " ++ case n of 7 -> "... SHARKNADO!"
                                                 _ -> "all's well"
```
---

* example that calculates acceleration due to gravity at distance 'r' from center of earth

```
gravity :: (Fractional a) => a -> a
gravity r = 6.674e-11 * 5.972e24 / (r ^ 2)
```

it has bunch of mysterious constants, can just define them within methods locally if just used there

```
gravity :: (Fractional a) => a -> a
gravity r = let g = 6.674e-11
                earthMass = 5.972e24
            in g * earthMass / (r ^ 2)
```
---

* a definition looks like

```
_pattern_ = _result_
...
```

a good idea to make last pattern match everything,

* a guard expression looks like following, the 'otherwise' clause is not mandatory but good practice

```
_pattern_
  | _expression_ = _result_
  ...
  | otherwise = result
```

* a where clause looks like following, can only be used inside definition and can't be nested

```
_result_ where
_pattern_ = _result_
...
```

* let expression can be used anywhere and can be nested

```
let _pattern_ = _result_
  ...
in _result_
```

* case expression is like

```
case _expression_ of _pattern_ -> _result_
    ...
```

Haskell cares about indentation, corresponding part shall be indented to same level and subordinates indented further.

---


