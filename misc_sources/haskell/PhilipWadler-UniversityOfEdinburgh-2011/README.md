* [UCLA CamlBack WebApp](http://camlback.cs.ucla.edu/phpredux.php)
* [UCLA CamlBack Project @github](http://camlback.github.io/)

* [Micro Haskell SampleCode](./lecture16.example00.hs)

---

* (n+k) patterns

> * WARNING
> some examples here will have '(n+k) pattern' which is banned from Haskell-2010
> try those examples with 'Guards' instead as following
```
f 0     = 0
f (n+5) = n
--@ghci-- let f 0 = 0 ; f (n+5) = n
-- f :: (Integral t) => t -> t

f' 0 = 0
f' n | n >= 5 = n'
  where
    n' = n - 5
--@ghci-- let f' 0 = 0 ; f' n | n >= 5 = n' where n' = n - 5
-- f' :: (Num t, Ord t) => t -> t
{- also in let notation
let f'' 0 = 0
f'' n | n >= 5 =
  let n' = n - 5
  in n'
-}
{- fixing signature
let f 0 = 0 ; f n | toInteger n >= 5 = n' where n' = n - 5
-}
```
> also like
```
-- (Integral t) => t -> t
factorial  0    =  1
factorial (n+1) = (n+1) * factorial n
-- results same as
-- (Num t) => t -> t
factorial' 0 = 1
factorial' n = n * factorial' (n-1)
```

---
