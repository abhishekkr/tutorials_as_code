
### 2d :: List Comprehension

```ghci
GHCi, version 7.10.1: http://www.haskell.org/ghc/  :? for help

Prelude> [2^n | n <- [1..10]]
[2,4,8,16,32,64,128,256,512,1024]

Prelude> [2^n | n <- [1..10], 2^n >= 10, 2^n < 100]
[16,32,64]

Prelude> [x | x <- "haskell", not (elem x "aeiou")]
"hskll"

Prelude> [x | x <- "haskell", not (x `elem` "aeiou")]
"hskll"

Prelude> [ [x | x <- word, not (elem x "aeiou")] | word <- ["miranda", "lisp", "prolog", "ocaml", "haskell", "erlang"]]
["mrnd","lsp","prlg","cml","hskll","rlng"]

Prelude> [ [x * y | y <- [1..10]] | x <- [1..3] ]
[[1,2,3,4,5,6,7,8,9,10],[2,4,6,8,10,12,14,16,18,20],[3,6,9,12,15,18,21,24,27,30]]

Prelude> 

```
