
### 2e :: Tuples

* length of a list is not part of its type; different length list can be compared
* length of tuple is part of it and all element may not be same type

```ghci
Prelude> [1,2]
[1,2]

Prelude> (1,2)
(1,2)

Prelude> (1,2,3,(4,5,6))
(1,2,3,(4,5,6))

Prelude> [1,2,"a"]

<interactive>:35:2:
    No instance for (Num [Char]) arising from the literal ‘1’
    In the expression: 1
    In the expression: [1, 2, "a"]
    In an equation for ‘it’: it = [1, 2, "a"]

Prelude> (1,2,"a")
(1,2,"a")
Prelude> 

```

* Tuples with 2,3,4 elements are called Pair, Triple, Four Tuple.

> For Pair Tuple

```
Prelude> let tpl = ("James", "Bond")

Prelude> tpl
("James","Bond")

Prelude> fst tpl
"James"

Prelude> snd tpl
"Bond"

Prelude> let firstNames = ["James", "Sherlock", "Doctor"]
Prelude> let lastNames = ["Bond", "Holmes", "Who"]
Prelude> zip firstNames lastNames 
[("James","Bond"),("Sherlock","Holmes"),("Doctor","Who")]

```

Trying solve a puzzle "when does a smaller number has longer name"

```
Prelude> 
Prelude> let numbers = [1..10]
Prelude> let words = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]
Prelude> let pairs = zip numbers words
Prelude> pairs
[(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five"),(6,"six"),(7,"seven"),(8,"eight"),(9,"nine"),(10,"ten")]

Prelude> --- making all pair combinations for pair first elements
Prelude> [(fst p, fst q) | p <- pairs, q <- pairs]
[(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9),(2,10),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9),(3,10),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7),(4,8),(4,9),(4,10),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7),(5,8),(5,9),(5,10),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6),(6,7),(6,8),(6,9),(6,10),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7),(7,8),(7,9),(7,10),(8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(8,7),(8,8),(8,9),(8,10),(9,1),(9,2),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),(9,9),(9,10),(10,1),(10,2),(10,3),(10,4),(10,5),(10,6),(10,7),(10,8),(10,9),(10,10)]

Prelude> [(fst p, fst q) | p <- pairs, q <- pairs, fst p < fst q]
[(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9),(2,10),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9),(3,10),(4,5),(4,6),(4,7),(4,8),(4,9),(4,10),(5,6),(5,7),(5,8),(5,9),(5,10),(6,7),(6,8),(6,9),(6,10),(7,8),(7,9),(7,10),(8,9),(8,10),(9,10)]

Prelude> [(fst p, fst q) | p <- pairs, q <- pairs, fst p < fst q, length(snd p) < length(snd q)]
[(1,3),(1,4),(1,5),(1,7),(1,8),(1,9),(2,3),(2,4),(2,5),(2,7),(2,8),(2,9),(4,7),(4,8),(5,7),(5,8),(6,7),(6,8),(6,9)]
Prelude> 

```
