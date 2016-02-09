

### 2b :: Functions, If, Let

```program.hs
--- defining function 'hypotenuse'
hypotenuse a b = sqrt (a ^ 2 + b ^ 2)

--- defining a function using 'if'
identifyCamel humps = if humps == 1
                        then "dromedary"
                        else "bactrian"
```

at 'ghci' can load the program as ':load programFile' or ':l programFile'

can use 'let' to decalre function on the fly when in ghci

```ghci
Prelude> :load program
Prelude> hypotenuse 10 20
22.360679774997898

Prelude> identifyCamel 1
"dromedary"

Prelude> identifyCamel 2
"bactrian"

Prelude> let hyp a b = sqrt (a ^ 2 + b ^ 2)
Prelude> hyp 10 20
22.360679774997898

Prelude> let x = 10.0
Prelude> let y = 20.0
Prelude> hypotenuse x y
22.360679774997898

Prelude> let x = 10
Prelude> let y = 20
Prelude> hypotenuse x y
22.360679774997898

```

