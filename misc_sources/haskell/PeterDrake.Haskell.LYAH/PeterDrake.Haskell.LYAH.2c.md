
### 2c :: Lists


haskell data structures are immutable

```
Prelude> let numbers = [4,8,15,16,23,32]

Prelude> head numbers
4

Prelude> head numbers
[8,15,16,23,32]

Prelude> tail (tail (tail ( tail numbers) ) )
[23,32]

Prelude> numbers
[4,8,15,16,23,32]
```

cons operator 'colon'

```
Prelude> 5 : []
[5]

Prelude> 1 :2 : 5 : 10
[1,2,5,10]

Prelude> 99 : tail numbers
[99,8,15,16,23,32]
```

head and tail take constant time
length, reverse, !!(index-element) like things take linear time

```
Prelude> length numbers
6

Prelude> reverse numbers
[32,23,16,15,8,4]

Prelude> numbers !! 3
16

Prelude> last numbers
32

Prelude> init numbers
[4,8,15,16,23]
 
Prelude> null numbers
False

Prelude> null []
True

Prelude> elem 15 numbers
True

Prelude> [1,2,3] ++ [4,5,6]
[1,2,3,4,5,6]

Prelude> ['h','a','s','k','e','l','l']
haskell

Prelude> "AAA" < "BBB"
True

Prelude> "AAA" < "AAA"
False

Prelude> [[1,2,3],[4,5,6]]
[[1,2,3],[4,5,6]]

Prelude> maximum numbers
32

Prelude> minimum numbers
4

Prelude> sum numbers
98

Prelude> product numbers
5652480

Prelude> sum [1..100]
5050

Prelude> ['a'..'e']
abcde

Prelude> [2,4..10]
[2,4,6,8,10]

Prelude> [10,9..1]
[10,9,8,7,6,5,4,3,2,1]

--- producing infinite list, being lazy can be iterated over
Prelude> [1..]

Prelude>

```
