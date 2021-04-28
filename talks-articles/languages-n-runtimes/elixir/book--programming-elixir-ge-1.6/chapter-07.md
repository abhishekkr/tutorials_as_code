
## Chapter.7 Lists and Recursion

> sample code at [chapter-07.exs](./chapter-07.exs)

### Head and Tails

* List is empty or contains Head and Tail, where Head is an element and Tail is remaining list

```
[]
[ 3 | [] ]
[ 1 | [ 2 | [ 3 | [] ] ] ]
[ 1 | 2 | 3 | [] ]
```

```
iex> [a, b, c] = [:x, :y, :z]
[:x, :y, :z]
iex> a
:x
iex> b
:y
iex> c
:z
iex> [h | t] = [:x, :y, :z]
[:x, :y, :z]
iex> h
:x
iex> t
[:y, :z]
```

* character sequence with single quotes are charlists

```
iex> [99, 97, 96]
'ca`'
iex> [99, 97, 116]
'cat'
```


### Using Head and Tail to Process a List

* similar to following

```
def list_len([]),             do: 0
def list_len([_head | tail]), do: 1 + list_len(tail)
```


### Using Head and Tail to build a List

* similar to following

```
def incr1([]),            do: []
def incr1([head | tail]), do: [head+1 | incr1(tail)]
```


### Creating a Map Function

* similar to following

```
def map([], foo),            do: []
def map([head | tail], foo), do: [ foo.(head) | map(tail, foo) ]
```


### Reducing a List to a single value

* similar to following

```
def reduce([], base_value, _foo),           do: base_value
def reduce([head | tail], base_value, foo), do: reduce(tail, foo.(head, base_value), foo)
```


### More Complex List Patterns

* can match multiple elements as `[a, b, c | tail] = [1, 3, 5, 7, 9]`

* sample code for list of list matching iterations with `UserFilter.for_user_7/1`, `UserFilter.for_user/2` and `UserFilter.for/2`


### The List Module in Action

* sample code module `ListModule` showcases some List module powers

---
