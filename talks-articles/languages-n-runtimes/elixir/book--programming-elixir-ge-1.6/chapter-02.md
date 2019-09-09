
## Chapter.2 Pattern Matching

* similar to that of erlang, except variable can be re-matched so not immutable

> As per Joe Armstrong, `=` used here is like that in Algebra for mapping symbols and values for entire formula

* `a = 1` isn't 1 assigned to a but `matched`, `=` is `match operator`

> * elixir match true binding RHS to LHS if RHS is an infer-able value and LHS is variable

```
a = 1  ## true, returns match 1
1 = a  ## since a is pre-matched to 1, true, returns match 1
2 = a  ## false, as a is pre-matched to 1; causes error
```

### Complex Matches

* list is superset of elements and empty list, good for pattern matching to extract specific elements

> * to ignore a field `_` is used

```
iex> contact007 = ["James", "Bond", ["007", "kill"]]
["James", "Bond", ["007", "kill"]]
iex> ["James", "Bond", ["007", "kill"]] = contact007
["James", "Bond", ["007", "kill"]]

iex> [fname, lname, _] = contact007
["James", "Bond", ["007", "kill"]]
iex> fname
"James"
iex> lname
"Bond"

iex> [fname, lname, [code, _]] = contact007
["James", "Bond", ["007", "kill"]]
iex> code
"007"

iex> [_, _,[_, licens_to]] = contact007
["James", "Bond", ["007", "kill"]]
iex> licens_to
"kill"

iex> [fname, lname, details] = contact007
["James", "Bond", ["007", "kill"]]
iex> details
["007", "kill"]
```

* Pattern matching combined with only situation where a condition is satisifed, as below based on a field's value

```
iex> [f1name, _, ["007", _]] = contact007
["James", "Bond", ["007", "kill"]]
iex> f1name
"James"
iex> [f2name, _, ["008", _]] = contact007
** (MatchError) no match of right hand side value: ["James", "Bond", ["007", "kill"]]

f2name is undefined as above failed
```

```
iex> [a,a] = [1,2]
** (MatchError) no match of right hand side value: [1, 2]
iex> [a,a] = [1,1]
[1, 1]
iex> a
1
```

* To use existing value of a variable in pattern, use `^` as `^a = 1` would fail if not pre-matched

---
