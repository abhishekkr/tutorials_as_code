
## Chapter 1.2 Sequential Programming

### 1.2.1 Erlang Shell

* can start a repl using `erl`

* try out simple commands, but make sure to mark their processing by fullstop `.`
```
1> 2 + 5
1> .
7

2> 2+5.
7

3> (2+3)*5.
25
```

* to exit use `Ctrl + g` followed by `a`bort.

```
^G
a
```

---

### 1.2.2 Modules and Functions

* a sample module [chapter1-2-01.erl](./chapter1-2-01.erl) exports a function called `double`
> * `module(name)` to define the module
>
> * `export([$fnName/$numOfArguments])` to export the functions that can be used
>
> * and variables must start with capital letter

* as following a erlang module can be compiled and loaded in erlang shell, then used with module namespaced function
> exported functions could be used as `$moduleName:$fnName($arguments...)`

```
1> c('chapter1-2-01').
{ok,'chapter1-2-01'}

2> 'chapter1-2-01':double(2).
4
```

* the module definition in code and while compiling is in single quotes due to non-alphanumeric characters

* sample module [chapter1-2-02.erl](./chapter1-2-02.erl) exports a function called `factorial`
> * `factorial(1)` end with `;` indicates there is more function to come
>
> * it ends with `.` after final pattern gets defined for function
>
> * multiple functions to be exported are just multiple elements in list of exported notation

```
1> c('chapter1-2-02').
{ok,'chapter1-2-02'}

2> 'chapter1-2-02':factorial(3).
6
```

---

### 1.2.3 Atoms

* Atoms are an erlang data-type, name starting with small letter. These are simply names, nothing else (like symbols in some languages).

* sample module [chapter1-2-03.erl](./chapter1-2-03.erl) uses `inch` and `centimeter` atoms for pattern matching correct action for function
> these 2 are called `clauses`, in case of unidentified clause we get error message about `function_clause`

```
1> c('chapter1-2-03').
{ok,'chapter1-2-03'}

2> 'chapter1-2-03':convert(5, inch).
1.968503937007874

3> 'chapter1-2-03':convert(5, centimeter).
12.7

4> 'chapter1-2-03':convert(5, chi).
** exception error: no function clause matching 'chapter1-2-03':convert(5,chi) (chapter1-2-03.erl, line 7)
```

* the function definition isn't clear enough, they can be coupled using tuples

---

### 1.2.4 Tuples

* tuples have fixed length elements, defined using `{...}`

* `convert_length` from module [chapter1-2-03.erl](./chapter1-2-03.erl) uses atom grouped value
> can be nested because of call definition is same as return type

```
1> 'chapter1-2-03':convert_length({inch, 5}).
{centimeter,12.7}

2> 'chapter1-2-03':convert_length('chapter1-2-03':convert_length({inch, 5})).
{inch,5.0}
```

---

### 1.2.5 Lists

* list have variable length elements surrounded by `[...]`

* lists can be split into parts using `|`
> the Variable at the end contains subset of list left past elements before `|`
>
> the subset could be empty as well

```
1> [Fst |Rst] = [1,2,3,4,5].
[1,2,3,4,5]

2> Fst.
1

3> Rst.
[2,3,4,5]

4> [First, Second | Rest] = [1,2,3,4,5].
[1,2,3,4,5]

5> First.
1

6> Second.
2

7> Rest.
[3,4,5]

8> [M | Em] = [1].
[1]

9> M.
1

10> Em.
[]
```

* `list_len` from module [chapter1-2-04.erl](./chapter1-2-04.erl) uses pattern-matching in list
> `_` is anonymous variable, used as placeholder for variable which don't have a use
>
> used model is not tail-recursion

```
1> 'chapter1-2-04':list_len([]).
0

2> 'chapter1-2-04':list_len([1]).
1

3> 'chapter1-2-04':list_len([1,2,3,4,5]).
5
```

* erlang don't have purist string data type, it's list of ASCII characters
> `[97,98,99]` is same as `abc`

---

### 1.2.6 Standard Modules and Manual Pages

* erlang has lots standard modules, to read manual for any module like `io` do `erl -man io`

---

### 1.2.7 Writing output to a stdout

* `io:format/2` takes 2 list arguments
> * `~w` replaced by a term taken in order from second argument
> * `~n` replaced by a newline
> * function returns the atom `ok`

```
1> io:format("ohai ahoy", []).
ohai ahoyok

2> io:format("ohai: ~w~n", [ahoy]).
ohai: ahoy
ok

3> io:format("ohai: ~w~n~w~n", [ahoy, hola]).
ohai: ahoy
hola
```

---

### 1.2.8 A Larger Example

* module [chapter1-2-08.erl](./chapter1-2-08.erl) using fundamental constructs

```
1> 'chapter1-2-08':format_temps([
1>   {xcity, {c, -7}},
1>   {ycity, {f, 89}}
1>   ]).
xcity           -7 c
ycity           31.666666666666668 c
```

* anything following `%` is a comment

* any function not in `export` is a local and need to be used at least once within module

* newlines are allowed at sensible breaks within a definition

* in case of nested function call, it gets evaluated from inside out

* in `print_temo`, notation `~15w` specify a field length of 15 width

---

### 1.2.9 Matching, Guards and Scope of variables

* module [chapter1-2-09.erl](./chapter1-2-09.erl) exports `max_of_list` allowing to find maximum value of elements in list

```
1> 'chapter1-2-09':max_of_list([1,4,6,4,7,3,2]).
first is greater than results so far
first: 4, rest: [6,4,7,3,2]
first is greater than results so far
first: 6, rest: [4,7,3,2]
first is not greater than results so far
first: doesn't matter, rest: [7,3,2]
first is greater than results so far
first: 7, rest: [3,2]
first is not greater than results so far
first: doesn't matter, rest: [2]
first is not greater than results so far
first: doesn't matter, rest: []
result is: 7
7
```

* different commands in function separated by comma `,`

* here `max_of_list` has a polymorphic definition on `name/arity` basis

* can follow the flow of logic going through result of `io:format`

* a Variable can only be given value once in its scope
> in example Variable like `Result_so_dar`, `First` and others have different scope in every function definition

* we'll notice one of function definition has conditional evaluation to be picked instead of simple parameter count matching

* some usefule operators in guards `< for less-than`, `> for greater-than`, `== for equal`, `<= for less-equal`, `>= for greater-equal` and `/= for not-equal`

* in private `max_of_list` definition, arguments can be pulled apart creating new ones using match operator to be used in function definition

```
1> {X, Y} = {oneTwo, {1, 2}}.
{oneTwo,{1,2}}
2> X.
oneTwo
3> Y.
{1,2}

4> [First,Second|Rest] = [1,4,2,4,6,7].
[1,4,2,4,6,7]
5> First.
1
6> Second.
4
7> Rest.
[2,4,6,7]
```

* `fst_snd_rest` definition represents simple match operator use and `io:format` printing string argument

---

### 1.2.10 More about Lists

* `|` operator can be used to separate required head elements and rest of it

* `|` can also be used to push an element as head to a list

```
1> [Lw|Lx] = [5,1,2,3].
[5,1,2,3]
2> Lx.
[1,2,3]
3> Lw.
5

4> Ly = [0|Lx].
[0,1,2,3]
```

* module [chapter1-2-10.erl](./chapter1-2-10.erl) exports `rev_list` which reverses order of list elements using above push notation

```
1> 'chapter1-2-10':rev_list([1,2,3,4,5]).
calling with
(rest: [2,3,4,5], [first: 1, rev-list: []])
calling with
(rest: [3,4,5], [first: 2, rev-list: [1]])
calling with
(rest: [4,5], [first: 3, rev-list: [2,1]])
calling with
(rest: [5], [first: 4, rev-list: [3,2,1]])
calling with
(rest: [], [first: 5, rev-list: [4,3,2,1]])
[5,4,3,2,1]
```

* `format_temps` has been re-written with list append notation
> * `find_max_and_min` redefined using same list notation
>
> * `if` construct in `find_max_and_min` can be used to assign value out to `Max_City` or get it exported from `Min_City`; `Max_City` assignment is preferref way

```
1> 'chapter1-2-10':format_temps([{xcity, {f, 32}}, {yxcity, {c, 2}}, {zcity, {f, 44}}]).
xcity           0.0 c
yxcity          2 c
zcity           6.666666666666667 c
Max temperature was 6.666666666666667 c in zcity
Min temperature was 0.0 c in xcity
ok
```

---

### 1.2.11 If and Case

* `if` construct as defined below, or used in previous section example
> * no `;` before `end`
>
> * conditions are same as guards, test which succeed or fail
>
> * conditions are checked from top, and once matched all following are skipped, there will be run-time failure in case of no match
>
> * `true`is an atom, condition which always succeds

```
if
  Condition1 ->
    action1;
  Condition2, Condition22 -> %% "," marks "and", as "Condition2 && Condition22"
    action2;
  Condition3; Condition33 -> %% ";" marks "or", as "Condition3 || Condition33"
    action3
end
```

* another construct is `case` for similar flows
> behavior can be modified using guards

```
case Value of
  Match1 ->
    action1;
  Match2 ->
    action2
end.
```

* module [chapter1-2-11.erl](./chapter1-2-11.erl) exports `month_length` using `if` and `case` with `when` to figure out day count in any month in any year

```
1> 'chapter1-2-11':month_length(2016, feb).
29
2> 'chapter1-2-11':month_length(2018, feb).
28
3> 'chapter1-2-11':month_length(2018, mar).
31
```

---

### 1.2.12 Built In Functions (BIFs)

* in previous section module, used a BIF `trunc` to cut out decimals

* `rem` used as below returns remainder value from division

```
28> trunc(1.1).
1
29> 200 rem 3.
2
30> trunc(200/3).
66
```

* only a few BIFs can be used in guards, can't use self-defined functions to avoid side-effects

```
1> round(1.1).
1
2> length([a,p,d,f,g]).
5
3> float(1).
1.0
4> is_atom(abc).
true
5> is_atom("abc").
false
6> is_tuple({p,q,r}).
true
7> is_tuple([p,q,r]).
false
8> atom_to_list(abc).
"abc"
9> list_to_atom("abc").
abc
10> integer_to_list(123).
```

* division returning round-ed integer

```
1> 4 div 2.
2
2> 5 div 2.
2
```

* BIFs allow conversions, otherwise impossible in Erlang

---

### 1.2.13 High Order Functions (Funs)

* an example

```
1> Fx = fun(X) -> X+X end.
#Fun<erl_eval.6.118419387>

2> Fx(10).
20
```

* module [chapter1-2-13.erl](./chapter1-2-13.erl) exports `foreach` and `map` as useful functions to work with list

```
1> 'chapter1-2-13':foreach(fun(X) -> X+X end, [1,2,3,4,5]).
: 2
: 4
: 6
: 8
: 10
ok

2> 'chapter1-2-13':map(fun(X) -> X+X end, [1,2,3,4,5]).
[2,4,6,8,10]

3> 'chapter1-2-13':convert_list_to_c([{xcity, {f, 43}}]).
[{xcity,{c,6}}]
```

* matching list elements to sort function

```
1> lists:sort([3,4,6,5,2,1]).
[1,2,3,4,5,6]

2> lists:sort(fun({A, B}, {X, Y}) -> B < Y end, [{3,4},{6,5},{2,1}]).
[{2,1},{3,4},{6,5}]
```

---

### (update from 2.6 v10.1) 1.2.u14 Maps

* key-value association encapsulated with `#{...}`, like `#{r => R, g => G, b => B}`
> * `=>` is allowed when creating a new map
>
> * `:=` is used while updating value in existing map

* module [chapter1-2-u14.erl](./chapter1-2-u14.erl) shows color blending with alpha channels
> * check functions like `red` to see pattern matching in parameters for maps
>
> * macro `is_channel` is defined to be ised with guard tests; checks value range and type

```
1> Rgb1 = 'chapter1-2-u14':new(0.2,0.3,0.4,0.5).
#{alpha => 0.5,blue => 0.4,green => 0.3,red => 0.2}

2> Rgb2 = 'chapter1-2-u14':new(0.3,0.4,0.5,1.0).
#{alpha => 1.0,blue => 0.5,green => 0.4,red => 0.3}

3> 'chapter1-2-u14':blend(Rgb1, Rgb2).
#{alpha => 1.0,blue => 0.45,green => 0.35,red => 0.25}

4> 'chapter1-2-u14':blend(Rgb2, Rgb1).
#{alpha => 1.0,blue => 0.5,green => 0.4,red => 0.3}
```

* can check [The Preprocessor](http://erlang.org/doc/reference_manual/macros.html) for more information on macros

---
---
