
## Chapter.2 Erlang language essentials

This covers variables and pattern matching; function clauses and guards; case switches and funs (lambda expressions); exceptions; list comprehensions; binaries and bit strings; records, preprocessor includes and macros; process operations and message passing; ETS tables; recursion.

### 2.1 The Erlang Shell

Erlang doesn't handle running code like other programs but more like an OS.

#### 2.1.1 Starting the shell

Highly interactive, compiler/interpreter doesn't just make code run till it finishes and return control to OS. Erlang is designed to run continuosuly with interactive development, also debugs and upgrades.
Interaction happens via shell.

#### 2.1.2 Entering expressions

Run `erl`, if on windows then `werl`, this opens shell. Can be run via `-noshell` in case of batch job or daemon.

Expressions need to end with period '.'. Expressions don't end if one forgets closing quote as well, can followup with closing quote and period character. Can use the up arrow or Ctrl+P to go back and edit line.

Shell by default keeps 20 results.


#### 2.1.3 Shell functions

* `help()` prints available shell functions
* `h()` prints command history
* `v(N)` fetchs computed value at prompt N
* `cd(Dir)` changes current dir to Dir value
* `ls([Dir])` prints dir listing, Dir is optional
* `pwd()` prints working/current dir
* `q()` quits, shorthand for `init:stop()`
* `i()` prints info about what system is running
* `ni()` info about networked systems
* `memory()` prints memory usage info
* `memory(T)` prints memory info about Type
* `c([File])` compile and load code from file
* `bt(Pid)` stack backtrace for a process
* `l(Module)` load/reload a module
* `lc([File])` compile a list of erlang modules
* `nc(FileOrModule)` compile and load code/module on all nodes
* `m()` list loaded modules
* `m(Mod)` info about module Mod
* `regs()` info about registered processes
* `uptime()` prints node uptime

#### 2.1.4 Esacping from the shell

There are different ways of leaving shell and stopping entire system.

* Calling `q()` or `init:stop()` is safest is to run `q()`, tells running applications to stop and giving them time to stop and respond.

* The low-level `BREAK` menu by Ctrl-C/Break followed by `a` gives a chance of unsafe quick exit.

* Ctrl-G is user switch command menu. Following `q` gives quick shutdown. Remaining options there are for job control.

#### 2.1.5 Job-control basics

* If you run something that'll be there forever and there are other important processes running, can kill current job and start new one without affecting others.

```
%% lock the shell
timer:sleep(infinity)

%% bring up user command switch menu
Ctrl-G
 --> h
  c [nn]            - connect to job
  i [nn]            - interrupt job
  k [nn]            - kill job
  j                 - list all jobs
  s [shell]         - start local shell
  r [node [shell]]  - start remote shell
  q                 - quit erlang
  ? | h             - this message

%% press 'j' to list all jobs
j

%% connect to another job which is not to be killed (on local system)
c <job-to-safe-index>

%% kill the required job by index
k <job-to-kill-index>
```

---

### 2.2 Data types in Erlang

Data in erlang is referred as `terms`.

#### 2.2.1 Numbers and arithmetic

Two kinds integers and floats.

* Integers here can be arbitrarily sized. Can have integers in any base between 2 and 36. `$-notation` yields character code (ASCII/Latin-1/Unicode) for any character.

```
101
-101
100 * 100 * 9

16#FFffFFff
2#1001001
36#ZZ

$9
$z
$\n
```

* Floats are handled using 64-bit IEEE 754-1985 double-precision. All floating points need to be have a number before decimal point.

```
3.14
-0.12
6.1e-11
```

* If either or both arguments to arithmetic operators are float, result is made in floating point. Integer divisions are performed by a floor/truncating.

* Standard library module `math` with functions like `math:sqrt(2)`

* There are integer operators for bitwise operators, `N bsl K` shifts int N by K steps to left. Right shift by `bsr`. Bitwise logic operators are named `band`, `bor`, `bxor` and `bnot`. Example `X band (bnot Y)` masks away those bits from X that are set in Y.

#### 2.2.2 Binaries and bitstrings

* `binary` is a sequence of 8-bit bytes, `bitstring` is generalized binary whose length in bits isn't necessarily a multiple of 8, can be 12 bits long

* the basic syntax of binary is `<<0, 1, 2, ..., 255>>`, `<<"hey", 10, "you">>`; empty binary as `<<>>`
> there should not be any space after `<<` and before `>>` operators

* it's same as writing corresponding sequence of bytes for 8-bit character codes of strings

#### 2.2.3 Atoms

* An `atom` is a special kind of string constant. Normally written starting with a lowercae letter, they can contain uppercase letters, digits, underscores and `@` symbol. For atom with other symbols need `''` enclosed names. Length limited to 255 characters.

```
foo
fooBar
try_exit
bond007
james@bond
'$@#!'
'Some-Thing'
'My Name'
```

* Can have at max 1,048,576 characters at most. Atoms aren't removed from table until erlang system restarts. So don't auto-generate them.

* Internally these strings are stored in a table and are referred to by table index. So matching them is just table index compare. These index can vary from one run to another.

* Some popular atoms used in programs are `true`, `false`, `ok`, `undefined`

#### 2.2.4 Tuples

* Is a fixed length ordered sequence of other erlang terms. Written within curly braces.

```
{1,2,3}
{one,two,three}
{night, "comes", "for us"}
{can,{have,{nested},tuple}}
{}
```

* Standard convention in erlang is to label tuples to indicate what types of data they contain by using an atom as first element, as in `{size, 51}`. These are `tagged tuples`.

* Tuples are main way of constructing compound data structures or returning multiple values. Entries are numbered, accessing element is constant-time operation. Record syntax allow naming tuple entries. Pattern matching makes easy referencing different parts.

* Standard library contains modules Implementing more abstract data types as arrays, sets, dictionaries and more.

#### 2.2.5 Lists

* Lists are used to hold an arbitrary number of items, written with square brackets.

```
[]                    %% an empty list, known as 'nil'
[1,2,3]
[one,two,three]
[[1,2],[3,4]]
[{one, 1}, {two, 2}]
```

* Can add to a list making a new list easily using vertical bar `|`

```
[ 1 | [] ]            %% yields [1]
[ 2 | [1] ]           %% yields [2,1]
[ 5, 4, 3 | 2,1 ]     %% yields [5,4,3,2,1] done by adding first 3, then 4, then 5
```

* Can combine two lists of arbitrary lengths

```
[1,2,3] ++ [4,5]
%% result: [1,2,3,4,5], here length of left side decides how much time it takes as it need to be found to add more
```

#### 2.2.6 Strings

* Double-quoted chracter set is just another way of writing a list of chars in Erlang, called string.

```
"atoz"
"a\nz"
[97,98,99]
[$a,$b,$c,$\n]   %% works by '$' notation shown in number section
[]
```

* There are library functions available for returning string typecasting like `atom_to_list(A)`. Can do lots of string programming with list processing features.

#### 2.2.7 Pids, ports, and references

* All processes in erlang have unique `Pids`. Opaque objects but printed as 3 integers joined with dots like `<i.j.k>`, this is only for debugging purpose. The same identifier may be reused in long running Erlang systems. Function `self()` returns pid of process it was run in. When run in Erlang shell, gives shell's Pid.

* Much like process, but `port` can also communicate with outside world. Get printed like `#Port<0.123>`.

* References are used as unique one-off labels or cookies. Referred to as `references` or `refs`.

#### 2.2.8 Functions as data:funs

* For FP, it need to pass a function as data as argument or result; then be able to run it.

#### 2.2.9 Comparing terms

* Use built-in operators like `<,>,==`. Ordering on number hold naturally. For atoms, strings and tuples; they are organized lexicographically and so compared.

```
1 < 2
1.1 < 2
'aba' < 'abb'
"zzy" < "zzz"
[1,2,3] > [1,2,2,1]
{fred,baker,42} < {fred,cook,18}
```

* Ordering between different types, as any term can be compared with any term here.

```
42 < 'abc' %% numbers before atoms
{1,2,3} < [1,2,3] %% tuples before lists
'abc' < "abc" %% atoms before tuples and lists
```

* `lists:sort(...)` can be use from standard lib.

* Less-than operators are not like other languages (except Prolog) as `<=` due to reserved use as arrow but like `=<`. Greter-than is written like others as `>=`.

* Two kind operators for equality. Exact equality operator is `=:=`, negative form is `=/=`. Exact equality doesn't work for same number in integer and float. Second oprator is arithmetic equality `==`, negative of this is `/=`

```
42 =:= 42  %% true
42 =/= 43  %% true
2.0 == 2   %% true
2.1 /= 2   %% true
```

#### 2.2.10 Understanding lists

* Lists get special treatment. Created from empty list `nil` and `list cells` add one element a time on top of list building singly linked list in memory. Each cell uses 2 words, head for value and pointer for next word called tail.

* List cells are also called `cons cells`  for being constant. Adding new `cons` is `consing`.

* By convention first `cons` contain payload of list and from second starts the data.

* `[1, 2, [3, 4]]` is a list with 3 items. `[1, 2 | [3, 4]]` is list with 4 items made from stack 2 elements from outer list on top of inner.

```
[1, 2, [3, 4]]    %% [1,*-]-->[2,*-]-->[*-,[]]-->[3,*-]-->[4,[]]
[1, 2 | [3, 4]]   %% [1,*-]-->[2,*-]-->[3,*-]-->[4,[]]
```

* Lists are mostly good for temporary/intermediate data.

* `Proper` lists are the one described before. An `improper` list is created by stacking a list cell on something not already a list, like `[1 | what]`. It creates list with `what` tail. Don't generally use an improper list.

---

### 2.3 Modules and functions

Each module has a unique name, specified as an atom.

#### 2.3.1 Calling functions in other modules (remote calls)

* Using function like `lists:reverse(1)`, module name joining function of it with colon. Calling function from same module is `local call`, from another modules is `remote module`. Remote procedure call is asking another process on same or other computer.

#### 2.3.2 Functions of different arity

* Number of arguments required by function marks its `arity`. Nullary, unary, binary, ternary, and so on are functions passing 0, 1, 2, 3 arguments.

* Erlang treats functions with different arity differently, it's not overloading as such.

#### 2.3.3 Built-in functions and standard library modules

* The module named `erlang` contains functions that are central to erlang systems. Modules `lists`, `io`, `dict`, `array` are very common.

* Some low-level functions are intrinsic part of language and called BIFs (built-in functions), like Erlang also implemented in C (like `lists:reverse/1`).

* Few important functions are like `erlang:self()` are auto-imported, so don't need to prefix. Even operators of language belong here.

#### 2.3.4 Creating modules

* To make a module one need to write a module, compile and then load it like.

```
%% comments followed by %

%% declaration is anything other than function or comment and starts with hyphen
-module(module_name_atom).  %% module declaration
-export([methodToBeUsedWhereLoaded/0]). %% tells compiler of public functions


methodToBeUsedWhereLoaded() ->    %% definition before arrow is head, rest body
  io:format("oh hi!~n", []).      %% no return required, always returns value of last expression
```

* Anything following `%` is a comment, except one part of atom or quoted string.

#### 2.3.5 Compiling and loading modules

* Compiling `.erl` file, creates `.beam` object file that can be loaded and execute.

* Can compile and load using shell function `c(module_name_atom).`. To look in dir run `ls()`.

* Erlang automatically tries load a module from current dir. Can see load paths using `code:get_path().`.

#### 2.3.6 The stand-alone compiler, etc.

* `erlc module_name_atom.erl` can compile code directly from console.

* `erlc -o ./other_name_module module_name_atom.erl` can compile code with changed name object file.

#### 2.3.7 Compiled modules versus evaluation in the shell

* All code in `.beam` file gets compiled together in same context. It can mention which functions are exported, what's name of module and other things.

* Never measure on code interpreted by shell, write them as modules.

---

### 2.4 Variables and pattern matching

#### 2.4.1 Variable Syntax

* Variables begin with uppercase letter (lowercase starting names are reserved for atoms) or underscore, use CamelCase. If starting with underscore and follow with lowercase, then snake case.

```
A
Name
AddressLine1
_ThisVar
_that_var
```

* Compiler warns in case a variable got assigned but never used, except for underscored snake-case variables.

* Any unused variable will be optimized away. Can use them freely to annotate code.

#### 2.4.2 Single assignment

* Variables can be assigned only once in a scope, type is identified by inference. In shell to reset variable scope run `f(VarName).`.

* Simplest form of assignment is via `=` match operator.

#### 2.4.3 Pattern matching: assingment on steroids

* Pattern matching serves following purpose
> * Choosing control flow branches
>
> * Performing variable assingments (bindings)
>
> * Decomposing data structures (selecting and extracting parts)

* Match operator `=` does pattern matching instead of assignment. Thus `nonVar = value` wouldn't work as they wouldn't match.

```
Abc = abc.
{square, length} = {square, 105}. %% length gets 105
%% left side same name variables need to match same value, else it fails
{One, One, Two} = {11, 11, 21}.
```

#### 2.4.4 More about patterns

* Patterns look like expressions but limited. It can only contain variables, constants and constant data structures like lists/tuples (no operators, function calls, funs, others).

```
1> MyDetails = [{ person, [{name, "James", "Bond"}, {job, "spy"}, {tags, [mi,licenseToKill]}] }].
[{person,[{name,"James","Bond"},
          {job,"spy"},
          {tags,[mi,licenseToKill]}]}]

2> [{ person, [{name,_,Surname}, _, {tags, Tags}] }] = MyDetails.
[{person,[{name,"James","Bond"},
          {job,"spy"},
          {tags,[mi,licenseToKill]}]}]

3> Surname.
"Bond"

4> Tags.
[mi,licenseToKill]
```

* Use `_` to indicate don't care pattern.

* Splitting lists

```
1> [First, Second | Rest] = [1, 23, 45, 67, 89].
[1,23,45,67,89]

2> First.
1
3> Second.
23
4> Rest.
"-CY"
```

* With string (list of characters), `$A` representing 65 can be used as

```
1> [$h, $t, $t, $p, $:, $/, $/ | Uri] = "http://www.erlang.org".
"http://www.erlang.org"
2> Uri.
"www.erlang.org"
3> "https://" ++ Uri.
"https://www.erlang.org"
```

---

### 2.5 Functions and clauses

* Functions and pattern matching are connected.

#### 2.5.1 A function with side effects: printing text

* Standard lib function `io:format/2` is normal way of writing text to stdout stream in erlang. Takes 2 arguments, a format string and list of terms to be mapped. This output is a side-effect. From [chapter2_5_1.erl](./chapter2_5_1.erl).

```
%% code in chapter2_5_1.erl utilizes io:format for print as used below

1> c(chapter2_5_1).        %% compile+load the module
{ok,chapter2_5_1}

2> chapter2_5_1:print(123).
is 123 also 123
ok

3> chapter2_5_1:print("abc").
is "abc" also [97,98,99]
ok
```

* In above example `~p` means pretty-print, like character array as string. Line break using `~n`, erlang raw term as `~w`.

* In erlang, all side-effects are `messages`. Above `io:format` prepares message and sends it to console driver before sending `ok`.

#### 2.5.2 Multiple clauses and pattern matching for choice

* A function can consist of more than one `clause`. Clauses are separated by `;` and last clause terminated by a `.` All clauses must have same name and arity, and defined together. From [chapter2_5_1.erl](./chapter2_5_1.erl).

```
%% following example has 3 clauses

either_or_both(true, _) ->
  true;
either_or_both(_, true) ->
  true;
either_or_both(false, false) ->
  false.

%% 1> chapter2_5_1:either_or_both(true, true).
%% true
%% 2> chapter2_5_1:either_or_both(false, false).
%% false
%% 3> chapter2_5_1:either_or_both(false, true).
%% true
```

* Clause selection is done top-down using pattern matching. If none match, a runtime exception of type `function_clause` is raised.

* The last clause could have been `either_or_both(_, _)`, but it's good practice to make last left match explicitly known. Helps reader/debugger for much complex cases.

#### 2.5.3 Guards

* To add extra requirements to clauses to avoid unwanted flows. Like passing non-boolean term for do-not-care param in `either_or_both`. So writing something like `either_o_both` from [chapter2_5_1.erl](./chapter2_5_1.erl) would work. There are type tests for all primitive data types like `is_boolean/1`, `is_integer/1`, `is_atom/1` and more.

* Clause guard begin with keyword `when` and end with `->`. Can contain one or many tests separated by commas, all have to be true for selection.

* Things allowed in guard is limited. Can use most of operators `(+, -, *, /, ++, more)` and some BIFs like `self()`. Can't call your own functions or other module's functions, to avoid side-effects.

#### 2.5.4 Patterns, clauses, and variable scope

* [chapter2_5_1.erl](./chapter2_5_1.erl) assumes you are using tagged tuples representing info. Pattern matching decides which clause will be selected, binds data with variable in pattern. Order in the example doesn't matter as tags are mutually exclusive. Scope of variable bound in head of function clause is entire clause.

```
1> c(chapter2_5_1).
{ok,chapter2_5_1}

2> chapter2_5_1:area({circle, 2.1}).
13.854423602330987

3> chapter2_5_1:area({square, 5}).
25

4> chapter2_5_1:area({rectangle, 5, 8}).
40
```

* Variable out of scope gets GC-ed if not needed anywhere. Variable name can be reused in neighboring clauses if deemed fit.

---

### 2.6 Case and if expressions

* Other way for clause to manage flow control is Case statement. Case exceptions can also have one or more clauses, but only one pattern per clause. You can use guards.

```
area(Shape) ->
  case Shape of
    {circle, Radius} ->
      Radius * Radius * math:pi();
    {square, Side} ->
      Side * Side;
    {rectangle, Height, Width} ->
      Height * Width  %% no ';' for last clause, as they are separators
  end. %% period after 'end'

either_or_both(A, B) ->
  case {A, B} of  %% need a tuple to match case against multiple terms
    {true, B} when is_boolean(B) ->
      true;
    {A, true} when is_boolean(A) ->
      true;
    {false, false} ->
      false
  end.
```

#### 2.6.1 Boolean if-then-else switches in erlang

* Even if the choice left for last clause is obvious, use that directly instead of do-not-care term to allow fast failure for wrong usage. As in `either_or_both` use case, using it will only provide true or false but last clause shall match exact value as best practice. Allows decoupling of functions called and protection from future changes.

```
case either_or_both(X, Y) of
  true -> io:format("yay~n");
  false -> io:format("no~n")
end.
```

#### 2.6.2 If expressions

* If expressions are stripped down Case expressions, without specific switch value or pattern. Used when depend on flow only based on guards.

```
sign(N) when is_number(N) ->
  if
    N > 0   -> positive;
    N < 0   -> negative;
    N =:= 0 -> zero
  end.

signAsCase(N) when is_number(N) ->
  case dummy of
    _ when N > 0    -> positive;
    _ when N < 0    -> negative;
    _ when N =:= 0  -> zero
  end.
```

---

### 2.7 Funs

#### 2.7.1 Funs as aliases for existing functions

* Can refer to a function in same module by `sign/1`; bind it to a variable `F = fun sign/1`.

* Pass to another function as `sign_fn(fun sign/1, 10)`; call it if recieved as

```
sign_fn(F, N) ->
  case F(N) ->
    positive -> io:format("positive~n");
    negative -> io:format("negative~n");
    zero     -> io:format("zero~n")
  end.
```

* Above `sign_fn` is a `higher-order function` to accept a function as input. Same is for a function returning a function.
> These are used at places that delegates, adapters, commands, strategies and more in OOP languages.

* These local alias functions are similar to `anonymous funs` tied to current module version.

* Remote alias funs need be done as `fun module_name:sign_fn/1`. Their loading behavior is different as not bound to a particular version but latest available when called. These are symbolic.

#### 2.7.2 Anonymous Funs

* Also called `lambda expressions` are defined like `fun () -> task end`. To make use of these either bind them to a variable or pass them directly.

* An anonymous fun that does same as `area` function from above

```
fun ({circle, Radius}) ->
      Radius * Radius * math:pi();
    ({square, Side}) ->
      Side * Side;
    ({rectangle, Height, Width}) ->
      Height * Width
  end
```

* Local funs have short life tied to a specific version and not exist on reload. If sent as message to different erlang system, same version of code is required for it to work. For long-availability remote funs are better.

* `Closure` refers to extemely useful case of using variables within bounds of `fun...end` that are bound outside the fun. Current values of these variables get encapsulated.

```
-module(eganon).
-export([call_with_return_fn/1]).

call_with_return_fn(X) ->
    Fn = call_with(X),
    io:format("~p~n", [Fn(10)]).

call_with(X) ->
    io:format("~p~n", [X]),
    fun(X) -> X + 1 end. %% variable 'X' shadowed in 'fun' closure

%% 1> eganon:call_with_return_fn(22).
%% 22
%% 11
%% ok
```

> Let's say you have a list of items as pairs of strings, name and respective description. A function `to_html` creates HTML fragment, applies a callback fun provided which wraps name from pair in a markup of choice. Callback is applied after `to_html` HTML-escaped string, so you don't handle such details.
>
> Can make the names bold like `to_html(Items, fun (Text) -> "<b>" ++ Text ++ "</b>" end)`. Resulting from `[{"A&B", "a and b"}]` to `... <b>A&amp;B</b> ... a and b ...`.

```
render(Items, Em) ->
  to_html(Items,
    fun (Text) ->
      "<" ++ Em ++ ">" ++ Text ++ "</" ++ Em ++ ">"
    end).
```

> Such a fun will include, as a snapshot, current values of those variables bound outside the fun. Whether function called then or later,due to single assignment the variable value wouldn't change. Each fun live its own isolated life.
>
> The caller of `render` choses what `Em` to used for every name string.

---

### 2.8 Exceptions, try, and catch

* Exception return back to initial process call and process dies. There are 3 exception classes

> * `error` is runtime error kind of exception. If they cause process to die, it gets reported to Erlang error logger.
>
> * `exit` used to signal process is giving up, expected not to be caught but die and inform others of reason. Can also be terminated for job done with `ok`, in this case not reported to error logger.
>
> * `throw` is for user defined purpose. If isn't caught by the process, mutates into `error` exception with reason `nocatch`, terminating process and logging it.

#### 2.8.1 Throwing (raising) exceptions

* There is a BIF to throw each kind of exception. `throw(SomeTerm)` and `exit(Reason)` are auto-imported, `exit(Normal)` is a special case terminating as if process finished fine.

* To raise an error `erlang:error(Reason)` can be used.

#### 2.8.2 Using try...catch

* Can use `try` expression to handle exceptions. The patterns of `catch` clauses may contain `:`,  separating excpetion class with thrown term.

* If no clauses listed match, the exception will continue as if there was no try expression. Shouldn't normally catch `error` and `exit` if flow created carefully.

```
try
  some_unsafe_function()
catch
  oops          -> got_throw_oops;
  throw:Other   -> {got_throw, Other};
  exit:Reason   -> {got_exit, Reason};
  error:Reason  -> {got_error, Reason}
end
```

#### 2.8.3 try...of...catch

* Longer form of `try` is required if need flow for success and exception cases.

* Part under `of` is no longer protected.

```
try
  something_unsafe()
of
  O -> io:format("good~n");
  N -> something_new()
catch
  throw -> got_throw
end
```

#### 2.8.4 after

* `after` section guarantees execution for sake of side-effects right before the `end`, irrelevant of what happens in rest of `try` expression.

* `catch` part is not mandatory with it, but can be used.

* If `after` part throws excpetion, that takes over and earlier exceptions are forgotten.

```
{ok, FileHandle} = file:open("foo", [read]),
try
  process_file(FileHandle)
after
  file:close(FileHandle)
end
```

#### 2.8.5 Getting a strack trace

* Normally stack trace is stored Internally. Can be inspected using BIF `erlang:get_stacktrace()`.

* The stack trace is a list in reverse order (last call first). Each function represented as `{Module, Function, Args}`, first 2 are atoms and args either arity of function os list of arguments to call. Empty list suggest no caught exception.

#### 2.8.6 Rethrowing

* Can be done using BIF `erlang:raise(Class, Reason, Stacktrace)`. Here `Class` must be one of `error`, `exit` or `throw`. `Stacktrace` should be what one got from `erlang:get_stacktrace()`.

```
try
  do_this()
catch
  Class:Reason ->
    Trace = erlang:get_stacktrace(),
    case analyze_exc(Class, Reason) of
      true -> handle_exc(Class, Reason, Trace);
      false -> erlang:raise(Class, Reason, Trace)
    end
end
```

#### 2.8.7 Plain old catch

* Before `try` expressions were introduced, `catch` was there.

```
1> catch 2+3.
5

2> catch throw(some_error).
some_error

3> catch exit(some_error).
{'EXIT',some_error}

4> catch foo=bar.
{'EXIT',{{badmatch,bar},[{erl_eval,expr,3,[]}]}}

5> foo.
foo

6> catch Foo=bar.
bar

7> Foo.
bar
```

* `catch Expression` could have any expression. If it produces a result that gets returned. In case of exception, it's caught and presented as a result of catch.

* Should avoid old-style plain `catch`.

---

### 2.9 List comprehensions

* Mathematical set notation `{x | x ∈ N, x > 0}`. Vertical bar separates `template` part describing how individual elements are made up, from `generators and conditions` part that specifies what source for elements are and what are restrictions.

#### 2.9.1 List comprehension notation

* In erlang order of elements is important and what data structures you use.

```
%% creating a new list of only positive numbers
[ X || X <- ListOfNumbers, X > 0 ]
```

* If you have more than one generator in comprehension, it will cause it to go through all combinations of elements like nested loops. It's rarely useful.

#### 2.9.2 Mapping, filtering and pattern matching

* Single list comprehension can do `map` & `filter`.

* Greatest power comes with pattern matching. Left side of `<-` can be any pattern, not necessarily a variable. Generators already have a built-in condition, only patterns matching elements are considered. Assuming there is a list of tuples.

```
[ math:pow(X,2) || X <- ListOfIntegers, X > 0, X rem 2 == 0 ]. %% select +ive even numbers

[ X+1 || {_, X, _, _} <- [{1, 2, 3, 4}, {-1, -2, -3, -4}], X > 0 ]. %% gives [3] using pattern matching
```

---

### 2.10 Bit syntax and bitstring comprehensions

* Binaries and bitstrings discussed in `section 2.2.2`. In modern erlang `bitstring` could be any length.

* `Bit syntax` allows creating bitstring of size and layout required, can be used in patterns to extract segments from bitstring (like data from a file or socket). With `comprehensions`, notation gets more powerful.

#### 2.10.1 Building a bitstring

* Written as `<<Segment1, ...SegmentN>>` with 0 or more segment specifiers between double less-than/greater-than delimiters. In bits, size of bitstring is sum of all segments' length.

* Segment specifier can be declared in one of following forms.

```
Data
Data:Size
Data/TypeSpecifiers
Data:Size/TypeSpecifiers
```

* Here `Data` is an integer, floating-point number or another bitstring. `Size` and `TypeSpecifiers` can be used to make Data of specific size and encoding/decoding. If an integer needs more bits than segment size, it's truncated to fit.

```
<<1,2,3>> %% 3 size segments, type defaults to integer
<<"abc">> %% same as <<$a,$b,$c>>

<< 1, 2, 256, 3333 >>. %% results truncation as <<1,2,0,5>>

%% returns <<1,2,0,5>> and assigns A=1, B=2, C=0, D=5 by pattern matching
<<A, B, C, D>> = << 1, 2, 256, 3333 >>.
```

* You can't concat 2 bitstrings without specifying their type being bits.

```
B1 = <<1,2>>,
B2 = <<3,4>>,
%% <<B1, B2>> wouldn't work as B1,B2 assumed to be integers
<<B1/bits, B2/bits>> %% yields <<1,2,3,4>>
```

* Can control encoding and decoding through TypeSpecifiers. TypeSpecifiers can be one or more atoms separated by hyphen `-` like `integer-unsigned-big`, order isn't significant. Specifiers available are
> * integer (size 1), float (size 1), binary (size 8), bytes (alias for binary), bitstring (size 1), bits (alias for bitstring), utf8, utf16, utf32 (for utf size is determined by input)
>
> * signed, unsigned
>
> * big, little, native
>
> As a special case, can include `unit:Integer`

#### 2.10.2 Pattern matching with bit syntax

* Deconstructing bitstring using bit syntax pattern matching makes parsing a much simple task.

```
%% classic example to parse IP packet header using pattern function clause
ipv4(<<Version:4, IHL:4, ToS:8, TotalLength:16, Identification:16,
      Flags:3, FlagOffset:13, TimeToLive:8, Protocol:8, Checksum:16,
      SourceAddress:32, DestinationAddress:32, OptionsAndPadding:((IHL-5)*32)/bits,
      RemainingData/bytes>>) when Version =:= 4 ->
  ...

%% All values of packet with Version field as 4, get decoded into variables.
%% Excpet OptionsAndPadding is a bitstring whose length depends on previously decoded IHL field.
%% RemainingData segment contain all data following header.
%% Extracting a binary from another like this doesn't involve copying data, it's a cheap operation.
```

#### 2.10.3 Bitstring comprehensions

* Idea of list comprehensions extends to bitstrings. A bitstring comprehension is enclosed in `<<...>>`.
> Example, for a list of small numbers between 0-7 cam be packed into bitstring using only 3 bits per number.

```
<< <<X:3>> || X <- [1,2,3,4,5,6,7] >> %% returns <<41,203,23:5>>
%% here 23:5; it means total length is 8(for 41)+8(for 203)+5(specific size for 23)=21 bits i.e. also 7elements x 3bits
```

* Decoding such bitstring can be done with another bitstring comprehension. Here `<=` for parts of input is used instead of `<-` which only picks elements.

```
<< <<X:8>> || <<X:3>> <= <<41,203,23:5>>  >> %% it yields <<1,2,3,4,5,6,7>>
```

* To get a list instead of bitstring, use a list comprehension with a bitstring generator.

```
[ X || <<X:3>> <= <<41,203,23:5>> ] %% yields [1,2,3,4,5,6,7]
```

---

### 2.11 Record syntax

* Tuples are main building blocks for most kinds of structured data, but not much flexible. Records kind of work like structs from few other languages.

#### 2.11.1 Record declarations

* Record syntax lets you work with `records`, tagged tuples. First need to write a `record declaration`.

```
-record(user, {name="<johndoe>", address, phone}).
%% tells compiler to work with 4-tuples (3 fields plus tag)
%% 1st element is always atom 'user', other fields need to be in order of declaration
```

#### 2.11.2 Creating records

* Can use any variant from following

```
#user{} %% '#' followed record id atom makes compiler identify composition
#user{phone="019283"} %% can choose to give value for required fields
#user{name="Max Payne", address="Town Square", phone="09234521"} %% or all
```

* Fields not set will have a default value if specified in declaration, else gets atom `undefined`.

#### 2.11.3 Record fields and pattern matching

* Example in [chapter2_11_1.erl](./chapter2_11_1.erl) shows output for above made association.

```
1> chapter2_11_1:show_users().
name: "<johndoe>", address: undefined, phone: undefined
Contact: <johndoe> at 019283. Lives at undefined.
Contact: Max Payne at 09234521. Lives at Town Square.
ok
```

* Say if `A = #user{}`, `A#user.name` gives access to internal attributes.

#### 2.11.4 Updating record fields

* You don't update existing data structures part in Erlang. Can create a new modified shallow copy. Tuple creation is fast operation and Erlang is optimized for creating/recycling large count small tuples and list cells.

* Notation is similar to creating new record, must use another name then original will get recycled automatically.

```
X = #user{name="Max Payne", address="Town Square", phone="09234521"}
X1 = X#user{name="Nathan Drake"}
```

* Example `update_user()` from [chapter2_11_1.erl](./chapter2_11_1.erl) gives

```
1> chapter2_11_1:update_user().
Contact: Max Payne at 09234521. Lives at Town Square.
Contact: Nathan Drake at 09234521. Lives at Town Square.
ok
```

#### 2.11.5 Where to put the record declarations

* For records use only within single module, write it at top of module.

* For common record used across modules, set these definition to be shared in a separate `header file` and read by all modules needing it.
> This gets managed by `Preprocessor`.

---

### 2.12 Preprocessing and include files

* Erlang's preprocessor is a `token-level` preprocessor, works on sequence of tokens produced by splitting source file into separate words and symbols, not on characters. Makes it easier to reason, but also limits its capability.

* Preprocessor always runs as part of compilation process and performs 3 tasks: `macro expansion`, `file inclusion` and `condition compilation`.

#### 2.12.1 Defining and using macros

* Can define a macro with or without parameters using `define` directive, like following.

```
-define(PI, 3.14).
-define(pair(X,Y), {X,Y}).
```

* Macros names can be variables or atoms, traditional to use all uppercase for constants and mostly lowercase for other macros.

* To expand a macro later in source code, must prefix it with a `?`

```
circumference(Radius) -> Radius * 2 * ?Pi
%% expands to 'circumference(Radius) -> Radius * 2 * 3.14'

pair_of_pairs(A,B,C,D) -> ?pair( ?pair(A,B), ?pair(C,D) ).
%% expands to 'pair_of_pairs(A,B,C,D) -> { {A,B}, {C,D} }'
```

* Macros aren't substitute for proper functions but an escape route when normal functions wouldn't suit required abstraction.
> When syntax doesn't allow a function call and expansion need to happen at compile time.

* `undef` can be used to remove a macro definition

```
-define(foo, false). %% foo is defined false
-undef(foo).
-define(foo, true). %% foo is define true now
```

* Preprocessor predefines certain macros like `MODULE` macro (that expands to name of module being compiled in, as form of atom). Can use `FILE` and `LINE` macros to get current position in source file.

```
current_pos() -> [{module, ?MODULE}, {file, ?FILE}, {line, ?LINE}].
```

#### 2.12.2 Include files

* Using `include dorective` erlang can include header file `-include("<filename.hrl>").`. Have a file extension `.hrl`.

* Text of included file is read by preprocessor and inserted at point of include directive. Such file generally contain only declaration not functions.

* Header files can be loaded from `include path`, can add directory to include path using `-I` flag to `erlc`. Also by option `{i, Directory}` to `c(...)` shell function like `c("src/my_module", [ {i, "../include/"} ])`.

* Can include header file from other application or library by adding its file dir to include path. In addition, install path might contain a version number. Directive `include_lib` helps avoid most of trouble by `-include_lib("path/to/file.hrl")`. Looks for file in relative path to all paths erlang knows of.

#### 2.12.3 Conditional compilation

* Allows certain program parts to skip compilation.

```
-ifdef(MacroName). %% check if a macro is defined
...
-ifndef(MacroName). %% check if a macro isn't defined
...
-else.
...
-endif.

%% following macro is defined only if DEBUG macro is defined
-ifdef(DEBUG),
-export([foo/1]).
-endif.
```

* From cli or build system, can define amacro by option `{d,MacroName,Value}` to shell `c` function. Can also pass `-Dname=value` to erlc, true is used as value since it doesn't matter much.

* Since parser work on `.` terminated declaration (called `form`), can't use conditional compilation in middle of function definition.

```
-ifdef(DEBUG).
define(show(X), io:format("Value of X is: ~w.~n", [X])).
-else.
-define(show(X), ok).
-endif.

foo(A) ->
  ?show(A),
  ...
```

---

### 2.13 Processes

* Processes, messages and concept of process links & signals are introduced alongwith PIDs.

#### 2.13.1 Operating on processes

* There are few types of spawn functions. One takes a (nullary) fun as starting point for new process. Second type take a module name, a function name and a list of arguments.

```
%% returns fun of new process to empty list, else works like spawn/3
spawn(Fun) -> pid()

%% returns fun of new process (if Node is missing then a useless pid) to empty list, else works like spawn/3
spawn(Node, Fun) -> pid()

%% returns Pid of Module:Function to Args,
%% error_handler:undefined_function(Module, Function, Args) is evaluated if Fun missing
spawn(Module, Function, Args) -> pid()

%% returns Pid of new process of Fun
spawn(Node, Module, Function, Args) -> pid()
```

```
%% nullary fun
Pid = spawn(fun() -> do_something() end)

%% type with details
Pid = spawn(Module, Function, ListOfArgs)
```

* Second type requires named function exported from its module, initial data can only be passed through argument list. They always lookup latest version of module and generally better for starting a process on a remote machine which might have mismatching module version.

```
Pid = spawn(Node, Module, Function, ListOfArgs)
```

* `spawn_opt(Fun, Options) -> pid() | {pid(), reference()}` takes a list of additional options. Can be used to set a monitor, link, priority level, full sweep, heap size, message queue data.

```
Pid = spawn_opt(fun() -> do_something() end, [monitor]) %% new process is monitored, and both the pid and reference for the monitor are returned
```

* For `link` option there is an alias `Pid = spawn_link(...)`, it ensures link is created to new process as atomic operation. It prevents race conditions, can occur if you spawn process first and try link to it later using `link(Pid)`.

* Spawned process doesn't know of parent process until told somehow. Process can find its own pid using BIF `self()`. Following code spawns a child process that knows its parent. The call to self need be made outside `fun...end` otherwise it gets executed by new process.

```
Parent = self(),
%% myproc:init/1 is entry point for child process and take parent Pid
Pid = spawn(fun() -> myproc:init(Parent) end)
```

* An alternative to links, `monitors` are unidirectional link to allow a process to monitor another without affecting it.

```
%% if Pid dies, message containing unique Ref is sent to process that set up monitor
Ref = monitor(process, Pid)
```

* The `exit/1` BIF can be used to terminate running process. Unless caught `exit(Reason)`by process, it will terminate and pass on Reason.

* Sending an explicit exit signal to another process using `exit(Pid, Reason)`, don't have to be linked for this.
> This `exit/2` doesn't terminate sender, but receiver.

* Can trap exit singals, process can set its `trap_exit` flag. Incoming exit signals convert harmless messages, signal `kill` is untrappable.

```
process_flag(trap_exit, true)
```

#### 2.13.2 Receiving messages, selective receive

* `receive` expression can extract messages from mailbox queue. Messages are stored in order of arrival, but can be picked and skipped as required using pattern matching.

```
receive
  PatternX when GuardX_Y -> BodyX_X_Y; %% if message matchs a clause, gets removed from mailbox
  ...
  PatternY when GuardY_Y -> Body_Y_Y_Y %% each message passes these clauses in order and skipped if not matched
after Time -> %% optional segment with int Time or atom infinity, receiver carries on if omitted
  TimeoutExpression %% if no message found, this gets evaluated
end
```

#### 2.13.3 Registered processes

* Each erlang process has a local process registry, simple name service registering processes.

* Only one process can use a name at any time on each runtime system, can be used for singleton processes.

* Calling `registered()` in erl-shell gives following. Erlang runtime is like an OS itself.

```
Eshell V8.3.5.5  (abort with ^G)
1> registered().
[inet_db,erts_code_purger,init,error_logger,rex,user,
 kernel_sup,kernel_safe_sup,user_drv,global_name_server,
 global_group,code_server,standard_error,file_server_2,
 application_controller,standard_error_sup,erl_prim_loader]
```

* Can find Pid registered under a name using BIF `whereis(<name>).`.

* Can send message directly to process via registered name (or node+name for remote runtime).

```
%% below is a trick, sends message to self, this message will stop runtime
init ! {stop, stop}
```

* Can start process with `spawn(...)` and register it.

```
6> Pid = spawn(timer, sleep, [102030]).
<0.68.0>
7> register(foo, Pid).
true
8> whereis(foo). %% returns Pid of registered process
<0.68.0>
9> whereis(foo).
undefined %% after registered process is done
```

* To talk to a registered process `{other_node, other_registered_name} ! Message.`.

* If registered process dies and restarted, it will have a new Pid. Updating registry for this saves hassle of using it elsewhere.

#### 2.13.4 Delivery of messages and signals

* `!` operator used to pass messages is a special case of more general signal system. `exit` signal sent from dying process to its neighbors are other kind.

* There are few other not directly available to developer. As `link requests` are sent while linking to processes. Link is bi-directional, as in both sides must know of it.

* There are few basic delivery guarantees:
> * If process `P1` sends 2 signals to `P2` on any node, they will reach `P2` in same order as sent if none get dropped. It makes sure even `exit` signal reachs after any previous signal has reached target. Neither can a message overtake a signal.
>
> * Within same erlang runtime, there is no danger of losing messages. Between networked runtimes message may get dropped.

* For most part one doesn't have to worry of ordering and delivery of message.

#### 2.13.5 The process dictionary

* Each process maintains its own `process dictionary` in its state as a hash-table. BIFs `put(Key, Value)` and `get(Key)` are used to use it.

* There are following reasons to avoid using it:
> * Can no longer look at code and get whole story. Makes it harder to reason about programs.
>
> * It brings in process state part of process logic. Makes it hard to do inter-process handovers.
>
> * Force client to use single process for no context switch, if used from a library.

* ETS is better option.

---

### 2.14 ETS tables

* ETS is Erlang Term Storage. ETS data can be shared between processes.

#### 2.14.1 Why ETS tables work like they do

* Design philosophy is to have table that feel as separate process. They are efficient hashtables implemented in `C` as part of erlang runtime system; they are lightweight, fast and available as BIFs.

* Should avoid sharing data until all options/patterns are exhausted. Implement a storage based on normal semantics of process and message passing.

* ETS table works like a DB, isolated from everything else and holds info used by many. If someone tries updating position in table, the dat wouldn't be affected.

#### 2.14.2 Basics of using ETS tables

* ETS tables are created and manipulated via stdlib `ets` module.

* Use `etc:new(Name, Options)` to create a new table. The function returns a table identifier, can be used for table operations.
> `Name` must be an atom and `Options` must be a list.
>
> Unless `named_table` option is specified, name isn't of significance and can be reused any number of times.
>
> Name could be useful while debugging, so can use name of current module instead of random text.

```
Tabl = ets::new(this_table, []),
ets:insert(Tabl, {16, hey}),
ets:insert(Tabl, {17, bye}),
```

* ETS table only stores rows, tuples. Any other data need be wrapped into tuples.
> A tuple field need to be used as index, first by default.
>
> Can lookup element in table by index. `ets:lookup(Tabl, 17)` would return `[{17, bye}]`.

* Table doesn't need to be a `set` of rows with unique keys. It can be a `bag` with duplicate keys, but can't have duplicate rows. It can be a `duplicate bag` which can even have identical rows. In other cases, lookup returns multiple elements.

---

### 2.15 Recursion: it's how you loop

* Other than list comprehension, recursion gets used. Starting from a simple example adding numbers from 0 to N.

```
sum(0) -> 0;
sum(N) -> sum(N-1) + N.
```

#### 2.15.1 From iteration to recursion

* Erlang only uses recursion to create loops, implementation is efficient.

* In [fun sum/1](./chapter2_15_1.erl), the pattern match will help call first definition until 0 is passed as N, and then result in there is returned.

* In [fun do_sum/2](./chapter2_15_1.erl), when N gets passed as 0, defines when recursion is done and return a result. When first clause no longer matches, second clause gets used instead. Step 1 needs `Total` to be passed as 0.

```
do_sum(N) -> do_sum(N, 0).

do_sum(N, Total) when N =/= 0 -> do_sum(N-1, Total+N);
do_sum(0, Total) -> Total.
```

#### 2.15.2 Understanding tail recursion

* Recursive call can be tail-recursive (where all previous state can be let go) or non tail recursive. The `sum` call at start was non tail-recursion. The `do_sum/1` is a tail-recursive call.

* In `sum/1` even after last call there is still result of last recursive call left to be used for new addition and return. In `do_sum/1` no more work is needed, it's not waiting on anything except returning result of recursion.

* Erlang guarantees tail-call optimization. Compiler can generate code to throw away info on current call from stack before performing tail-call, so tail-call don't make stack grow.

#### 2.15.3 Accumulator parameters

* For `sum/1` will do half work computing and half making note on stack for later computing, until it's empty.

* For `do_sum/1` uses a single note on stack, keeps replacing it with newer info until done. Here `Total` is an example of `accumulator parameter` with purpose of accumulating info in a variable instead of keeping it in stack to return later.

#### 2.15.4 Some words on efficiency

* If result requires tracking essentialy same info non tail-recursive gets, then tail-recursive might have to do complex data management and turn out a bit slower.

* The function with no upper bound on looping, must be tail-recursive so it runs in constant space.

* Don't spend time on premature optimization and keep it readable. Picking between 2 is an exercise.

#### 2.15.5 Tips for writing recursive functions

> Say for a fun to create a reversed list, one will obviously need to visit all elements of original list.

* Prepare out examples, better to have examples to visualize a patter. Also helps to create few unit tests before beginning

```
[]           -> []
[x]          -> [x]
[x,y]        -> [y,x]
[x,y,z]      -> [z,y,x]
```

* Then write out base cases and desired outcome, couple of clauses required for this scenario.

```
rev([])  -> [];
rev([X]) -> [X].
```

* Look at construction of remaining cases. In this scenario `[A, B, ...]` can be written as `[A | [B | ...]]` which can form upper clause.

```
%% below will check for 'rev' to work for all cases as WIP
rev([A | [B | RestOfList]]) -> no_yet_implemented;
```

* Now let's implement the `no_yet_implemented` definition for desried result.

```
rev([A | [B | RestOfList]]) -> rev(RestOfList) ++ [B, A]; %% not tail-recursive
rev([])  -> [];
rev([X]) -> [X].
```

* For a complex logic flow, it is important to determine the recursion wouldn't be endless. A `monotonically decreasing arguments` can reason for termination. At least one of arguments (part of loop termination condition) should be getting smaller and none should be getting larger.
> For cases like `sum/1` we need a clause like `when N > 0` to ensure a negative number can't be passed as input causing an endless loop.

* Try to minimize base cases as much possible. As in `rev/1` the base case `rev([X])` can also be written as `rev([]) ++ [X]`, it can be joined to matching case.

```
rev([A | RestOfList]) -> rev(RestOfList) ++ [A]; %% not tail-recursive
rev([]) -> []. %% the order here doesn't matter as pattern matching differs empty from non-empty
```

* There is a chance of falling victim to quadratic time behavior. If input is a huge list, above would take much time as for every element it also calls append to new list (also splitting list for head and rest, though that time is minimal).

* Can't use `++` to avoid quadratic time. You can use same basic devision into base case and recursive case.

```
%% it is linear time
tailrev(List) -> tailrev(List, []).

tailrev([X | RestOfList], Acc) -> tailrev(RestOfList, [X | Acc]);
tailrev([], Acc) -> Acc.
```

* Can use BIF `length` in guards, as in following list. Don't use it to loop on list. A function like this is quadratic time.

```
%% do this pattern for checking non-empty list
loop([SomeElement | RestOfList]) ->
  do_something;
loop([]) ->
  done.

%% checking for lists for various lengths using pattern matching
%% need to check for longer list first, as it matches in order
loop([A, B | RestOfList]) -> two_or_more;
loop([A | RestOfList]) -> one_or_more;
loop([]) -> none.
```

---

### 2.16 Erlang programming resources

#### 2.16.1 Books

* `Programming Erlang - Software for a Concurrent World` by Joe Armstrong.

* `Erlang Programming` by Cesarini and Thompson.

#### 2.16.2 Online material

* [www.erlang.org](https://www.erlang.org), official site.

* [www.trapexit.org](https://www.trapexit.org), a community site.

* [www.planeterlang.org](https://www.planeterlang.org) for feeds.

---
---
