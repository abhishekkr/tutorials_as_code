
## Chapter.22 Macros & Code Evaluation

> never use a Macro when you could use a Function
>
> code samples at [chapter-22.exs](./chapter-22.exs) & [chapter-22-exercise.exs](./chapter-22-exercise.exs)

### Implementing an `if` statement

* if we try implementing `if` using `case` as in `DontUse.xif/2`; it works for values but not blocks with calls as for `IO.puts` example

> also in `IO.puts` example, actual call made is `DontUse.xif 1 == 1, do: :ok, else: :ok` due to eval of `IO.puts` beforehand


### Macros inject code

* to tell compiler what to manipulate `defmacro`, `quote` & `unquote` are available

* when we pass a param to macro, Elixir doesn't evaluate them instead pass them as tuples representing their code

> * simple example at `MyMacro.one` printing out all it gets
>
> * this shows all things like `do: else:` blocks gets represeted as 3 element tuple

#### Load Order

* put Macro definition in one module and Call to them in other

* Macros are expanded before program executes; macro defined in one Module must be available before one uses it gets compiled

* `require` tells Elixir to keep Macro defining module ready; Elixir first compiles source files then runs them

* in a normal `*.ex` file if we have a Module and Source using it in same file; Elixir doesn't know where to load Module from yet and hence fails

#### The quote function

* parameters passed to Macros ain't evaluated

* `quote` forces code block given to it remain in unevaluated form

* `quote` makes parser interpret the following block and return internal representation of it


### Using the Representation as Code

* this unchained internal representation need to be injected back into CODE's internal representation

* 2 ways; first is Macro

> Elixir doesn't return this representation to invoking code; instead inject the code back into internal representation of program and returns to caller resulting execution of that code. Execution takes place only if needed.
>
> This different is made clear by difference in output of `MyMacro.two` & `MyMacro.three`.
>
> At `MyMacro.three` use of `quote` generates internal form; running code fragment from `quote` not passed param.

#### The `unquote` Function

* as in `MyMacro.four` this gets used inside a `quote` only to access param passed to Macro within `quote` block; otherwise `undefined` error occurs

* `unquote` block doesnt run when `quote` block is parsed but when code generated by `quote` 

#### Expanding a List: `unquote_splicing`

* with `unquote` list is inserted as is

```
iex(1)> Code.eval_quoted(quote do: [1,11,unquote([2,22])])
{[1, 11, [2, 22]], []}
```

* with `unquote_splicing` just the elements of list gets inserted

```
iex(2)> Code.eval_quoted(quote do: [1,11,unquote_splicing([2,22])])
{[1, 11, 2, 22], []}
iex(3)> Code.eval_quoted(quote do: [?a,?l,?o,unquote_splicing([?h,?a])])  
{'aloha', []}
```

#### Back to `if` implementation

* module `UseThis.iff` has usable macro

> `UseMacrosAndCodeEval1` & `UseMacrosAndCodeEval2` implementing exercises


### Using Bindings to Inject Values

* bindings are the 2nd way to inject code in `quote`

* its a simple keyword-list of variable-values passed as binding to `quote` get set inside

* commented part in `MacroBinding` & `UseMacroBinding` gives `** (CompileError) chapter-22.exs:129: invalid syntax in def x1()` as Enum haven't executed when Macro is called; so Bindings come to aid via `bind_quoted` passed keywords


### Macros and Hygienic

* check module `Scopex` result

> * if macro body just gets substituted at point of call; sharing same scope; Macros wouldn't clobber each other's variables
>
> * `import` & `alias` are locally scoped


### Other ways to run Code Fragments

> check usage in module `Scopey`

* can use `Code.eval_quoted` to evaluate code fragments similar to those returned by `quote`

* `quote` fragment is hygienic, so no access to variables outside its scope

* using `var!(:name)`, we can disable this feature & allow a quoted block to access variables in containing scope; here pass binding to `eval_quoted`


### Macros and Operators

* can override unary/binary operators using Macros; first need to remove it's definition from original module

* like to override `+` operator from `Kernel` module need something similar to `Strlab` module

* if we don't do `import Kernel, exclude: [+: 2]`, it will error out with ambiguous call for `+`


### Digging Deeper

* checkout `Kernel` module source for macros of operators, `def`, `defmodule` and similar


### Digging Ridiculously Deep

* for 3-element tuple internal representation; 1st is function/macro, 2nd is metadata list & 3rd is arg list

> this 3-element tuple gives feel of homoiconic language; giving ability to rewrite existing function; create new code on the fly

* this representation can be evaluated using `eval_quoted`

> Exercise.3 Math Expression Explain via Macro is under `MacrosAndCodeEval3` module

---