
## Lua Functions, Strings, Arrays

> code for: [functions](./b_-_lua-functions.lua); [strings](./b_-_lua-strings.lua); [arrays](./b_-_lua-arrays.lua)

### Functions

```
_optional_scope_local function fn_name(arg1, arg2, ...)
    _function_body
    return _result_params_comma_separated
end
```

* example in [code](./b_-_lua-functions.lua) for:
> anonymous fn, alias of fn, variable length arguments, multiple return values,
> assigning multiple returns, fn calls as arguments,
>

* the multiple value returns can be varied in counts if conditional returns are there

* named arguments aren't available, achieved by instead passing associative `{k:v}` table

* optional args can be done, by checking if passed table as arguments has a key or not

* `local function ...` is namespace scoped as the local variables; Global by default


### Strings

```
strA = "THIS."
strB = 'This.'
strC = [[this.]]
```

* Escape sequences available:
> `\a Bell`, `\b Backspace`, `\f Formfeed`,
> `\n New line`, `\r Carriage return`, `\t Tab`, `\v Vertical tab`,
> `\\ Backslash`, `\" Double quotes`, `\' Single quotes`,
> `\[ Left square bracket`, `\]	Right square bracket`

* example in [code](./b_-_lua-strings.lua) for:
> declaration varities, string manipulations, concatenation, looping,
> to number, split with pattern, check if null


### Arrays

```
-- indexing starts at 1, not 0 like most other languages
array = {"Lua", "Tutorial"}
```

* example in [code](./b_-_lua-arrays.lua) for:
> using arrays, multi-dimensional arrays, slicing/sorting/merging arrays,
> different length given array is clean or mix of indexed items with key-vals,
> * implementation of Stack/Queue using `metatables` with built-in `rawget(..)` & `rawset(..)` that r/w to tables directly skipping table behaviors

* Metatables allow overriding array behaviors. Attached to data containing Metamethods that get called when certain action on attached-to datum happens.

---
