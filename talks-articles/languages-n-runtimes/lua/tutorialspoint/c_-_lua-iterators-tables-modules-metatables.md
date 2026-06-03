
## Lua Iterators

> code for: [iterators](./c_-_lua-iterators.lua), [tables](./c_-_lua-tables.lua),
>       [modules](./c_-_lua-modules.lua), [metatables](./c_-_lua-metatables.lua)


### Iterators

* Two types: Stateful and Stateless

* Lua has Generic and Numeric `for`. Generic `for` iterator `for k,v in pairs(tbl) do ... end`. Numeric `for` as `for i=1,10,2 do print(i) end`

* `ipairs` & `pairs` are highly used built-in iterators.. can use `for` with `next` function

```
for k,val in next, array do
   print(k, val)
end
```

* Can create custom iterators like `square` or `likePairs` in code sample.

* No built-in chain iterators.. I'd suggest use nested iterations on list of iterables instead of concocting a chain iterator to feed in complexity.


### Tables

* Are Lua's only collection datatypes; allow creating Arrays, Dictionaries, Sets, etc.

* When used as Arrays; default starts with 1. Can have negative indexes as start acting like Dictionaries, but iterable with `pairs` not `ipairs`.

* For not-found key, returns `nil`. So, behaves same when used like arrays.


### Modules

```
-- Assuming we have a module blahblah with a funtion bleh(arg)

-- FIRST STYLE
require "blahblah"
blahblah.bleh("baah")

-- SECOND STYLE
local blah = require "blahblah"
blah.bleh("baa")

-- ANOTHER STYLE
require "blahblah"
local blahbleh = blahblah.bleh
blahbleh("baabaa")
```

* Once Lua loads a module, its cached and further occurences use the loaded copy. Also allows share states easily. As done with [blahblah.lua](blahblah.lua) & [blahbaah.lua](blahbaah.lua)


### Metatables

* Metatables are underlying object implementation in Lua, defining custom behavior

```
-- metamethods in Lua 5.1
       __eq :     Equality      => a == b
       __le :     Less than     => a < b
       __ge :   Greater than    => a > b
      __add :     Addition      => a + b
      __sub :    Subtraction    => a - b
      __mul :   Multiplication  => a * b
      __div :     Division      => a / b
      __pow :   Exponentation   => a ^ b
      __unm :     Negation      => -a
      __mod :      Modulus      => a % b
   __concat :   Concatenation   => a .. b
__metatable :     Metatable     => getmetatable(a)
     __call :       Call        => table()
    __index :     Indexing      => table.x | table['x']
 __newindex :     New Index     => table.x = 'abc'
     __mode :  Weak References  => mt.__mode = 'k'
 __tostring : String Conversion => tostring(a)
```

* When accessing non-existent key in a table, Lua check metatable for `__index()` implementation. If `__index()` checks another table, and that another.. chaining is in works.

---
