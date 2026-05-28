
## Lua Basics

### Overview

* written in C as in-house project in 1993 by Roberto, Luiz and Waldemar

* was desgined to be integrable with conventional languages

* providing good distance from hardware with dynamic structures, no redundancies, easy testing and debugging

* has safe env, automated memory management and good facility to handle data with dynamic sizes

* is extensible, simple, efficient, portable nad FOSS

* example `print("Hey!")`

* contains 2 parts
> * interpreter written in ANSI C, giving it portanility and strength
>
> * and functioning software system that can interpret programs written in lua

---

### Basic Syntax

* can start a REPL using `lua -i` for interactive mode

* can save program in a file (conventionally with extension `.lua`) and run as `lua ${file}.lua`


**Tokens**

* token is either a `keyword`, `identifier`, `constant`, `string literal`, or `symbol`

* following has 3 tokens

```
io.write("Hello from ", _VERSION, "!\n")
```


**Comments**

* start with `--[[` and end with `--]]`, as

```
--[[ this
     wouldn't
     be
     run --]]

-- neither would this single line comment
```


**Identifiers**

* identifier for a variable, function or user-defined item

* exceptable identifier examples

```
abc ABC _abc A01bc a_bc abc_
```


**Keywords**

* `and`, `or`, `break`, `do`, `end`, `in`

* `if`, `then`, `else`, `elseif`, `false`, `for`, `repeat`, `until`, `while`

* `local`, `function`, `return`

* `true`, `false`, `nil`, `not`


**Variables**

* all variables are GLOBAL, unless explicitly declared as `local varName` for a scope

* table fields are special type that can hold anything except nil; including functions

```
local a = 1             -- local a assigned 1
b, c = 2, 3             -- global b=2, global c=3
d, e = 4                -- global d=4, global e=nil

b, c = c, b             -- can swap easily
```


**Data Types**

* `nil`, `boolean`, `number` for double precision floats, `string`,
* `function` for C/Lua methods, `userdata` for arbitrary C data,
* `thread` for coroutines, `table` for arrays/symbol-tables/sets/records/graphs/trees/anything except nil

```
print(type("What is my type"))          -- string
```


**Operator**

* arithmetic: `+ - * / % ^`; relational: `== ~= > < >= <=` with `~=` for not equal
* logical: `and or not`; concat strings: `..` like `a..b`; unary op to get string/table length `#` as `#"this"`


**Loops**

* nested loops are fine; has `break` but not `continue` in standard lua

```
while(true)
do
    callFn()
    break
end

for idx = initCount, maxOrMinValue, incrementStep
do
   callFn(idx)
end

numbers = {10, 20, 30, 40, 50}
for i = 1,#numbers,1 
do 
   print(numbers[i]) 
end

idx = 0
repeat
   print(numbers[idx]) 
   idx = idx + 1
until idx > #numbers

-- with lua 5.2+ can imitate continue with goto
for idx = 1, 5 do
    if idx == 3 then
        goto continue
    end
    print(idx)
    
    ::continue::
end
```


**Generic: For**

* `ipairs()` returns index-value pairs used mostly for numeric tables, non-numeric keys are ignored and gaps in-between indices leads to halt.. in-short better for array like tables

* `pairs()` returns index-value pairs and suits associative tables.. all keys are preserved but order is unspecified

```
numeros = {10, 30, 20, 60, 30, 90, 40, 120}

for i, v in ipairs(numeros) do
   print(i, v)
end

for _, v in ipairs(numeros) do
   print(v)
end

days = { ["ichi"]="One", ["ni"]="Two", ["san"]="Three" }
for i, v in pairs(days) do
   print(i, v)
end
```


**Decision Making**

* only boolean `false` and `nil` is FALSE; thus `0` zero eval as `true`

```
if (true) then
    callFnA()
end

if (not(true)) then
    callFnA()
else
    callFnB()
end

if (true) then
    callFnA()
elseif (true) then
    callFnB()
end
```


**Date and Time**

* `os.date()` get formatted current date; `os.time()` get timestamp as per args
* `os.clock()` provides CPU clock time; `os.difftime()` to get diff between two timestamps

```
currentDate = os.date()

formattedDate = os.date("%d-%m-%Y")

time1 = os.time({year = 2026, month = 5, day = 28, hour = 13, min = 23})
time2 = os.time({year = 2026, month = 5, day = 28, hour = 13, min = 24})
diff = os.difftime(time2, time1)

startTime = os.clock()
wasteSomeTimeFn()       -- a time-consuming task
endTime = os.clock()
print("fn time:", endTime - startTime, "sec")
```

---
