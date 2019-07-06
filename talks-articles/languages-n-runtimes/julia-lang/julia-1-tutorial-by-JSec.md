
## Julia 1.0 Tutorial

> by [J-Sec](https://www.youtube.com/playlist?list=PLJ39kWiJXSiyegKeVd-praE_jOv6sgCn5)

* `?<func-name>` to get help at console

---

### Comments

```
# single line comment

#=
multi-line
comments
=#
```

---

### Variables

```
some_char = 'J'
some_str = "Julia by MIT"
_some_int = 100

## unicode chars also allowed as var; available in IDE like `\alpha` `\pi` `\:hearts:`
π = some_str
♥ = 100
x^3 = x³ ### achieved by x\^2

typeof(some_char) ## Char
```

---

### Output

```
println(some_str) ## some_str with new-line
print(some_str)   ## some_str with new-line
show(some_str)    ## some_str in it's representational mode "\"Julia by MIT\""
```

---

### Input

```
some_var = readline(stdin)
some_var = readline() ## default takes from stdin as well
```

---

### Numbers and Arithmetic

* normal number and arithmetic sample

```
## follows classic BEDMAS grouping

x, y = 100, 5
x + y == 105
x - y == 95
x * y == 500
x / y == 20
x \ y == 0.05
x ^ y == 10000000000

+(x, y) == 105
-(x, y) == 95

div(21, 4) == 5
fld(21, 4) == 5 ## explicit floor div
cld(21, 4) == 6 ## explicit ceil div

rem(21, 4) == 1
mod(21, 4) == 1
21 % 4     == 1

lcm(16, 12)  == 48 ## least common multiple
gcd(16, 12)  == 4  ## greatest common divisor

sqrt(16) == 4.0
cbrt(8)  == 2.0

log(100)      == 4.605170185988092
log10(100)    == 2.0
log(10, 100)  == 2.0 ## explicit base as first param

hypot(x, y)   == 100.12492197250393

some_float = 3.21
prevfloat(some_float) == 3.2099999999999995
nextfloat(some_float) == 3.2100000000000004
```

* rational numbers

```
some_rational = 2//5
denominator(some_rational) == 5
numerator(some_rational)   == 2
```

* vectorized dot operator

```
[1,10,11,100] .+ 5 == [6, 15, 16, 105]
[1,10,11,100] .^ 5 == [1, 100000, 161051, 10000000000]

```

---

### Strings

```
_char = 'C'
_strx  = "S"

_strx[1] == 'S' ## indexing starts from 1
```

* indexing

```
_str = "Julia from MIT"
_str[1]     == 'J'
_str[1:end] == "Julia from MIT"
_str[1:end-1] == "Julia from MI"
_str[2:end-1] == "ulia from MI"

_str[1:-1]  == ""
_str[1:0]   == ""
_str[end:2] == ""

_str[end:-1:2] == "TIM morf ailu"
_str[end:-1:1] == "TIM morf ailuJ"
reverse(_str)  == "TIM morf ailuJ"

_str[end:-1:end]   == "T"
_str[end:-1:end-2] == "TIM"
```

* concatenation

```
string(_strx, ": ", _str)  == "S: Julia from MIT"
_strx * ": " * _str        == "S: Julia from MIT"
```

* interpolation

```
"[+] $_strx: $_str" == "[+] S: Julia from MIT"

"2 ^ 4 == $(2 ^ 4)" == "2 ^ 4 == 16"
```

* manipulation

```
uppercase(_str)  == "JULIA FROM MIT"
lowercase(_str)  == "julia from mit"
titlecase(_str)  == "Julia From Mit"

length(_str) == 14

split(_str, " ") == ["Julia", "from", "MIT"]

repeat("*-", 10) == "*-*-*-*-*-*-*-*-*-*-"

occursin("Jul", _str) == true  ## not contains/search, they are from older version

SubString(_str, 1,3)  == "Jul"


join(["A","B","C"], "->-") == "A->-B->-C"
join("ABC", "->-")         == "A->-B->-C"

strip(" ABC ") == "ABC"
strip(" ABC")  == "ABC"

strip(" ABC :::")       == "ABC :::"
strip(" ABC :::", ':')  == " ABC "
```

* multiline standard and raw string

```
julia> myname = "Julia"
"Julia"

julia> """This
       is
       $myname"""
"This\nis\nJulia"

julia> raw"""This
       is
       $myname"""
"This\nis\n\$myname"
```

---

### Tuples and Named Tuples

* tuples

```
typeof((1))  == Int64
typeof((1,)) == Tuple{Int64}


some_tplx = ()
typeof(some_tplx) == Tuple{}

some_tplx = (1,3,5,7,9)
typeof(some_tplx) = NTuple{5,Int64}
some_tplx[1] == 1
some_tplx[3] == 5

## some_tplx[2] = 20 ## gives error, no method for setindex on NTuple
```

* named tuple, like a key and val

```
some_ntpl = (a=10, b=11, c=100)
typeof(some_ntpl) == NamedTuple{(:a, :b, :c),Tuple{Int64,Int64,Int64}}

some_ntpl = (a=10, b="B", c=100)
typeof(some_ntpl) == NamedTuple{(:a, :b, :c),Tuple{Int64,String,Int64}}

some_ntpl[1]  == 10
some_ntpl.b   == "B"
some_ntpl[:c] == 100

getindex(some_ntpl, 1)    == 10
getindex(some_ntpl, :c)   == 100

keys(some_ntpl)     == (:a, :b, :c)
values(some_ntpl)   == (10, "B", 100)

collect(some_ntpl)   == [10, "B", 100] ## returns array

collect(pairs(some_ntpl)) == [Pair(:a, 10), Pair(:b, "B"), Pair(:c, 100)] ## for list of pairs
```

* getting more info

```
julia> dump(some_ntpl)
NamedTuple{(:a, :b, :c),Tuple{Int64,String,Int64}}
  a: Int64 10
  b: String "B"
  c: Int64 100

julia> fieldnames(typeof(some_ntpl))
(:a, :b, :c)
```

---

### Arrays

* basics

```
## [], Int64[], Float64[]
## as noted before, indexing starts from 1 not 0; last element is inferred by `end`

int_arr = Int64[1,3,4]  ## specific typed array; 3-element Array{Int64,1}

arr = ["name", 100]     ## flexible list for Any type; 2-element Array{Any,1}

eltype(int_arr) == Int64 ## element type
ndims(int_arr)  == 1     ## dimension
size(int_arr)   == (3,)  ## tuple of dimensional length
length(int_arr) == 3     ## total element count
typeof(int_arr) == Array{Int64,1}
```

* indexing

```
int_arr[1] == 1
int_arr[end] == 4
int_arr[2:end] == [3,4] ## slicing arrays
```

* creating from range

```
range(1, stop=10) == 1:10

collect(1:10)     == [1,2,3,4,5,6,7,8,9,10]

collect(range(1,stop=10)) == collect(1:10)

range(1, step=2, stop=10) == 1:2:9
collect(1:2:9) == [1,3,5,7,9]
collect(0:2:10) == [1,3,5,7,9]
```

* mix and match

```
odd = collect(1:2:9)
even = collect(0:2:10)
union(odd, even) == [1,3,5,7,9,0,2,4,6,8,10]
union(odd, even, [22]) == [1,3,5,7,9,0,2,4,6,8,10,22]

intersect([1,3,5], [3,6,9]) == [3]

setdiff([1,3,5], [3,6,9])   == [1,5]
```

* manipulation

```
push!(int_arr, 10) ## int_arr == [1,3,4,10]
pushfirst!(int_arr, 0) ## int_arr == [0,1,3,4,10]

pop!(int_arr) == 10
popfirst!(int_arr) == 0
int_arr == [1,3,4]

append!(int_arr, [0,5])  ## int_arr == [1,3,4,0,5]


sort(int_arr) == [0,1,3,4,5]  ## int_arr == [1,3,4,0,5]
sort!(int_arr) == [0,1,3,4,5] ## int_arr == [0,1,3,4,5]
```

* getting boundaries

```
extrema(int_arr) == (0, 5)
maximum(int_arr) == 5
minimum(int_arr) == 0
```

---

### Functions

* basic definition

```
function myname(param)
  ## explicit `return blah` used for split flow
  ## last expression gets returned by default
  param ## whatever operation
end

x = myname(10) ## x == 10
```

* one liner function definition

```
add3num(a,b,c) = a + b + c  ## add3num (generic function with 1 method)

add3num(2,4,6) == 12
```

* define function with fixed typed input

```
function pow2(x::Int64)
  x * x
end

pow2(10) == 100
## pow2("A") ## would give ERROR: MethodError: no method matching pow2(::String)
## pow2(10.0) ## even this ERROR: MethodError: no method matching pow2(::Float64)

function pow2x(x::Number) ## can define with higher type
  x * x
end
pow2x(2)    == 4
pow2x(2.0)  == 4.0
```

* checking available variations of method available

```
## if pow2(::Float64) was also defined to allow pow2 support just Int64 and Float64 explicitly
julia> methods(pow2)
# 2 methods for generic function "pow2":
[1] pow2(x::Int64) in Main at REPL[197]:2
[2] pow2(x::Float64) in Main at REPL[210]:2
```

* default arg, varargs, kwargs

```
## default args
fx(x,y=10) = x + y
fx(1)   == 11
fx(1,2) == 3

## varargs
fy(args...) = length(args)
fy(2,4,5) == 3

fz(pow, args...) = args .^ pow
fz(2, 1,3,5) == (1, 9, 25)

## kwargs
volum(;length,width,height) = length * width * height
volum(length=2, height=3, width=2) == 12
volum(length=2, width=2, height=3) == 12

## kwargs with default values
volume(;length=1,width=1,height=1) = length * width * height
volume(length=2)            == 2
volume(length=2, height=3)  == 6
```

* map a function over a list

```
fx.([1,2,3,4]) == [11,12,13,14]

fx.([1,2,3,4], [1,-1,3,1]) == [2,1,6,5]  ## passing list of multiple params
```

---

### Loops

#### For

* basics

```
for i in iterable
  doSomething(i)
end

for i in 1:10   ## 1:10 replaceable by range(1,stop=10)
  print("$i,")
end             ## output: 1,2,3,4,5,6,7,8,9,10,

for i = 1:10
  print("$i,")
end             ## output: 1,2,3,4,5,6,7,8,9,10,
```

* scopes

```
counts = 0

## ## below use of 'counts' gives error 'ERROR: UndefVarError: counts not defined'
## for i in 1:5
##   print("$i:$(counts+=1),")
## end

for i in 1:5
  print("$i:$(global counts+=1),") ## need to use global if in that context
end ## 1:1,2:2,3:3,4:4,5:5,

## although when in a function, it's available
function xfor()
  counts = 0
  for i = 1:5
    print("$i:$(counts+=1),")
  end
end
xfor() ## 1:1,2:2,3:3,4:4,5:5,

## whole trying global for counts in function will give following error
## ERROR: syntax: `global counts`: counts is a local variable in its enclosing scope
```

* `let`..`end`; to ensure scoping isn't a problem in or our of function

```
let counts = 0
  for i in 1:5  print("$i:$(counts+=1),")  end
end ## 1:1,2:2,3:3,4:4,5:5,
```

* `zip` arrays for same loop

```
zip([1,2,3],["a","b","c"]) ## Base.Iterators.Zip2{Array{Int64,1},Array{String,1}}([1, 2, 3], ["a", "b", "c"])

for (k,v) in zip([1,2,3],["a","b","c"])
  print("$k:$v,")
end ## output: 1:a,2:b,3:c,
```

#### While

* basics

```
while condition_is_true
  do_something()
end

let idx = 1
  while idx <= 5
    print("$i,")
    idx += 1
  end
end ## output: 1,2,3,4,5,
```

* conditional `break` and `continue` are available as well

---

### Conditonals

#### If..Else

```
if condition_is_true
  do_something()
else
  do_something_else()
end

myname = "Julia"
if myname == "Julia"
  println("Hello Julia")
elseif yname == "julia"
  println("Baby Julia")
else
  println("Where is Julia")
end ## output: Hello Julia
```

#### Ternary

```
## condition ? do_something_if_true() : do_something_else_if_false()
greeting = myname == "Julia" ? "Hello Julia" : "Where is Julia?"
```

---

### Dictionary

* creating dictionary

```
some_dict = Dict("A" => 1, "B" => 2)  ## Dict{String,Int64} with 2 entries: "B" => 2 "A" => 1

zip_str_num = zip(["a", "b"], [11,12]) ## (["a", "b"], [11, 12])
other_dict = Dict(zip_str_num) ## Dict{String,Int64} with 2 entries: "b" => 12 "a" => 11
```

* dictionary operations

```
keys(some_dict) ## ["B", "A"]

values(some_dict) ## [2,1]

some_dict["A"] == 1

getindex(some_dict, "A") == 1
getindex(some_dict, "B") == 2

some_dict["A"] = 10
some_dict == Dict("B" => 2, "A" => 10)


in("B"=>2, some_dict) == true
in("B"=>3, some_dict) == false

haskey(some_dict, "A") == true
haskey(some_dict, "a") == false

merge(some_dict, other_dict) ==  Dict("A" => 10, "B" => 2, "a" => 11, "b" => 12)
some_dict ==  Dict("A" => 10, "B" => 2) ## no change to either dict

merge!(some_dict, other_dict) ==  Dict("A" => 10, "B" => 2, "a" => 11, "b" => 12)
some_dict == Dict("A" => 10, "B" => 2, "a" => 11, "b" => 12) ## as for `!` commands

get(some_dict, "A", 0) == 10
get(some_dict, "C", 0) == 0 ## default gets returned in case of missing key
## get! returns default and also sets it in dict for later use
```

---

### Sets

* basics

```
#=
* order ain't important
=#

some_set = Set([11,13,15,16,11])
some_set == Set([13, 16, 11, 15])

primes = Set([2,3,5,7,11,13])  ## Set([7, 13, 2, 3, 11, 5])
odds = Set(collect(1:2:15))    ## Set([7, 9, 13, 3, 11, 5, 15, 1])
evens = Set(collect(2:2:14))   ## Set([4, 14, 10, 2, 8, 6, 12])
```

* common set operations

```
union(odds, evens) == Set([2, 11, 7, 9, 10, 8, 6, 4, 3, 5, 13, 14, 15, 12, 1])
union(odds, primes) == Set([7, 9, 13, 2, 3, 11, 5, 15, 1])
## union!(..) is also available

intersect(primes, odds) == Set([7, 13, 3, 11, 5])
## intersect! is also available

setdiff(primes, odds) == Set([2])
setdiff(odds, primes) == Set([9, 15, 1])
## setdiff! is also available

xset = Set([1])
issubset(odds, xset) == false
issubset(xset, odds) == true
## ⊂ is also available as \subset
```

---

### Structs and Mutable Structs

* in `v1+` use mutable struct to create custom types; for immutable data-type use struct

* basics

```
mutable struct NewType
  age
end

struct NewImmutableType
  name::String
end
```

* sample immutable type

```
struct XStruct
  a::Vector{Float64}
  b
end

xstruct1 = XStruct([11,14,18], "Hey")

typeof(xstruct1) == XStruct

xstruct1.a == [11.0, 14.0, 18.0]

xstruct1.b == "Hey"

fieldnames(typeof(xstruct1)) == (:a, :b)

## xstruct1.b = 12 ## would give ERROR: type XStruct is immutable
## but can update based on data-type, like push to vector
append!(xstruct1.a, 12)
xstruct1 == XStruct([11.0,14.0,18.0,12.0], "Hey")
```

* mutable type

```
mutable struct YStruct
  a::Vector{Float64}
  b
end
ystruct1 = YStruct([11,14,18], "Hey")
ystruct1.b = 12 ## ystruct1 = YStruct([11,14,18], 12)
```

* while debugging can dump any instance

```
julia> dump(ystruct1)
YStruct
  a: Array{Float64}((3,)) [11.0, 14.0, 18.0]
  b: Int64 12
```

---
---
