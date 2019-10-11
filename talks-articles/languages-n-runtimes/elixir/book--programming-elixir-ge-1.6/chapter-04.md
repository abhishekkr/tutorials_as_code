
## Chapter.4 Elixir Basics

### Built-in Types

* are Value Types, System Types and Collection Types

* Strings and Structures are built using basic types; they are available built-in but are not basic types


### Value Types

* Arbitrary-sized Integer literals as decimal (`1231`), hexadecimal (`0xcafe`), octal (`0o753`), binary (`0b1110`)

> also decimal with underscores like `1_000_000` or `10_00_000`

* Floating-point Numbers with decimal point should have at least one digit at non-fraction as `1.2` or `0.12`; optional trailing exponent as `0.314159e1` or `314159.0e-5`

> floats are IEEE754 double pricision, 16 digit accuracy and max 10^308 exponent

* Atoms are constants like Ruby Symbols where its name is its value; prefixed with colon, sequence of UTF-8 letters including combining marks, digits, underscores and `@`; may end with `!` or `?`

> valid atoms as `:person`, `:is_person?`, `:person@1`, `:<>`, `:===`, `:"func/3"`, `:"new person"`

* Ranges as `start..end` with values of Integer, example `1..10`

* RegExp written as `~r{regexp}` or `~r{regexp}opts`

> any non-alphanumeric character could be used as delimited, not just `{ }`, PCRE (like Perl5) regexp patterns are supported
>
> opts: `f` force to match on first line of multi-line; `i` case insensitive; `s` allow `.` match any newline; `U` non-greedy `*` and `+`; more


### System Types

> these reflect resources in underlying Erlang VM

* `PID` reference to local or remote process, PID of current process is `self`, new PID get assigned to new spawn-ed process

* `port` is a reference to resource (typically external to app) to read/write

* `make_ref` function creates globally unique reference


### Collection Types

* Tuple is ordered collection, written as `{1, 2}` or `{:ok, "id", 12321}`, they are closest to arrays in other popular languages

> a common pattern is returning `{:ok, ...data}` or `{:error, ...data}`, where `:ok` or `:error` matched could determine flow

* List are written as `[1,2,3]` and effectively a linked data structure (as in either empty or with `head|tail`)

> * lists are easy linear traversal, expensive on random order; always cheap to get the head of a list and extract tail
>
> * since all data struct is immutable if we wanna remove the head the var just returns pointer of tail

```
iex> [:a, 1] ++ [:b, 2]
[:a, 1, :b, 2]
iex> [:a, 1, :b] ++ [:b, 2]
[:a, 1, :b, :b, 2]
iex> [:a, 1, :b] -- [:b, 2]
[:a, 1]
iex> 2 in [:b, 2]
true
```

* Keyword Lists lets you represent `[{:fname, "John"}, {:lname, "Doe"}]` as `[fname: "John", lname: "Doe"]`; allows keys to be repeated `[a: 1, a: 2]`

> can also skip square brackets if KewordList is last element in a structure as `{1, fname: "John", lname: "Doe"}` for `{1, [fname: "John", lname: "Doe"]}`


### Maps

* written like `%{ "fname" => "john", "lname" => "doe", :code => 10 }`, if key is atom can also write as `%{"a": 1, b: 2}`

> can also use expressions as keys; `{String.downcase("Name"): "John"}`

* under `some_map = {"fname" => "john", {:a, :b} => 123}, code: 1`; `some_map["fname"]` gives `"john"` and `some_map[{:a, :b}]` gives `123` and with atoms can also have `some_map.code`


### Binaries

> when need access to data as sequence of bits and bytes

* binary literals are enclosed between `<< .. >>`; following packs successive integers into bytes `bin = << 1, 2 >> ## byte_size(bin) == 2`

* can add modifiers to control size/type of individual field, as `bin = <<3 :: size(2), 5 :: size(4), 1 :: size(2)>> ## 11010101` holds 2, 4, 2 bits in a byte

* used by elixir to represent UTF strings


### Dates and Times

* `Date` type holds year, month, day and reference to ruling calendar, `~D[]` is sigil here

```
iex> {:ok, d} = Date.new(2019,10,05)
{:ok, ~D[2019-10-05]}
iex> Date.day_of_week(d)
6
iex> dx = Date.add(d, 31)
~D[2019-11-05]
iex> Date.range(d, dx)
#DateRange<~D[2019-10-05], ~D[2019-11-05]>
iex> Enum.count(Date.range(d, dx))
32
```

* `Time` type holds `hour`, `minute`, `second` and fractions of second as tuple containing microseconds and significant digits; `~T[]` is sigil

```
iex> {:ok, t} = Time.new(12, 34, 56)
{:ok, ~T[12:34:56]}
iex> ~T[12:34:56] == t
true
iex> ~T[12:34:56.00] == t
false
iex> ~T[12:34:56.00] == ~T[12:34:56.0]
false

iex> Time.add(t, 1800)
~T[13:04:56.000000]
iex> Time.add(t, 1800, :millisecond)
~T[12:34:57.800000]
iex> Time.add(t, 0)
~T[12:34:56.000000]
iex> Time.add(t, 0) == t
false
```

* `NaiveDateTime` with sigil `~N[]` contains date and time. `DateTime` adds ability to associate date and time to a TimeZone.

* Can augment these with 3rd party libraries, like Lau Taarnskov's Calendar library.


### Names, Source Files, Conventions, Operators and more

* identifiers may start with UTF-8 letter or underscore, followed by letter/underscore or UTF-8 decimal-digit; allowed to end with `!` or `?`

* `module`, `record`, `protocol`, `behavior` names shall start with uppercase letter following CamelCase; all other shall be snake-case starting with lowercase

* unused identifiers starting with underscore are not reported in warning

* 3 special value for Bool operations: `true`, `false`, `nil`; nil is treated false in Bool contexts; all these 3 alias atoms of same name

* comparison operators: strict (in)equality `===` and `!==`; value (in) equality `==` and `!=`; others `>`, `>=`, `<` and `<=`

* comparison for non-compatible type uses ordering rule `number < atom < reference < function < port < pid < tuple < map < list < binary`

* boolean operators: `or`, `and`, `not`, `||`, `&&`, `!`

* arithmetic operators: `+`, `-`, `*`, `div`, `rem`

* join operators: `<>` for binaries so also string; `++` and `--` for list

* `in` operator as `a in enum` for list, range or a map


### Variable Scope

* code sample at [chapter-04.exs](./chapter-04.exs) as `Shout.users` and `Shout.users_handled`

* having a variable updated within a `do-block`, initialized outside and accessed later is considered risky

* `with` epression define local scope for var if don't want some variable to leak out, also gives control over pattern-matching failures as

```
lp = with {:ok, file}     = File.open("/etc/passwd"),
          content         = IO.read(file, :all)
          :ok             = File.close(file),
          [_, uid, gid]   = Regex.run(~r/^lp:.*?:(\d+):(\d+)/m, content)
     do
       "Group: #{gid}, User: #{uid}"
     end
```

* `MatchError` will get raised in above non pattern-matched with clauses; to handle gracefully can use `<-` which return unmatched value instead

```
lp = with {:ok, file}     = File.open("/etc/passwd"),
          content         = IO.read(file, :all)
          :ok             = File.close(file),
          [_, uid, gid]   <- Regex.run(~r/^xlp:.*?:(\d+):(\d+)/m, content)
     do
       "Group: #{gid}, User: #{uid}"
     end
```

* `with` gets treated as function/macro call, so first clause need to be either on same line as `with` or `with(...)` notation need to be used; covered as more definitions

* `with..do..else` clause example is also covered in above mentioned code sample

---
