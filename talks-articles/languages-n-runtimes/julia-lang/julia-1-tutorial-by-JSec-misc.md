
## Julia 1.x Tutorial Misc/Level-Up

> by J-Sec, updated as per July/2019

### Package Manager

* create a new package at REPL by `generate NewPackageName` in `]` package mode

> * it will create a `Project.toml` file and a main lib module file under `src`; update `Project.toml` for correct project metadata
>
> * running `activate .` and `add NameOfPackageToUse` will create/update `Manifest.toml`
>
> * check [document-summarization.jl](./sample-code/document-summarization.jl) as sample which can be used in src

---

### FileSystem Handling

* cli like commands to manage file structure

```
pwd() == "/my/working/dir"

mkdir("/tmp/blah") ## creates dir, this usage doesn't imply '-p'

cd("/tmp/blah") ## change working dir

touch("myfile") ## creates/touch "myfile", absolute/relative paths shall work

readdir() == ["myfile"]

touch("myfile.log")

readdir() == ["myfile", "myfile.log"]

rm("myfile")

readdir() == ["myfile.log"]

mv("myfile.log", "myfile.md") ## src to dest file path

readdir() == ["myfile.md"]
```

* File I/O Writing

```
write("myfile.md", "## Julia") ## would overwrite per call

open("myfile.md", "w") do fyl ## to push in content using multiple flows
  write(fyl, "## Julia\n")
  write(fyl, "* File I/O\n")
  write(fyl, "read and write operations")
end
```

* File I/O Reading

```
fyl = open("myfile.md", "r") ## IOStream(<file myfile.md>)

readlines(fyl) == ["## Julia", "* File I/O", "read and write operations"]
readlines(fyl) == [] ## as stream is empty now

fyl = open("myfile.md", "r")
readline(fyl) == "## Julia"

readlines(open("myfile.md", "r")) == ["## Julia", "* File I/O", "read and write operations"]

open("myfile.md", "r") do fyl
  for line in eachline(fyl)
    println(line)
  end
end
#=
## Julia
* File I/O
read and write operations
=#
```

---

### Pickling

> same as Pickle in Python, saving/loading Julia data structures on disk/byte stream

* packages of use are `JLD`, `HDF5`, `PyCallJLD`; HDF5 is format primarily for scientific data here and PyCallJLD is for PyCall objects

* using `save..load`; but overwrites

```
julia> using JLD

julia> some_data = [["XY", 1, 'x'], [Dict("a" => 1, "b" => 2), 2, 'y']]

#=
 If file extension of save path is not 'jld', might get following error
 ERROR: FileIO.File{FileIO.DataFormat{:UNKNOWN}}("/tmp/some_data.jl") couldn't be recognized by FileIO.
=#
julia> save("/tmp/some_data.jld", "data", some_data)

julia> nu_data = load("/tmp/some_data.jld", "data")

julia> nu_data[1][1] == "XY"

## macros @save and @load are also available
```

* using `jldopen`, to write/read multiple entry points

```
jldopen("/tmp/some_data.jld", "w") do fyl
  write(fyl, "xdata", some_data)
  write(fyl, "ydata", other_data)
end

jldopen("/tmp/some_data.jld", "r") do fyl
  x_data = read(fyl, "xdata")
  y_data = write(fyl, "ydata", other_data)
end
```

---

### Map, Lambda Functions

* map(fn,collection)

```
map(round, [1.1,2.3,4.5,6.7]) == [1.0,2.0,4.0,6.0]

map([1.3,3.6,5.8]) do x
  2x^2 + 5x
end  == [9.88, 43.92, 96.28]
```

* Lambda

```
julia> plusOne(x) = x+1
plusOne (generic function with 1 method)

julia> plusOne(10)
11
```

* Lambda Functions and Map

```
map(plusOne, [1,3,5]) == [2,4,6]

map(x->x*2, [1,3,5]) == [2,6,10]
```

---

### Building CLI

* useful pacakges `ArgParse.jl`, `Fire.jl`, `DocOpt.jl` to easily crete cli

* `Fire.jl` expose all functions prefixed with `@main` macro and `--help` for them; doesn't allow one-line function

```
julia cli.jl --help

julia cli.jl <arg-for-single-exposed-func>

julia cli.jl <exposed-func-name> <arg-for-mentioned-exposed-func>
```

* check [basic-cli.jl](./sample-code/basic-cli.jl) as sample

---

### HTTP Requests

* using Package `HTTP`, HTTP client/serveri/websocket modules could be written

* HTTP client example

```
using HTTP
r = HTTP.request("GET", "http://httpbin.org/ip"; verbose=3)
r.status == 200
String(r.body) == "{\n  \"origin\": \"0.0.0.0, 0.0.0.0\"\n}\n"
```

---

### Date, Time and TimeZones

* using Package `Dates` (stdlib)  and `TimeZones` (pkg add)

```
julia> using Dates, TimeZones

julia> DateTime(2018,01,21)
2018-01-21T00:00:00

julia> DateTime(2018)
2018-01-01T00:00:00

julia> Date(2018)
2018-01-01

julia> Date(2018,12,21)
2018-12-21

julia> DateTime(2018,01,21,23,21)
2018-01-21T23:21:00

julia> xdate = DateTime(2018,01,21,23,21)
2018-01-21T23:21:00

julia> year(xdate)
2018

julia> Time(xdate)
23:21:00

julia> month(xdate)
1

julia> day(xdate)
21

julia> week(xdate)
3

julia> hour(xdate)
23

julia> minute(xdate)
21

julia> second(xdate)
0

julia> monthday(xdate)
(1, 21)

julia> monthname(xdate)
"January"

julia> dayname(xdate)
"Sunday"

julia> today()
2019-07-15

julia> time()
1.563204964069171e9

julia> unix2datetime(time())
2019-07-15T15:36:17.987

julia> now()
2019-07-15T21:06:47.256

julia> Time(now())
21:06:52.147

julia> dayname(now())
"Monday"
```

* format parsing

```
julia> df = DateFormat("yyyymm")
dateformat"yyyymm"

julia> Date("2018-12", df)
2018-12-01

julia> Date("2018-12-03", df)
ERROR: ArgumentError: Found extra characters at the end of date time string


julia> df = DateFormat("yyyy . mm")
dateformat"yyyy . mm"

julia> Date("2018 . 12", df)
2018-12-01
```

* adding and subtracting dates and timedeltas

```
julia> dayOfOccurence = DateTime(2010,10,23,14,21,32)
2010-10-23T14:21:32

julia> dayOfOccurence + Year(1)
2011-10-23T14:21:32

julia> dayOfOccurence + Month(10) ## similarly for Day/Week
2011-08-23T14:21:32
```

* using TimeZones

```
julia> timezone_names(); ### list of time zones

julia> TimeZone("Asia/Kolkata")
Asia/Kolkata (UTC+05:30)

julia> tz"Asia/Kabul"
Asia/Kabul (UTC+04:30)

julia> tz"Europe/Paris"
Europe/Paris (UTC+1/UTC+2)

julia> now(localzone())
2019-07-15T21:17:20.214+05:30

julia> now(TimeZone("Asia/Kolkata"))
2019-07-15T21:16:46.215+05:30

julia> now(tz"Europe/Paris")
2019-07-15T17:46:59.643+02:00

julia> varinfo(TimeZones); ## Julia 1.x's whos, to size eall available

julia> ZonedDateTime(2018, 02, 28, 18, 43, 32, tz"Europe/Paris")
2018-02-28T18:43:32+01:00
```

---

### Playground / Virtual Environment

* setup either by `Pkg.add("Playground")` or `ENV["PLAYGROUND_INSTALL"]=true;Pkg.build("Playground")`

* adding `~/.playground/bin` in `PATH`

* available by utility `playground`

```
playground list ## list available julia versions and playgrounds

playground create --name NewPlayground ## creating a playground

playground activate --name NewPlayground ## to enable existing playground

## can install Julia package to tryout in virtual environment

exit ## to deactivate the active Playground

playground rm --name NewPlayground ## to delete Playground
```

* it also allows o manage Julia versions, clean dead Julia-versions or Playground links; also exec a command in Playground and exit

#### at REPL

* activate by `using Playground`

```
env = Environment("NewPlayground")
create(env)
config = Config()
list(config)

activate(env;shell=false) ## to only activate in REPL
##activate(config, "OldPlayground"; shell=false) ## for pre-existing env

Pkg.init()

deactivate()
exit()
```

---

### LaTex

> packages `Latexify` and `LaTeXStrings`

* in built support for greek alphabets and other symbols; like `\alpha`, `\theta`, `\infty`, `\pi`, `\sum`, `\int`, `\sigma` with `<TAB>`

* convert to latex format using `latexify(expr)`, `L"expr"`; `latexraw(expr)` to get raw LaTeX format; eg. `latexify(:(1/2))`, `L"$\frac{2}{4x}$"`

---
---
