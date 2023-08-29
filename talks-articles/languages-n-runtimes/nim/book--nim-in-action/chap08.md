
## Chapter.08 Interfacing with Other Languages

> will cover calling procedures from C, wrapping external SDL; Nim has C, C++, Objective C & JS as compilation backends

### FFI

* allows to interface with C/C++/ObjC in same app; but not C & JS in same

#### Wrapping C Procedures

* we'll wrap C's `printf` decalred as `int printf(const char * format, ...);`; this prototype is important while wrapping

> * here `const char *` representa a string in Nim as `cstring`; `int` as Nim's `cint`; then there is part of varargs
>
> * following code to be wrapped  can be written as, [ffi01.nim](./code-samples/chap8ffi/ffi01.nim)

```
              const char *    C's int          allows variable args
             ,-------------,    |,                ,-------,
proc printf (format: cstring): cint     {.importc, varargs, header: "stdio.h".}
'-----------|----------------------'    '---|----'          '------|--------'
 standard proc declaration            import from C     specifies where printf is defined


## used as
discard printf("this %s and that %d\n", "Alice", 100)
```

* compatible Nim types are `cstring`, `cint, cuint`, `pointer`, `clong, clonglong, culong, culongulong`, `cchar, cschar, cuchar`, `cshort, cushort`, `csize`, `float, cdouble, clongdouble`, `cstringArray`

* including `{discardable.}` pragma in cases like above where return value can be ignored, would allow to not use `discard` at every call

#### Type Compatibility

* Nim's string is object with two fields length and char arrays; thus can't be itself passed to C; whereas `cint` is just an alias to `int32`. Keeping it safe for usage, `c..` types to be used.

* Conversion from `cstring` to `string` isn't implicit; thus need explicit casting via `$`.

#### Wrapping C types

* other than wrapping proc, wrapping types is important

* looking at [time](http://en.cppreference.com/w/c/chrono/time) procedure, prototype looks like `time_t time( time_t *arg);` with type `time_t` which from code looks like signed int.. so

```
type
  CTime = int64

proc time(arg: ptr CTime): CTime {.importc, header: "<time.h>"}
```

* [localtime](http://en.cppreference.com/w/c/chrono/localtime) procedure as `struct tm *localtime{ const time_t *time };`

> * it takes pointer to a `time_t` & return pointer to a `struct tm`; `struct` in Nim can be `object` (not obvious if allocated over heap or stack)
>
> * when C returns a pointer; important to know whether it needs an explicit deallocation
>
> * doc mentions it's *pointer toa static internal tm object* i.e. static storage and doesn't need deallocation
>
> * if type is `dynamic` means would get allocated at each use and would need to be explicitly deallocated, can wrap and call `free`

* a full `time` & `localtime` wrapper would be like

```
type
  CTime = int64

proc time(arg: ptr CTime): CTime {.importc, header: "<time.h>"}

type
    TM {.importc: "struct tm", header: "<time.h>".} = object
        tm_min: cint
        tm_hour: cint

# takes & returns a pointer
proc localtime(time: ptr CTime): ptr TM {.importc, header: "<time.h>".}

var seconds = time(nil)           # a new seconds var
let tm = localtime(addr seconds)  # passing seconds var to localtime
# 'addr' keyword returns pointer; thus need to create a var first for value to be stored
echo(tm.tm_hour, ":", tm.tm_min)  # display current time
```

* the field names have to match those of C Types, can specify name of each field in `importc` pragma to rename them

```
type
    TM {.importc: "struct tm", header: "<time.h>".} = object
        min {.importc: "tm_min".}: cint
        hour {.importc: "tm_hour".}: cint
```


### Wrapping an external C library

* Will wrap SDL2 (Simple DirectMedia Library) is a cross-platform multimedia library. Can find a [wrapper here](https://github.com/nim-lang/sdl2). Download [SDL here](https://www.libsdl.org/download-2.0.php#source). Consult SDL's def of procedure prototypes.

* Wrapper will be a single `sdl.nim` file w/ `sdl` module.

* With dynamic linking, must provide filename of library (that depend on OS)

```
when defined(Windows):
    const libName* = "SDL2.dll"
elif defined(Linux):
    const libName* = "libSDL2.so"
elif defined(MacOsX):
    const libName* = "libSDL2.dylib"

# to instruct compiler to dynamically link procedure, using pragma 'dynlib'
proc init*(flags: uint32): cint {.importc: "SDL_Init", dynlib: libName.}
```

> The `dynlib` pragma supports simple versioning scheme, so can provide `"libSDL2(|-2.0.1).so"` as arg to support multiple versions. [Details](http://nim-lang.org/docs/manual.html#foreign-function-interface-dynlib-pragma-for-import).

* Need to wrap types before procedures. Type internals ain't needed, these act as stubs for some objects.

```
type
    SdlWindow = object
    SdlWindowPtr* = ptr SdlWindow
    SdlRenderer = object
    SdlRendererPtr* = ptr SdlRenderer
```

* Only required procedures need be wrapped, here to show colored window.

```
int SDL_Init(Uint32 flags)
int SDL_CreateWindowAndRenderer(int width, int height, Uint32 window_flags,
                                SDL_Window** window, SDL_Renderer** renderer)
int SDL_PollEvent(SDL_Event* event)
int SDL_SetRenderDrawColor(SDL_Renderer* renderer, Uint8 r, Uint8 g, Uint8 b, Uint8 a)
void SDL_RenderPresent(SDL_Renderer* renderer)
int SDL_RenderClear(SDL_Renderer* renderer)
int SDL_RenderDrawLines(SDL_Renderer* renderer, const SDL_Point* points, int count)
```

> Each procedure need to specify `dynlib` pragma as above for `SDL_Init`. Pragma `push` allows to apply specified pragma to procedures defined below it, until `pop` pragma is used. Can use it to clean code.

```
{.push dynlib: libName.}
proc init*(flags: uint32): cint {.importc: "SDL_Init".}

proc createWindowAndRenderer*(width, height: cint, window_flags: cuint, window: var SdlWindowPtr, renderer: var SdlRendererPtr): cint {.importc: "SDL_CreateWindowAndRenderer".}

proc pollEvent*(event: pointer): cint {.importc: "SDL_PollEvent".}

proc setDrawColor*(renderer: SdlRendererPtr, r, g, b, a: uint8): cint {.importc: "SDL_SetRenderDrawColor", discardable.}

proc present*(renderer: SdlRendererPtr) {.importc: "SDL_RenderPresent".}

proc clear*(renderer: SdlRendererPtr) {.importc: "SDL_RenderClear".}

proc drawLines*(renderer: SdlRendererPtr, points: ptr tuple[x, y: cint], count: cint): cint {.importc: "SDL_RenderDrawLines", discardable.}
{.pop.}
```

* Using `sdl_test.nim`

```
import os
import sdl

const INIT_VIDEO* = 0x00000020
if sdl.init(INIT_VIDEO) == -1:
    quit("Couldn't Init SDL.")

var window: SdlWindowPtr
var renderer: SdlRendererPtr

if createWindowAndRenderer(640, 480, 0, window, renderer) == -1:
    quit("Failed creating window/renderer.")

discard pollEvent(nil)
renderer.setDrawColor 29, 64, 153, 255       # blue
renderer.clear

renderer.present
sleep(5000)

renderer.setDrawColor 255, 255, 255, 255     # white
var points = [                               # point array to draw N
    (260'i32, 320'i32),
    (260'i32, 110'i32),
    (360'i32, 320'i32),
    (360'i32, 110'i32)
]
renderer.drawLines(addr points[0], points.len.cint)
renderer.clear
renderer.present
sleep(5000)
```


### The JavaScript backend

> **My Opinion:** With advances in WASM, the importance of dealing with Wrapping JS procs seems like unnecessary hassle.

#### Wrapping the Canvas element

* Assuming `<canvas/>` with size `600x600`, code to fill canvas with Blue & draw letter N in middle as with SDL. The JS code would be like

```
var canvas = document.getElementById("canvas");
canvas.width = 600;
canvas.height = 600;
var ctx = canvas.getContext("2d");
ctx.fillStyle = "#1d4099";
ctx.fillRect(0, 0, 600, 600);
ctx.strokeStyle = "#ffffff";
ctx.moveTo(250, 320);
ctx.lineTo(250, 110);
ctx.lineTo(350, 320);
ctx.lineTo(350, 110);
ctx.stroke();
```

* Creating `canvas.nim` to wrap Canvas API. DOM available via `dom` module already wraps `getElementById`. Gotta refer JS doc like [this for CanvasRenderingContext2D](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D). Optional args can be omitted, if using default value.

```
import dom

type
    CanvasRenderingContext* = ref object
    fillStyle* {.importc.}: cstring
    strokeStyle* {.importc.}: cstring

{.push importcpp.}
proc getContext*(canvasElement: Element, contextType: cstring): CanvasRenderingContext
proc fillRect*(context: CanvasRenderingContext, x, y, width, height: int)
proc moveTo*(context: CanvasRenderingContext, x, y: int)
proc lineTo*(context: CanvasRenderingContext, x, y: int)
proc stroke*(context: CanvasRenderingContext)
{.pop.}
```

> When wrapping data type in JS, wrapped type should be declared as `ref object`. JS objects have reference semantics.

#### Using the Canvas element

* `canvas_test.nim` to use it, `nim js -o:canvas_test.js canvas_ test.nim`. `.EmbedElement` type conversion is required to allow `Element` returned by `getElementById` to have `widht, height`.

```
import canvas, dom

proc onLoad() {.exportc.} =
    var canvas = document.getElementById("canvas").EmbedElement
    canvas.width = window.innerWidth
    canvas.height = window.innerHeight
    var ctx = canvas.getContext("2d")
    ctx.fillStyle = "#1d4099"
    ctx.fillRect(0, 0, window.innerWidth, window.innerHeight)
    ctx.strokeStyle = "#ffffff"
    let letterWidth = 100
    let letterLeftPos = (window.innerWidth div 2) - (letterWidth div 2)
    ctx.moveTo(letterLeftPos, 320)
    ctx.lineTo(letterLeftPos, 110)
    ctx.lineTo(letterLeftPos + letterWidth, 320)
    ctx.lineTo(letterLeftPos + letterWidth, 110)
    ctx.stroke()
```

* Need some HTML `index.html` to use it

```
<!DOCTYPE html>
<html>
    <head>
        <meta charset="utf-8"/>
        <title>Nim wrapping JS</title>
        <script type="text/javascript" src="canvas_test.js"></script>
    </head>
    <body onload="onLoad();">
        <div id="nimCanvas" style="margin: 0; overflow: hidden;"><canvas id="canvas" style="border: 1px solid black;"></canvas></div>
    </body>
</html>
```

---
