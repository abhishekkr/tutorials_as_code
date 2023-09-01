
## Chapter.9 Writing a Chat Application

> [sample code](./code-samples/chap9/)

* Some usecases being Advanced control flow (as Async proc), Lasy eval (as only eval params if logging is enabled), Lexer & parsers.

### Generics

* Nim Generics are related to Templates & Macros, to generate repititive code. Example `myMax` proc is code sample.

> * For cases where Nim fails to infer types, can specify them explicitly using square brackets `myMax[float](5, 10.5)`.
> * Call `myMax(5'i32, 10.1)` fails with mismatch as def accepts both params of same type.

* Can have mix of specific & generic type, as for `Container` type in sample code.

* Can constraint types supported as in `maxNum` & `isPositive` sample code. Many genric type classes are already defined, [check here](http://nim-lang.org/docs/manual.html#generics-type-classes).

* `Concepts` (a.k.a `user-defined type classes`) can specify requirements that a matched type must adhere to. Type constraints can be implemented to check `Comparalable` being supported.

> Concept is composed of 1+ expressions. Concepts are a new feature.


### Templates

* Generates code. Invoked as proc while compile, substituting any template with generated code.

* Stdlib has `template `!=` (a, b: untyped) =  not (a == b)` meaning `doAssert(5 != 4)` gets written as `doAssert(not (5 == 4))`.

* Template params can accept code block as in sample code. Also support multiple code block, [check here](http://nim-lang.org/docs/manual.html#procedures-do-notation).

* `untyped` arg allows identifiers with undeclared types; usable in mix with typed args as in decalreVar.

* `type, var, let, const, proc, iterator, converter, template, macro, method` are hygienic by default


### Macros

* Nim macro is procedural, executed at compile time, operate on AST. Don't substitute but generate code procedurally.

* CTFE (Compile-time Func Exec) has limits: No FFI access. Global var aren't accessible unless annotated with `{.compileTime.}` pragma.

* `dumpTree` macro from `macros` module can output AST of a code block.

* Every macro must have a return type; must generate a valid AST even if empty StmtList node as in `arguments` in sample code.

* All params are Nim AST nodes (except `static[T]` & `typedesc`).

> Macros give greater flexibility as allowing heterogeneous elements in Arrays.


### Creating configuration DSL

> * [code as library project, cfg9](code-samples/chap9/cfg9)
>
> will create a config DSL as sample, to support code mapping `config` type as below

#### Starting the project

```
import cfg9

config AppCfg9:
    address: string
    port: int

var cfg = newAppCfg9()
config.load("app.cfg")
echo("Config | address: ", cfg.address, " | port: ", cfg.port)
```

* Begin with `import macros` & `dumpTree` to test DSLs syntax (checking if language syntax allows it) under `src/cfg9.nim`

```
import macros

dumpTree:
    config AppCfg9:
        address: string
        port: int
```

> compiling this file gives

```
StmtList
  Command
    Ident "config"
    Ident "AppCfg9"
    StmtList
      Call
        Ident "address"
        StmtList
          Ident "string"
      Call
        Ident "port"
        StmtList
          Ident "int"
```

* Saving below macro

```
macro config(typeName: untyped, fields: untyped): untyped =
  result = newStmtList()
  echo treeRepr(typeName)
  echo treeRepr(fields)


config AppCfg9:
  address: string
  port: int
```

> give following output

```
Ident "AppCfg9"
StmtList
  Call
    Ident "address"
    StmtList
      Ident "string"
  Call
    Ident "port"
    StmtList
      Ident "int"
```

> * `AppCfg9` object stores config data
> * `newAppCfg9` constructor proc inits new `AppCfg9`
> * `load` proc parses specified file to populate `AppCfg9`

#### Generating object type

* The above `config` block need to generate `type ~ AppCfg9 = ref object ..`

```
dumpTree:
  type
    AppCfg9 = ref object
      address: string
      port: int
```

> gives

```
StmtList
  TypeSection
    TypeDef
      Ident "AppCfg9"
      Empty
      RefTy
        ObjectTy
          Empty
          Empty
          RecList
            IdentDefs
              Ident "address"
              Ident "string"
              Empty
            IdentDefs
              Ident "port"
              Ident "int"
              Empty
```

> * Several `Empty` nodes for optional constructs to ensure index of each node remains same.
> * `nnk..` prefix are for Node Kinds. `newTree` takes node kind as an arg with 0+ child nodes added automatically to new Nim AST.
> * `newIdentNode` accepts `string` or `NimIdent` arg to create `nnkIdent` node. Can refer to var or proc name.
> * `newEmptyNode` proc creates `nnkEmpty` node.
> * `nnkIdent` & `nnkEmpty` can be created via `newTree()` call as well.
> * `createRefType` proc accepts `identDefs` as arg (generated elsewhere, in cfg9 via `toIdentDefs`)

#### Generating constructor proc

* Constructor doesn't need much but init reference object.

* Can use template to generate it. Using `macros.getAst` can get AST for generated code within `config` macro.

#### Generating load proc

* Generate `load*` code & add it to result in config macro using `createLoadProc`.

#### Testing cfg9

* `nimble test` in root of `cfg9` project would run tests from `tests/test1.nim` that has a check to parse `tests/sample-dsl.cfg` using `cfg9.nim` code.

---
