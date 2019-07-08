
## Julia 1.x Tutorial Misc/Level-Up

> by J-Sec

### Package Manager

* create a new package at REPL by `generate NewPackageName` in `]` package mode

> * it will create a `Project.toml` file and a main lib module file under `src`; update `Project.toml` for correct project metadata
>
> * running `activate .` and `add NameOfPackageToUse` will create/update `Manifest.toml`
>
> * check [document-summarization.jl](./sample-code/document-summarization.jl) as sample which can be used in src

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
