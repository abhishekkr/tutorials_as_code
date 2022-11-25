
## Chapter.5 Package Management

> [nimble @github](https://github.com/nim-lang/nimble)


### The Nim Package Manager; Installing Nimble

> * Nimble is now bundled with Nim. If it's not available, try `koch nimble`


### The nimble command-line tool

> * `nim refresh` to update package list
> * `nim list` to list available packages
> * `nim search $search_term` to search for packages for a requirement
> * `nim install $pkg_name_or_url` to install a Nim package; `nim uninstall $pkg_name` to remove


### What is a Nimble package?

* simply stating a directory with `<package-name>.nimble` file and module(s) can act as a Nimble package; can run `nimble init` for interactive generation

```
# PkgName.nimble
## package info
version               = "0.1.0"
author                = "abhishekkr"
description           = "a sample module"
license               = "MIT"

## dependencies
requires     "nim >= 1.6.0"
```

* can provide other modules as dependency, also provide tasks as

```
task test, "Run package test":
  exec "nim c -r tests/alltests.nim"
```

* can run `nimble install` from within a local package on filesystem at dirpath with `$pkg.nimble`; could pass a url of Git/Mercurial repo

* `nimble search --ver $search_term` would list versions, managed as tags of SCM


### Installing Nimble packages

* can install from local FS, SCM URL, by name for a specific version and multiple at once

```
nimble install daemonize             # latest version by name
nimble install daemonize@>=0.0.1     # specific ranged version by name
nimble install daemonize@#b98cb9     # specific commit state version
```

> * package gets copied to `~/.nimble/pkgs/$pkg_name`; default package list is available [here](https://github.com/nim-lang/packages)
> * checks if tagged version matchs value in `$pkg_name.nimble`, dir-layout is compatible and dependency tree is available

### Creating a Nimble package

* created a Nim package [denim](#), to collect useful flows while learning

> * must place all modules in a separate dir named after the package; as in under root of `pkgX` must be a subdir `pkgX` with module code eg. `pkgX/pkgX`
> * can place a primary feature module in root, but must have filename that of package name eg. `pkgX/pkgX.nim`
> * modules within a package to be used only internally shall be placed in a subdir `private` within package-name dir; eg. `pkgX/pkgX/private`

```
## below code would work in pkgX/pkgX/data.nim
## now if pkgX/pkgX/db.nim has connect() and pkgX/pkgX/private/conf.nim has connAddress()
import pkgX/connect
import pkgX/private/conf

let dbHandle* = connect(connAddress())
```

* with dir structure and nimble config and source files done, `nimble install` shall install package locally.. at `~/.nimble`

* if need another package as dependency, `nimble install anotherpkg` and add it to nimble config as `requires "nim >= x.x.x", "anotherpkg"`

* `nimble c module` would compile by itself fetching dependency from nimble config


### Publishing Nimble packages

* push it to a Git/Hg (Github/Bitbucket) repo

* create a PR to `github.com/nim-lang/packages` or `nimble publish` with interactive inputs


### Developing a Nimble package

* semantic versioning managed in nimble config file as `version` key

* Nimble uses tags in package repo; so increment `version` key value, commit with msg `Version 1.2.3` and tag it as `git tag v1.2.3` then `git push --tags`

---
