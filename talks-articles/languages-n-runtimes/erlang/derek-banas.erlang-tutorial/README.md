
> **check out notes at `getting-started-with-erlang`, only selected notes are placed from this source**

## Erlang Tutorial: Learn Erlang in One Video

> by Derek Banas

* [check eg01.erl for fundamental constructs](./eg01.erl)

* After module declaration, any library imports if required need can be mentioned with 1st arg as module name and 2nd arg as list of function name backslash function arity.

```
-import (string, [len/1, concat/2, chr/2, substr/3, str/2,
                  to_lower/1, to_upper/1]).
```

* Module info can be viewed by `<module_name>:module_info().`

> Like `string:module_info().` gives detail of version, all exports and other info about `string` module.

---
---
