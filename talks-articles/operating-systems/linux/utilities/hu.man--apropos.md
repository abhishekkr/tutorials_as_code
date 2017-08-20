
## apropos

search manual page names and descriptions for provided keyword

* default (case-insensitive, not whole word)

```
apropos lsof
```

* use regex,wildcard

```
apropos -r .sof

apropos -w *sof
```

* combination of more than one keywords

```
apropos -a list -a lsof
```

* match exact what keyword as whole word

```
apropos -e lsof
```

* no trimming of description

```
apropos -l lsof
```

---
---
