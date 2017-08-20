## Rubocop

Cop to make people adhere to law of writing Ruby with style.

```
rubocop $PATH_WITH_RUBY_FILES_TO_COP
```

* To show all cops available

```
rubocop --show-cop
```

* To run a specific cop, say 'lint'

```
rubocop --lint
```

* Load order for config file to override default cop configs

> 1. 'rubocop/config/enabled.yml'
> 2. 'rubocop/config/disabled'
> 3. '.rubocop.yml'

to get current state of all configs
```
rubocop --auto-gen-config
```

---
---
