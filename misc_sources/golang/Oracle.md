
## Go Oracle

> it's a source analysis tool

```
go get golang.org/x/tools/cmd/oracle
```

Mode argument determines the query to perform:
* callees : show possible targets of selected function call
* callers : show possible callers of selected function
* callstack : show path from callgraph root to selected function
* definition : show declaration of selected indentifier
* describe : describe selected syntax; definition, methods, etc
* freevars : show free variables of selection
* implements : show 'implements' relation for selected channel op
* peers : show send/receive corresponding to selected channel op
* referrers : show all refs to entity denoted by selected indentifier
* what : show basic information about the selected syntax node
