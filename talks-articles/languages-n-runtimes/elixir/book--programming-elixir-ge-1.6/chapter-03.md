
## Chapter.3 Immutability

> a tweet by @jessitron, `GOTO was evil for "how did I get to this point of execution?"; Mutability leaves with "How did I get to this state?"`

* all data (values) is Immutable in Elixir, on change Elixir makes a copy of original with new values so no code holding original instance gets impacted by change

* since data is immutable, gets reused in part or whole when new structures are built

```
iex> lst1 = [1,2,3]
[1, 2, 3]
iex> lstx1 = [4 | lst1 ]
[4, 1, 2, 3]

## here 'lstx1' will keep on using 'lst1' as tail since that copy would be immutable
```

* each process has its own heap, data in app is divided up between these processes so each heap is much smaller so GC runs faster

* if process terminates before heap fills up, no GC is required

* any function modifying data will return a new copy of it

---
