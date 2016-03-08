## Trivia from [Haskell Wiki](https://wiki.haskell.org/)

#### [Bottom](https://wiki.haskell.org/Bottom)

Term 'bottom' refers to computation which never completes successfully.

Mathematical symbol for it '⊥'.

Bottom has an arbitrary type.

```
bottom = bottom
-- or
bottom = error "Non-terminating computation"

-- prelude exports a function
undefined = error "Prelude.undefined"
```

---
---
