
## GDB Tutorial: A Walkthrough with Examples

> [source: cs.umd.edu](https://www.cs.umd.edu/~srhuang/teaching/cmsc212/gdb-tutorial-handout.pdf)

* additional step when compiling, like `-g` switch for `gcc` to enable built-in debugging support needed by `gdb`

* starting up `gdb` and load a program to debug using command `file` as `(gdb) file myprog.x`

> can directly start with `gdb myprog.x`

* auto-complete words with TAB key, can check help with `help <command>`

* `run` command runs the loaded program, in case of issues it shall show some facts about it

* can break program in middle with `break file1.c:15`, if program ever reaches mentioned **file:line** pair it will pause and prompt

* to make gdb break at a function call use `break my_func`

> * can use `run` again to continue after pause, to reach to next breakpoint use `continue`
>
> * can use `step` with fine-grained control to proceed, similarly `next` single-steps as well the next sub-routine instead just next instruction

* `print <var>` command prints value of `var`, `print/x` prints value in hexadecimal

* set watchpoint on state like value of variables changing, example `(gdb) watch my_var`, it will interrup run and print old/new values

* other useful commands

> * `backtrace` produces a stack trace related to seg fault
>
> * `where` shows the next statement to be executed, where seg fault occured
>
> * `finish` completes fun till current function's end
>
> * `delete` to remove a breakpoint
>
> * `info breakpoints` shows info on all declared breakpoints

* using conditional breakpoints allow more fluidity, like `(gdb) break file1.c:6 if i >= SOMESIZE`


### Fun with pointers

* assume following struct

```
struct book {
  int uuid;
  char *name;
  float prince;
  int pages;
}
```

* assume we are in gdb at some point in execution after a line that looks like `struct book * b1 = <somebook>;`

* to see value of pointer `(gdb) print b1`

* checking particular field of struct by referencing

```
(gdb) print b1->uuid
(gdb) print b1->name
(gdb) print b1->price
(gdb) print b1->pages
```

* can also use

```
(gdb) print (*b1).uuid
(gdb) print (*b1).name
(gdb) print (*b1).price
(gdb) print (*b1).pages
```

* to see entire content of struct `(gdb) print *b1`

* can follow pointers iteratively as `(gdb) print list_ptr->next->next->data`, for something like linked list

---
