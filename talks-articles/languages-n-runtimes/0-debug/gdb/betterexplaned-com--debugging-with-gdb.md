
## Debugging with GDB

> [source: BetterExplained.com](https://betterexplained.com/articles/debugging-with-gdb/)


### Getting Started: starting and stopping

* `gcc -g myprogram.c`, compiling program with debugging option to let you use variables and function names inside GDB, instead of raw memory locations

* `gdb a.out` opens `a.out` in debugger, doesn't start running

* can run `help` to list topics, with which `help <topic>` can be used for actual commands lists

* to run program in debugger, usually breakpoints are set before running

```
r             ## to plainly run

r arg1 arg2   ## to pass arguments while running

r < somefile  ## feed in a file with arguments
```

* `q` to quit GDB


### Stepping through code

> to trace path and reach point of origin for concern

* `l` list 10 lines of code at current code point, `l 25` to use line:25 as center line, `l myfunc` to list the function

* `next` run until next line and pause, if current line is a function execute entire and then pause

* `step` runs the next instruction, it dives into function if next instruction happens to be function

* `finish` finish executing current function and pause, if wanna finish rest of function at once


### Breakpoints and Watchpoints

> to quickly enquire a certain code position, need to pause there

* `break 35` sets breakpoint at line:35, `break myfunction` to break at `myfunction`

* `watch x == 3` sets a watchpoint on `x == 3`, pauses the program when condition changes

* `continue` resume run until hit another breakpoint/watchpoint

* `delete N` to clear out a breakpoint


### Setting Variables and Calling Functions

> viewing and changing states at runtime, typically when paused

* `print x` to show value of var `x`, compile need to have debug information to use real names

* `set x = 3` to change `x to 3`, `set x = y` to change value to variable `y`

* `call myfunc()` to call user-defined function, `call urfunc(x)` to call with arg; beware of calling buggy function

* `display x` to show value of `x` after every step or pause, `undisplay x` to stop the regular show


### Backtrace and Changing Frames

> shows frame of a single function call when in program

* `bt` prints current stack, `bt full` shows full backtrace with local variables

* `up` and `down` to move to next or previous frame respectively

* `return` to be back from current function


### Crashes and Core Dumps

> core dump can be read and give the line number of crash, argument passed and more

* `gdb myprogram core` to debug myprogram with `core` as dump file

* can use `bt` to examine details


### Handling Signals

> events thrown at certain events, GDB might pause and can be ignored

* `handle <signalname> <action>`

* varied levels of ingoring as

```
handle SIGUSR1 nostop

handle SIGUSR1 noprint

handle SIGUSR1 ignore
```


### Tips

* prefer watchpoints over breakpoints

* `printf` works well for tracing, wrap `printf` in a `log` function for flexibility

* pass a log level with message (1 most important, 3 least, 0 off), tweak log function to persist errors

* use `#define LOG_LEVEL LOG_WARN` to display warning and above, `#define LOG_LEVEL LOG_NONE` to turn of debugging

```
#include

#define LOG_NONE 0
#define LOG_ERROR 1
#define LOG_WARN 2
#define LOG_INFO 3
#define LOG_LEVEL LOG_WARN


// shows msg if allowed by LOG_LEVEL
int log(char *msg, int level){
  if (LOG_LEVEL >= level){
    printf("LOG %d: %s\n", level, msg);
    // could also log to file
  }

  return 0;
}

int main(int argc, char** argv){
  printf("EHLO!\n");

  log("Really bad error!", LOG_ERROR);
  log("Warning, not so serious.", LOG_WARN);
  log("Just some info, not that important.", LOG_INFO);

  return 0;
}
```

---
