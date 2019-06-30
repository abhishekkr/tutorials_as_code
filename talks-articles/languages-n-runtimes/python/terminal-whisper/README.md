# Terminal Whispering

> at PyGotham2015 by Thoman Ballinger

* [netcat demo with publish delay, typewriter style, progress bars, more](./netcat-sample.py)

* [stdout with progress reports, no echo at all of typing](./stdout-sample.py)

* [git colored commit trick](./git-commit-trick)


## Notes

### ANSI Escape Sequences

[more at](https://en.wikipedia.org/wiki/ANSI_escape_code)
[some](http://wiki.bash-hackers.org/scripting/terminalcodes)

* '\x1b[22A'    move cursor 23 rows up
* '\x1b[2J'     clear entire screen
* '\x1b[ ?251'  hide the cursor
* '\x1b[1m'     start writing in bold
* '\x1b[31m'    start writing in red

---

### Python libraries

* 'blessings' for handling colors and stuff.
* 'blessed' fork of blessing with great stuff
* 'termios' to handle trun of echo, used in `getpass`.
[termios](https://docs.python.org/2/library/termios.html)

* Urwid (widget library for terminal)
[urwid](http://urwid.org/)

* Pudb debugger

* [Click](http://click.pocoo.org/5/)

* [Clint](https://pypi.python.org/pypi/clint/)

* [readline](https://en.wikipedia.org/wiki/GNU_Readline) has python lib

* [python-prompt-toolkit](https://github.com/jonathanslenders/python-prompt-toolkit) interactive console utils

* PTY - Pseudo Terminal to run interactive tool in controlled way

* Pexpect - handle interactive utils

---

### Command line utils

> checkout manpages

* tput
* raw
* termios
* ioctl
* fcntl
* sttl
* isatty

```
tput cols   # columns in terminal as of moment
tput lines  # lines in terminal as of moment

stty -echo
### no stdout echo of types characters
stty echo

reset ### to get rid of funny control set like font/color
```

---

### secret terminal foo with these Dials

* no echo
* immediate echo
* terminal size
* non-blocking input
* os.isatty(sys.stdout.fileno())

---

### Console Sessions shared right way

* TermCast

* ASCIICinema

---
