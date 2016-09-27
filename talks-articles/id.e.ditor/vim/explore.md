
> source:
> * anonymous, here and there
> * Improving VIM Speed, thoughtbot
> * VIM Navigation Commands, 
> * [shortcuts](http://stackoverflow.com/questions/1218390/what-is-your-most-productive-shortcut-with-vim)
> * [selection shortcuts](http://stackoverflow.com/a/1218429)
> * [tips](http://vim.wikia.com/wiki/Best_Vim_Tips)

* normally be in normal mode if not inserting text

* repeat last mode instruction with `.`

* and yes you exit via `:q`
> `:wq` if wanna save and quit
> `:q!` if wanna discard changes and quit

now lets move to smooth user experience

### Navigate

* `h` ~ left
* `j` ~ down 
* `k` ~ top
* `l` ~ right
> so, `9l` move you 9chars to right; `5k` moves you 5 lines up

* `w` ~ move by word; `W` is forward big word which includes non-alphabet making it
* `b` ~ move back word by word to start char; `B` is for back big word
* `e` ~ to end of word char; `E` to end char of big word
* `ge` ~ backward end of word
> so for text `abc-xyz`, `dw` deletes `abc` and `dW` deletes `abc-xyz` when cursor on `a`

* `g_` ~ last char of line
* `$` ~ end of line
* `0` ~ beginning of line
* `^` ~ first char of line

* high(`SHIFT+h`), medium(`SHIFT+m`), low(`SHIFT+l`) to move to top/middle/low of current page

* half page-up `CTRL + d`; full page-up `CTRL + f`
> so say if you wanna move bottom line of page to center, `CTRL+d ; SHIFT+m`

* bottom-of file `G`, top-of file `gg`
* to goto line 50, `50gg` or `:50`

* to move current line to middle of page (if possible), `zz` or `z.`
> so say if you wanna move bottom line of page to center, `SHIFT+l ; z+z`

* move current line one below on page, `CTRL + y`
* move current line one up on page, `CTRL + e`

* move to 50% of file by `50%`, and similar

* for a center piece, set a mark `m<MARK_CHAR>` and then to jump `BACKTICK+<MARK_CHAR>`

* `*` to highlight search the current word and finds next, `#` will find previous

* jump through paragraph or code-blocks using `{` and `}` as around blocks
* jump through paragraph or code-blocks using `(` and `)` as start of it
> `d + }` deletes whole next block when called from a line before

* jump to open-close of brackets using `%`

* f<CHAR> ~ move to next char occurence
* t<CHAR> ~ move to one position before next char occurence
> `;` to search next occurence of char and `,` for previous when using `f` or `t`
> but when using `,` with `t` it moves to one after occurence
>
> * df<CHAR> ~ remove everything till next occurence of char including that
> * cf<CHAR> ~ remove everything till next occurence of char including that and change to insert-mode
> * dt<CHAR> ~ remove everything till next occurence of char
> * ct<CHAR> ~ remove everything till next occurence of char and change to insert-mode

---

### Relative Line Numbering

```
command! NOREL set norelativenumber
command! NORELNO set relativenumber
```

* jump to 7th line down from current `+7`, 5th line up `:-5`

* say delete 7 lines down, `dd7` or `d6j`

* yank line from 2nd line down to 4th line down, `+2,+4y`

* copy from relative line blocks to current location `:-15,-10co.`

---

### Split & Explore current dir or any dir

* vertical split to a file `:vp /path/to/file`, for horizontal `:sp /path/to/file`

* vertical split to explorer `:Vex`, horizontal for same `:Sex`
> explorer at a path `:Sex /path/to/dir`

* `CTRL + W ; h|j|k|l` to move around

* for arrow-key movement

```
nmap <Leader><tab> <C-w>w
nmap <Leader><up> <C-w>k
nmap <Leader><down> <C-w>j
nmap <Leader><left> <C-w>h
nmap <Leader><right> <C-w>l
```

---

#### Selection

```
v%, vib  : if cursor is on starting/ending parenthesis and if inside block
vi", vi' : for double quotes and for single quotes
viB, vi{ : to select a curly brace block
ggVG     : entire file

```

#### Tips

```
:Ex     : file explorer note capital Ex
\be     : show buffer explorer (requires plugin)
:ls     : list of buffers(eg following)
:cd ..  : move to parent directory
guu     : lowercase line
gUU     : uppercase line
~       : invert case (upper->lower; lower->upper) of current character
gf      : open file name under cursor (SUPER)
ga      : display hex, ascii value of character under cursor
g8      : display hex value of utf-8 character under cursor
ggg?G   : rot13 whole file
xp      : swap next two characters around
CTRL-A,CTRL-X : increment, decrement next number on same line as the cursor
CTRL-R=5*5    : insert 25 into text
=             : (re)indent the text on the current line or on the area selected (SUPER)
=%            : (re)indent the current braces { ...  }
G=gg          : auto (re)indent entire document

* verbs
> d : Delete
> c : Change

```

---
---
