## PRY - The Good Parts

#### Debugging a (classic Rails Demo) Blog Engine

*  got color coding
*  tab completion
*  _ for last output
*  ? for show-doc
*  _file_ is memoization of last file 'cat' touched
*  wtf? shows you backtrace
*  $ for show-source
*  <alt + _> for last word copied down
*  <ctrl + r> for search history
*  _out_ for array of all outputs, _in_ is also there
*  edit (use $EDITOR)
*  --ex location of recent exception
*  cd (change self)
*  play -l to run a particular line
*  .<cmd> used to run shell command from within
*  <ctrl + d> (cd.. or exit)
*  ; (at end of line, hides output)

---

### How Do I Use Base64 Lib?

$ pry
>  require 'base64'
>  ls
>  ls Base64
>  Base64.encode64 "Hi"
>  Base64.decode64 _
>  ? Base64.strict_encode64
>  show-doc Base64.encode64

---

### Why doesn't my method work?

$ pry
>  cat baic_auth.rb
>  require _file_
>  BasicAuth.encode 'hi', 'mum'
>  BasicAuth.decode _
>  wtf?
>  $ BasicAuth.decode
>  edit <alt + _>

---

### Where didd that nil come from?

...*code*...
binding.pry
...*code*...

>  play -l 19
>  puts object_with_bug.faulty_value
>  cd object_with_bug
>  ls
>  $ faulty_value
>  @params
>  edit --ex
>  .git diff
>  cd ..
>  whereami
>  puts object_with_bug.faulty_value

---

### Further Reading

* pry-rescue
>  *  automatically opens pry on unhandled exception or test failure

* pry-stack_explorer
>  lets you move up/down callstack as you have binding at all level

* use better_errrors with Rails

* pry-debugger
>  break, stop, next, continue

* pry-plus
>  collection of main pry plugins

* pry-rails
>  makes rails console use pry
