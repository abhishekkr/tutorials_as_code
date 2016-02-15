[source](https://tenderlovemaking.com/2016/02/05/i-am-a-puts-debuggerer.html)

#### know where I'm but not how I got here

* puts caller

```
def i_am_here
  puts "#" * 90
  printf "Called from: %s\n", caller
  puts "#" * 90
  puts "wassup"
end

def came_from_here
  i_am_here
end

came_from_here # called here
```

* can have vim shortcuts and similar for often used stuff
```
" puts the caller
nnoremap <leader>wtf oputs "#" * 90<c-m>puts caller<c-m>puts "#" * 90<esc>
```

* can use 'raise' to print required stacktrace in situations

---

#### calling a method but no idea where it goes

* call source\_location over object recieved from 'method'

```
def i_am_here
  puts "wassup"
end

def came_from_here
  puts "#" * 90
  printf "Calling: %s\n", method(:i_am_here).source_location
=begin
if i_am_here was calling 'super', called super can be checked similarly as
  method(:i_am_here).super_method.source_location
=end
  puts "#" * 90
  i_am_here
end

came_from_here # called here
```

* if scope called in with 'method' has overridden definition for 'method', do this

```
method = Kernel.instance_method(:method)
p method.bind(request).call(:headers).source_location
@users = User.all
```

---

#### calling a method but no idea where it goes FORWARD

* when not immeditae next point but entire path is to be known, use Tracepoint

```
def something_complex
  @users = User.all
  tp = TracePoint.new(:call) do |x|
    p x
  end
  tp.enable
  calling_rabbit_hole ## calls under this to be determined
ensure
  tp.disable
end

## if want to see C-methods as well, use ':c_call'
```

---

#### know an exception getting raised but not where

* '-d' flag with ruby command line

The '-d' flag will enable warnings and also print out every line where an exception was raised.

```
bundle exec ruby -d $RUBY_APP_WITH_EXCEPTION

## to run RSpec using it
ruby -d -S rspec
```

* if real task runs in a sub-task, need to set RUBY_OPT

```
RUBYOPT=-d bundle exec rake test
RUBYOPT=-d rspec
```

---

#### need to find where object came from

It 'trace\_object\_allocations\_start' is available since 2.x and is a good wtf technique to follow through actual object lifecycle.
It prints out line number.

```
require 'objspace'
ObjectSpace.trace_object_allocations_start

def foo
  x = baz
  bar x
end

def bar x
  p ObjectSpace.allocation_sourcefile(x) => ObjectSpace.allocation_sourceline(x)
end

def baz; zot;        end
def zot; Object.new; end

foo
```

Similar to '-d' flag, to enable 'trace\_object\_allocations\_start' for entire run... '-r' can be used.

```
cat > "${DEBUGFILE_BASENAME}.rb" <<EOF_DEBUGFILE_BASENAME
$require 'objspace'
ObjectSpace.trace_object_allocations_start
EOF_DEBUGFILE_BASENAME

ruby -I. -r$DEBUGFILE_BASENAME $RUBY_PROGRAM
##### same as
RUBYOPT="-I -r$DEBUGFILE_BASENAME" ruby $RUBY_PROGRAM
```

---

#### know the object is mutated, dunno where

Call 'freeze' on the object after when you want the mutation to be caught.
Running will get you the stacktrace wherever the mutation happened as not allowed.

---

#### have deadlock, dunno where

similar to tracing object allocation start, can make ruby require below script to track deadlock

```
cat > "${TRACK_DEADLOCK}.rb" <<EOF_TRACK_DEADLOCK
trap(:INFO) {
  Thread.list.each do |t|
    puts "#" * 90
    p t
    puts t.backtrace
    puts "#" * 90
  end
}
EOF_TRACK_DEADLOCK

ruby -I. -r"${TRACK_DEADLOCK}" "${RUN_RUBY_TASK_WITH_DEADLOCK}"
```

press '<CTRL> + T' on MacOSX, 'kill' it on Linux

---

### wanna know when method executes, sometimes

can do something like

```
trap(:SIGINT) {
  puts "turning on debugging!"
  $foo = true
}
```

in your debug require script, and editing your foobar to be checked like

```
def foobar
  if $foo
    puts "#" * 90
    puts caller
    puts "#" * 90
  end
  # ..
end
```

'kill -2 $PID' shall get that block executed

---
---
