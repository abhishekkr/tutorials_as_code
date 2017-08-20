=begin
> can run it directly using ruby/irb

[source](https://rubymonk.com/learning/books/1-ruby-primer/chapters/42-introduction-to-i-o/lessons/89-streams)

## Streams

* ruby's IO class to initialize IO streams

```
=end

fd = IO.sysopen("/tmp/temp.new.fd", "w")  ## fd is a Fixnum
p IO.new(fd)

=begin
```

* IO objects can also be created using it's subclasses like 'File', 'BasicSocket', 'TCPSocket'

```
=end

io_streams = Array.new
ObjectSpace.each_object(IO) {|x| io_streams << x }

puts "IO streams available here:"
p io_streams
puts "*"*45

=begin
```

* Std{in,out,err}

> constants STDOUT, STDIN & STDERR available as IO objects for program's stdin, stdout & stderr respectively.
> 'puts' sends data to 'STDOUT', 'gets' captures from 'STDIN' and  'warn' pushes to 'STDERR'
> 'Kernel' module provides us with '$stdout', '$stdin', '$stderr' as well pointing to IO objects
> 'Kernel' methods are available everywhere. Calling 'puts' calls 'Kernel.puts' which proxies to '$stdout.puts'

```
=end

printf "STDOUT.class: %s, STDOUT.fileno: %s, STDOUT.object_id: %s, $stdout.object_id: %s\n",
        STDOUT.class, STDOUT.fileno, STDOUT.object_id, $stdout.object_id

printf "STDIN.class: %s, STDIN.fileno: %s, STDIN.object_id: %s, $stdin.object_id: %s\n",
        STDIN.class, STDIN.fileno, STDIN.object_id, $stdin.object_id

printf "STDERR.class: %s, STDERR.fileno: %s, STDERR.object_id: %s, $stderr.object_id: %s\n",
        STDERR.class, STDERR.fileno, STDERR.object_id, $stderr.object_id


=begin
```

* These linkings can be overridden for a different these to behavior to apply on another IO stream.

> Like faking IO with StringIO and capture all 'STDERR' calls.

```
=end

capture = StringIO.new
$stderr = capture
warn "o0ps! something fishy..."
$stderr = STDERR
warn "o0ps! did it a...."

=begin
```

* using 'File' class, as a stream

```
=end
sample_file = "/tmp/temp.file.log"

begin
  file = File.open(sample_file, "r+")
  puts "file.inspect: ", file.inspect
  file.close
rescue
  puts "something wrong, file may be absent"
ensure
end

File.open(sample_file, "w") do |fyle|
  fyle.puts "jack and jill\njohn and jane doe"
end

File.open(sample_file, "r"){|fyle|
  puts fyle.inspect, fyle.read
  fyle.rewind

  buffer = ""
  p fyle.read(5, buffer)
  p buffer
  p fyle.read(5, buffer)
  p buffer

  fyle.seek(4, IO::SEEK_SET)
  p fyle.read(5)
}

p File.readlines(sample_file)

File.open(sample_file, "w") do |f|
    p f.write("you found me, now delete me")
end


=begin
```

---
---
=end
