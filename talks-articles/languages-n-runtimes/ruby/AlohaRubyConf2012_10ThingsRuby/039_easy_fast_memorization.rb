#!ruby

fib = Hash.new{|num, idx|
  num[idx] = fib[idx - 2] + fib[idx - 1]
}.update(0 => 0, 1 => 1)

p fib[9]