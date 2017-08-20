=begin
> can run it directly using ruby/irb

## Lambdas

```
=end

gimme10 = lambda { 10 }
puts 'gimme10: ', gimme10.call

empty = lambda do |string|
  if string == ""
    return true
  else
    return false
  end
end
puts "empty \"\"", empty.call("")

Increment = lambda { |number|  number + 1  } 
puts "Increment.call(10)", Increment.call(10)

=begin
```

* a block working as Lambda

```
=end

def block_increment(number)
  yield(number)
end

puts block_increment(1) { |number| number + 1  }

someval = block_increment(1) do |number|
  number + 10
end
puts someval

=begin
```
> above example, calling the block followed by a lambda body
> 'yield' allows to inject a block somewhere in a function where required to have varied functionality

=end

def yet_another_foo(*tokens)
  yield(*tokens)
end

puts yet_another_foo(10) {|n| n}
puts yet_another_foo(5,2) {|m,n| m*n}
puts yet_another_foo(5,2,1) {|m,n,o| (m*n)-o}

=begin
```

---
---
=end
