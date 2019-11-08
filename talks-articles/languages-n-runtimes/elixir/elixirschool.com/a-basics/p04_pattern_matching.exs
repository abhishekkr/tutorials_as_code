## pattern matching

#### * one of soul features of elixir; allows to match values, data structures and even functions

### match operator
#### * with simple values
x = 1
1 = x  ## only allowed after varaible been matched to value
#### * with collections
lstx = [12, 23, 31]
[x1, x2, x3] = lstx
[xx1, _, _] = lstx
[_ | xtail] = lstx
IO.inspect(x1 == xx1)
IO.inspect(xtail == [x2, x3])

### pin operator
#### * for cases when variable rebinding is undesirable, match on existing value instead rebinding to new one
pinx = 100
{pinx, ^pinx} = {101, 100}  ## here it will fail if ^px is not 100
IO.puts(pinx)
#### * pinning a function clause
ehlo = nil
greet = fn
  (^ehlo, name) -> IO.puts("Hola #{name}")
  (ehlo, name) -> IO.puts("#{ehlo} #{name}")
end
greet.(nil, "John")
greet.("Howdy", "John")
