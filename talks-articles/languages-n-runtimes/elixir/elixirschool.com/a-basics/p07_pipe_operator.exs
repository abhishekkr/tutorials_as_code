## pipe operator

### introduction
#### * passes result of LHS expression as first parameter to RHS expression
add_1 = fn(x) -> x + 1 end
add_2 = fn(x) -> x + 2 end
add_3 = fn(x) -> x + 3 end
add_4 = fn(x) -> x + 4 end

IO.inspect(add_4.(add_3.(add_2.(add_1.(10)))))
add_1.(10) |> add_2.() |> add_3.() |> add_4.() |> IO.inspect()

### examples
"John Doe" |> String.upcase() |> String.split() |> IO.inspect()
"Jane Doe" |> String.ends_with?("oe") |> IO.inspect()


### best practices
#### * always use parentheses with piped fn
