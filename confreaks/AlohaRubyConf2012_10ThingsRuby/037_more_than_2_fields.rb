#!ruby

any = [ %w[a b c d], %w[e f 1\ 2 z]]
f1, f2, f3, f4 = any.assoc('e')
puts f1, f4
f1, f2, f3, f4 = any.assoc('a')
puts f1, f4