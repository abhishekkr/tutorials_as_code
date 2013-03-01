#!ruby

aa = [ %w[a b], %w[1 2], %w[c d]]
p aa.assoc('a')
p aa.rassoc('2')

p aa.assoc('c')
p aa.rassoc('d')