#!ruby

numbers = 1..10

p numbers.take(3)
p numbers.drop(7)
p numbers.take_while{|n| n <= 5 }
p numbers.drop_while{|n| n <= 5 }
