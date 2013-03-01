#!ruby

ring = %w[one two three].cycle
puts ring.take(10)