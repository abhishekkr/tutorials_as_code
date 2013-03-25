#!ruby

ring = [:one, [:two, [:three]]]

ring.last.last << ring

position = ring
10.times do
  puts position.first
  position = position.last
end