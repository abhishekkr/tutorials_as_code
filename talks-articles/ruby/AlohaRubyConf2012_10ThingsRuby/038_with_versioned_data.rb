#!ruby

d1 = [[:f, 'F'], [:l, 'L']]
m1 = d1.assoc(:l).last
puts m1

d1.unshift([:l, 'M'])
m2 = d1.assoc(:l).last
puts m2

c = d1.assoc(:l)
p = d1[(d1.index(c) + 1)..-1].assoc(:l).last
puts p