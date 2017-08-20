#!ruby
#1.9

# instead of:
p (1..3).inject({}){|hash, n| hash[n] = true; hash }

# each_with_object
object = (1..3).each_with_object({}) do |n, hash|
  hash[n] = true
end
p object
