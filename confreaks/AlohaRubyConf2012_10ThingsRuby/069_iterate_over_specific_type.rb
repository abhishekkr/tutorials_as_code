#!ruby
# MRI
ObjectSpace.each_object(Integer) do |obj|
  puts obj
end
