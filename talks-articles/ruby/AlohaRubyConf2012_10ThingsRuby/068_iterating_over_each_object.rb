#!ruby
# MRI
ObjectSpace.each_object do |obj|
  puts obj if obj.is_a? String
end
