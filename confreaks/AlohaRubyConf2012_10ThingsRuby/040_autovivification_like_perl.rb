#!ruby

deep = Hash.new {|hash, key| hash[key] = Hash.new(&hash.default_proc)}

deep[:a][:b][:c] = 111
p deep