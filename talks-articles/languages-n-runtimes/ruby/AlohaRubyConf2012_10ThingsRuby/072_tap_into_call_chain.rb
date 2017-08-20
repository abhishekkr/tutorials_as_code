#!ruby

puts "sssssssssxxxyyyyz".sub(/\A(\w)\1+/, '\1')
                        .tap {|str| p str.size}
                        .reverse
                        .capitalize
