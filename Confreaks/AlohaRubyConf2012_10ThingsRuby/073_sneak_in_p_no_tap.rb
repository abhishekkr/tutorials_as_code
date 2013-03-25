#!ruby
# 1.9 p() returns its args

puts p('ssssssssssssxxxyyyz'.sub(/\A(\w)\1+/, '\1')).reverse.capitalize
