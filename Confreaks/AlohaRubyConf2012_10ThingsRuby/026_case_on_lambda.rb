#!ruby

require 'prime'

n = rand(1...10)
puts n

case n
when lambda(&:prime?)
  puts 'prime'
when lambda(&:even?)
  puts 'even'
else
  puts 'odd'
end