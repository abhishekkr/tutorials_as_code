#!ruby
#1.9
#map() + flatten() = flat_map()

require 'pp'

chess_squares = ("A".."H").flat_map{|col|
  (1..8).map{|row| "#{col}#{row}" }
}

pp chess_squares
