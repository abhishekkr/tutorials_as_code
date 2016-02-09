#!ruby
# ./RB a.png

puts '>>>>>'
width, height = ARGF.read(24).unpack("@16N2")
puts "Width:  #{width} pixels"
puts "Height: #{height} pixels"
