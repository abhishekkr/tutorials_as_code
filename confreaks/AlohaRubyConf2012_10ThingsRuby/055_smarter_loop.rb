#!ruby
#1.9

animals = %w[cat bat rat]
enum = animals.to_enum
3.times do
  enum.next
end
enum.next rescue puts "Error raised: #{$!.class}"

enum.rewind
loop do
  puts "Processing #{enum.next}..."
end
