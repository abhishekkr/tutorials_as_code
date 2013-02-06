#!ruby

age = rand(1...100)
p age

case age
when -Float::INFINITY..20
  puts 'very young'
when 21..64
  puts 'come in'
when 65..Float::INFINITY
  puts 'go rest'
end
