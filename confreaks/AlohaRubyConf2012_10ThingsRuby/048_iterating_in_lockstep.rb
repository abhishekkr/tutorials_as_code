#!ruby

letters = "a".."d"
numbers = 1..3
p letters.zip(numbers)
letters.zip(numbers) do |letter, number|
  p(letter: letter, number: number)
end
