#!/usr/bin/env ruby
#prototype based dev

animal = 'cat'
def animal.speak
  puts "meow #{self}"
end
animal.speak

other = animal.clone  #dup fails
other.speak


#
# different aproach
Animal = Object.new

def Animal.number_of_feet=(feet)
  @number_of_feet = feet
end

def Animal.number_of_feet
  @number_of_feet
end

Animal.number_of_feet = 4
puts Animal.number_of_feet

Cat = Animal.clone
Cat.number_of_feet = 4
puts Cat.number_of_feet

felix = Cat.clone
puts felix.number_of_feet

## or also
def Animal.with_feet(feet)
  new_animal = clone
  new_animal.number_of_feet = feet
  new_animal
end

Dog = Animal.with_feet 4
duffy = Dog.clone
puts duffy.number_of_feet
