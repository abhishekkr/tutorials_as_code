#!ruby

Person = Struct.new(:name, :gender)
people = [ Person.new("Jack", :male),
           Person.new("Jill", :female),
           Person.new("Hill", :male) ]
males, females = people.partition{|person| person.gender == :male}

puts "Males: ", males.map{|m| "  #{m.name}"}
puts "Females: ", females.map{|f| "  #{f.name}"}
