#!/usr/bin/env ruby
# Inheritance

class Formatter
  def initialize(text)
    @text = text
  end
end

class TextileFormatter < Formatter
  def to_html
    "<html>#@text</html>"
  end
end

class RDocFormatter < Formatter
  def to_rdoc
    #
  end
end

t = TextileFormatter.new("*strong* coffee(tm)")
puts t.to_html


#
##
my_arry = Array
class Container < my_arry
end
p Container.superclass

class C2 < (rand < 0.5 ? Array : Hash)
end
c = C2.new
c[0] = 'hellp'
p c


#
## subclass
Person = Struct.new(:name, :likes)
d = Person.new('dav', 'rb')
puts d

Person2 = Struct.new(:name, :likes)
class Person2
  def to_s
    "#{self.name} likes #{self.likes}"
  end
end
d = Person2.new('dav', 'rb')
puts d

Person3 = Struct.new(:name, :likes) do
  def to_s
    "#{self.name} likes #{self.likes}"
  end
end
d = Person3.new('dav', 'rb')
puts d


#
## Instance methods
animal = 'cat'

class << animal
  def speak
    puts "#{self} meow"
  end
end

animal.speak

class Dave
  class << self
    def say_hello #does self.<> ...for such scneario instead of <<self
      puts "Hello"  # directly define self.say_hello
    end
  end
end
Dave.say_hello

class Jane
  @count = 0
  class << self
    attr_accessor :count
  end

  def initialize
    Jane.count += 1
  end
end
Jane.new
Jane.new
puts Jane.count
