#!/usr/bin/env ruby
# self ~ default receiver, current object

class Silly
  def meth1
    @var = 100
    meth2
  end

  def meth2
    puts "@var = #@var"
  end
end

Silly.new.meth1

class Idiot < Silly
  
  def meth3
    @var = 10
    meth2
    meth1
    meth2
  end
end

Idiot.new.meth3


## Ghost Class
### Singleton Class
### Eigenclass
### MetaClass
### UniClass
###

animal = 'cat'

def animal.speak
  puts 'meow'
end

animal.speak
puts animal.methods.select{|meth| meth =~ /speak/ }


### class and self
puts "Before class def, self is #{self}"
class Dummy
  puts "Inside Class def, self is #{self}"

  def d; puts "Inside method, self is #{self}" ; end
end
puts "After class def, self is #{self}"
Dummy.new.d
