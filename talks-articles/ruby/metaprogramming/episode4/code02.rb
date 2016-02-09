#!/usr/bin/env ruby
# class_eval (can only be called on classes and modules) ; module_eval

String.class_eval do
  puts "# #{self}"
end

a = 'a'
puts 'a'

String.class_eval do
  def red
    puts "\e[31m#{self}\e[0m"
  end
end
'RED'.red


#
## class_eval goes to reciever/instance-method/
## (unlike instance_eval going to singleton/class-method/)
class Thing
  def initialize
    @var = 100
  end
end
t = Thing.new
Thing.class_eval{
  def hello
    puts self, @var
  end
}
t.hello


#
## create methods w/o closure
module Accessor
  def my_attr(name)
    class_eval %{
      def #{name} ; @#{name} ; end
      def #{name}=(val) ; @#{name} = val ; end
    }
  end
end
class Ex1
  extend Accessor
  my_attr :var
end
ex1 = Ex1.new
ex1.var = 1000
puts ex1.var


#
## more..
module Inspector
  def pinspect
    puts "[+] #{self.inspect}"
  end
end
[String, Array, Hash].each do |clas|
  clas.class_eval{ include Inspector }
end
'i'.pinspect
[1, 2].pinspect
{}.pinspect

class Cursor
  def initialize
    @path = []
  end
  def right(n=1) ; @path << 'R'*n ; end
  def up(n=1) ; @path << 'U'*n ; end
  def path ; @path.join ; end

  def move(&block)
    instance_eval(&block)
  end
end
c = Cursor.new
c.right(3)
c.up(2)
c.move do
  right 10
  up 1
  right 1
end
puts c.path
