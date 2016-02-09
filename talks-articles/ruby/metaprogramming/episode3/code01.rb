#!/usr/bin/env ruby
# blocks

#
## blocks
(0..10).each{|idx| print idx }
puts
  ### 1st way
l1 = lambda{|var| var + 10}
  ### 2nd way
l2 = Proc.new{|var| var - 10}
  ### 3rd way
def convert(&block)
  block
end
l3 = convert{|var| var * 10}
# 'proc' (not Proc) deprecated
puts l1.call(90), l2.call(110), l3.call(10)

l4 = lambda{|a,b,c| a + b + c}
puts l4.call(5, 10, 15)

# lambda like method call
# Proc.new is liberal with argument count, parallel assignment
begin
  puts l4.call(1,2,3,4,5,6,7)
rescue ArgumentError => e
     puts e
end
l5 = lambda{|a,b,*c| "#{a.inspect} && #{b.inspect} && #{c.inspect}"}
puts l5.call(5, 10, 15), l5.call(5, 10, 15, 25, 40)
puts l2.call(1,2,3,4)
l6 = Proc.new{|var| var.inspect}
puts l6.call(1,2,3,4,5)
l7 = Proc.new{|a, b, c| "#{a.inspect} && #{b.inspect} && #{c.inspect}"}
puts l7.call(1,2,3,4,5)

#
## difference between way lambda and Proc.new return values
def meth1
  puts "start meth1"
  pr = Proc.new{ return 'Proc' }
  pr.call
  puts "end meth1"
end
def meth2
  puts "start meth2"
  pr = lambda{ return 'lambda' }
  pr.call
  puts "end meth2"
end
puts "before meth1"
puts meth1 # lambda return exits Proc
puts "after meth1, before meth2"
puts meth2 # Proc.new return exits surrounding context
puts "after meth2"
