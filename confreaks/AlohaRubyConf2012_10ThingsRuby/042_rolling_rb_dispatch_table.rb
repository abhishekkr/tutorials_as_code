#!ruby

class Input;             def initialize(input) @input = input end end
class Number    < Input; def calculate()       @input.to_i end end
class UnaryOp   < Input; def calculate(n)      n.send("#@input@") end end
class BinaryOp  < Input; def calculate(l, r)   l.send(@input, r) end end

stack      = [ ]

loop do
  puts stack.map.with_index {|n,i| "#{i}: #{n}" }
  print "\e[93m>> \e[0m"
  line = $stdin.gets.strip
  break if line === ''
  type = case line
         when %r{\A[-+*/]\z} then BinaryOp
         when %r{\An\z}      then UnaryOp
         else                     Number
         end
  op   = type.new( line.strip.tr('n', '-') )
  stack << op.calculate( *stack.pop( op.method(:calculate).arity ) )
end
