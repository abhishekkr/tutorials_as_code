#!ruby

operations = { number:    ->(n)         { n.to_i            },
               unary_op:  ->(op, n)     { n.send("#{op}@")  },
               binary_op: ->(op, l, r)  { l.send(op, r)     }
              }
stack      = [ ]

loop do
  puts stack.map.with_index {|n,i| "#{i}: #{n}" }
  print "\e[93m>> \e[0m"
  line = $stdin.gets.strip
  break if line === ''
  type = case line
         when %r{\A[-+*/]\z} then :binary_op
         when %r{\An\z}      then :unary_op
         else                     :number
         end
  op   = operations[type]
  stack << op[line.strip.tr('n', '-'), *stack.pop(op.arity - 1)]
end
