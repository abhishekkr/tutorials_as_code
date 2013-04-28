#!/usr/bin/env ruby
# bindings

eval 'puts 1 + 2'
#
## bindings
def simple(param)
  lvar = 'any value lvar'
  binding
end

s1 = simple(100)
s2 = simple(100){'some block'}
eval 'puts param', s1
eval 'puts lvar', s1
eval 'puts yield',s2


## modified view
class Simple
  def initialize
    @ivar = 'value of ivar'
    binding
  end

  def simple(param)
    lvar = 'value of lvar'
    binding
  end
end

s3 = Simple.new
s4 = s3.simple(10){'blocklizer'}
eval 'puts param', s4
eval 'puts lvar', s4
eval 'puts yield',s4
eval 'puts self', s4
eval 'puts @ivar', s4


## returning lambda
def n_times(n)
  lambda{|val| n * val}
end
two_times   = n_times(2)
five_times  = n_times(5)
puts two_times.call(5), five_times.call(5)

#puts eval("n", two_times)


## example
def meth1(start, inc)
  start -= inc
  lambda{ start += inc }
end
counter = meth1(10, 5)
puts counter.call
puts counter.call
puts counter.call
