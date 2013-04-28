#!/usr/bin/env ruby
# define_method

#
## set 1
class Ex
  def start
    def stop # only after start can one instance stop
    end
  end
end

ex = Ex.new
ex.start # now .start gets defined
ex.stop


#
## set 2
class Ex2
  def one(param)
    def one
      "[+] #@value"
    end
    @value = param
  end
end
ex2 = Ex2.new
puts ex2.one(10)
puts ex2.one


#
## define_method
class Ex3
  define_method(:hello){ "[+] yay" }
  define_method(:times2) do |num|
    num * 2
  end

  def self.create_logger(type)
    define_method("log#{type}") do |msg|
      puts "{#{type}} #{msg}"
    end
  end
  Ex3.create_logger 'RB'

  (3..11).each{|num|
    define_method("times#{num}"){|val| val * num }
  }
end
ex3 = Ex3.new
puts ex3.hello, ex3.times2(5)
ex3.logRB ex3.hello
puts ex3.times5(5)


#
## own version of attr_accessor
module Accessor
  define_method(:my_attr_accessor){|*variables|
    variables.each{|variable|
      i_variable = "@#{variable}"
      define_method(variable){ instance_variable_get(i_variable) }
      define_method("#{variable}="){|value| instance_variable_set(i_variable, value) }
    }
  }
end
class Ex4
  extend Accessor

  my_attr_accessor :var1
  my_attr_accessor :var2, :var3, :var4, :var5
end
ex4 = Ex4.new
ex4.var1, ex4.var2, ex4.var3, ex4.var4, ex4.var5 = 100, 1000, 10000, 10, 1
puts ex4.var1, ex4.var2, ex4.var3, ex4.var4, ex4.var5
