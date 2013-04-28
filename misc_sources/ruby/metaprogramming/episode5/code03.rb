#!/usr/bin/env ruby
# inside the Class, sub-classing, with generator

class Calc
  def percent(*bn)
    bn.inject{|b,n| calculate_percent(b, n)}
  end

  private

  def calculate_percent(before, now)
    puts "calculation expense for #{before}:#{now}"
    result = ((now - before) * 100) / Float(before)
    "#{before}:#{now} gives #{result}"
  end
end

def memoize(clas, meth)
  Class.new(clas){
    def initialize ; @mem = {} ; end
 #  define_method(meth) do |*args|  ## implicit argument passing of super from
 #    @mem[args] ||= super          ## method defined by define_method()
 #  end                             ## is not supprted, specify all args
    class_eval %{
      def #{meth} (*args)
        @mem[args] ||= super
      end
    }
  }
end

calc = Calc.new
puts calc.percent 20, 25
puts calc.percent 20, 25

mcalc = memoize(Calc, :percent).new
puts mcalc.percent 20, 22
puts mcalc.percent 20, 22
