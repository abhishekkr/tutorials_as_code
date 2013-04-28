#!/usr/bin/env ruby
# Ghost Class, with generator

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

def memoize2(obj, method)
  ghost = class<<obj; self; end
  ghost.class_eval do
    class_eval %{
      def #{method}(*args)
        @mem ||= {}
        @mem[args] ||= super
      end
    }
  end
end

calc2 = Calc.new
memoize2(calc2, :percent)
puts calc2.percent 20, 22
puts calc2.percent 20, 22
