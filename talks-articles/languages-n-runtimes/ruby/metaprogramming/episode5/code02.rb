#!/usr/bin/env ruby
# inside the Class, sub-classing

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

class MemCalc < Calc
  def initialize
    @mem = {}
  end
  def percent(before, now)
    key = [before, now]
    @mem[key] ||= calculate_percent(before, now)
  end

end

calc = Calc.new
puts calc.percent 20, 25
puts calc.percent 20, 25

mcalc = MemCalc.new
puts mcalc.percent 20, 22
puts mcalc.percent 20, 22
