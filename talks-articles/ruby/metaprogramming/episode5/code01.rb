#!/usr/bin/env ruby
# inside the Class

class Calc
  def initialize
    @mem = {}
  end

  def percent(before, now)
    key = [before, now]
    @mem[key] ||= calculate_percent(before, now)
  end

  def costly_percent(*bn)
    bn.inject{|b,n| calculate_percent(b, n)}
  end

  private

  def calculate_percent(before, now)
    puts "calculation expense for #{before}:#{now}"
    result = ((now - before) * 100) / Float(before)
    "#{before}:#{now} gives #{result}"
  end
end

calc = Calc.new
puts calc.percent 20, 25
puts calc.percent 20, 25
puts calc.percent 20, 22
puts calc.percent 20, 25
puts calc.percent 20, 22

puts calc.costly_percent 20, 22
puts calc.costly_percent 20, 22
