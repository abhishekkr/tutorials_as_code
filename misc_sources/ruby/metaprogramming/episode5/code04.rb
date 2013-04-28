#!/usr/bin/env ruby
# Ghost Class

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

calc = Calc.new

def calc.percent(*args)
  @mem ||= {}
  @mem[args] ||= super
end
puts calc.percent 20, 22
puts calc.percent 20, 22
