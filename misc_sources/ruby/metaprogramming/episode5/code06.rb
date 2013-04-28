#!/usr/bin/env ruby
# rewrite method

class Calc

  def percent(*bn)
    bn.inject{|b,n| calculate_percent(b, n)}
  end

  alias_method :_original_percent_, :percent

  private

  def calculate_percent(before, now)
    puts "calculation expense for #{before}:#{now}"
    result = ((now - before) * 100) / Float(before)
    "#{before}:#{now} gives #{result}"
  end
end
class Calc
  def percent(*args)
    @mem ||= {}
    @mem[args] ||= _original_percent_(*args)
  end
end

calc = Calc.new
puts calc.percent 20, 22
puts calc.percent 20, 22
puts calc._original_percent_ 20, 22
