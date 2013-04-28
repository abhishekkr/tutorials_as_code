#!/usr/bin/env ruby
# rewrite using method binding

module Memoize
  def remember(meth)
    original_method = instance_method(meth)
    mem ||= {}
    define_method(meth) do |*args|
      bound_method    = original_method.bind(self)
      mem[args] ||= bound_method.call(*args)
    end
  end
end

class Calc
  extend Memoize

  def percent(*bn)
    bn.inject{|b,n| calculate_percent(b, n)}
  end

  remember :percent

  private

  def calculate_percent(before, now)
    puts "calculation expense for #{before}:#{now}"
    result = ((now - before) * 100) / Float(before)
    "#{before}:#{now} gives #{result}"
  end
end

calc = Calc.new
puts calc.percent 20, 22
puts calc.percent 20, 22
