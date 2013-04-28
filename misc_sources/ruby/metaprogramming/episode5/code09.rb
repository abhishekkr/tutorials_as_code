#!/usr/bin/env ruby
# using a DSL

module Memoize
  def remember(meth, &block)
    define_method(meth, &block) # getting original method

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

  remember :percent do |*bn|
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
puts calc.percent 20, 22
puts calc.percent 20, 22
