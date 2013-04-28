#!/usr/bin/env ruby
# rewrite using module

module Memoize
  def remember(meth)
    original = "_original_#{meth}_"
    alias_method original, meth
    mem ||= {}
    define_method(meth) do |*args|
      mem[args] ||= send original, *args
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
puts calc._original_percent_ 20, 22
