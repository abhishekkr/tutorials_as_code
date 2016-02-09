#!/usr/bin/env ruby

#
## const_missing
begin
  puts ">> #{ABCDE}"
rescue Exception => e
  puts e
end

class Module
  def const_missing(name)
      "Missing: #{name} :("
  end
end
puts ">> #{U0123}"
puts ">> #{U3210}"
puts ">> #{ABCDE}"

class Module
  #original_const_missing = instance_method(:const_missing)
  original_const_missing = instance_method(:const_missing)

  define_method(:const_missing) do |name|
    if name.to_s =~ /^U([0-9A-Fa-f]{4})$/
      [$1.to_i(16)].pack("U*")
    else
      original_const_missing.bind(self).call(name)
    end
  end
end
puts ">> #{U0123}"
puts ">> #{U3210}"
puts ">> #{ABCDE}"


#
## localized
class ABCDE
  def self.const_missing(name)
    puts "~~~~~~no #{name}"
    super
  end
end
puts ABCDE::FG
puts FGH


#
## Constant-ine
class Color
  def self.const_missing(name)
    puts "constantizing #{name}"
    const_set(name, new)
  end
end
p Color::Red, Color::Orange
p Color::Red == Color::Orange
p Color::Blue == Color::Blue
