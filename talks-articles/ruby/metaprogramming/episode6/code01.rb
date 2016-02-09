#!/usr/bin/env ruby

class Module
 #def include(*modules)
 #  modules.reverse_each do |mod|
 #    mod.append_self(self)
 #    mod.included(self) # not early 1.8 days
 #  end
 #end
 #def included(mod); end

  def included(mod)
    puts "#{self} included"
  end
end
class SomeClass
  include Comparable

  def <=>(other)
    puts "comparing..."
    0
  end
end

s = SomeClass.new
s < 123
