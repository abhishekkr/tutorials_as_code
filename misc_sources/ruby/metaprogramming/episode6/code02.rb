#!/usr/bin/env ruby
#some hooks
#
# Method Related Hooks:
#   method_missing,
#   method_added, singleton_method_added
#   method_removed, singleton_method_removed
#   method_undefined, singleton_method_undefined
#
# Class & Module Hooks:
#   inherited, append_features, included, extend_object, extended,
#   initialize_copy, const_missing
#
# Marshaling Hooks:
#   marshal_dump, marshal_load
# Coercion Hooks:
#   coerce, induced_from, to_<xxx>
#

# hook: inherited, method_missing
class Parent
  def self.inherited(klass)
    puts "#{self} was inherited by #{klass}"
  end

  def self.method_missing(meth)
    puts "#{meth} not found for #{self} :("
  end
end

class Child < Parent

  def method_missing(meth)
    puts "#{meth} not found for #{self} :("
  end
end

class StepChild < Parent
end

c = Child.new
c.study
Child.what


#
## making a point for usage of inherited
class Struct
  @all_my_kids = []
  class <<self; attr_reader :all_my_kids; end
  def self.inherited(klass)
    @all_my_kids << klass
  end
end
TheGood = Struct.new(:name, :age, :goodness)
TheBad  = Struct.new(:name, :age, :badness)
TheUgle = Struct.new(:name, :age, :ugliness)
puts Struct.all_my_kids
