#!ruby
#ERb

require 'erb'

class Name
  def initialize(first, last)
    @first, @last = first, last
  end

  attr_reader :first, :last
  extend ERB::DefMethod
  def_erb_method("full", "full_name.erb")
  def_erb_method("last_first", "last_name_first.erb")
end

abc = Name.new('Abc', 'Xyz')
puts abc.full
puts abc.last_first
