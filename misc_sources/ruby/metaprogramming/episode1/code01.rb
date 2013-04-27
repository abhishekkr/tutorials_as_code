#!/usr/bin/env ruby
# object = state + behavior

require 'date'

class Person

  def initialization(name, dob)
    @name = name
    @dob  = Date.parse(dob)
  end

  def age; end

  def marry(someone); end
end

f = Person.new 'F', '9999-12-31'
p f
