#!/usr/bin/env ruby

# hook method: included

module Persist
  module ClassMethods
    def find(*params)
      puts "Looking...#{params.join(', ')}"
    end
  end
  def self.included(klass)
    puts "yaya #{klass} included #{self}"
    klass.extend ClassMethods
  end
end
class Data
  include Persist
end
class BigData < Data
end

Data.find(1)
BigData.find(0,1,2,3,4,5,6,7,8,9)
