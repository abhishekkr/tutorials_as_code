#!/usr/bin/env ruby
# current class

class Dave
  def meth ; p self ; end
end

class Fred
  class Wilma
    def meth ; p self ; end
  end

  def meth ; p self ; end
end

def meth ; p self ; end

meth
Dave.new.meth
Fred.new.meth
Fred::Wilma.new.meth
