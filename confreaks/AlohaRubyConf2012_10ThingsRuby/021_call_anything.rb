#!ruby

# new call anything syntax

class Callable
  def call
    :my
  end
end

p -> { :lambda }.()
p [ ].method(:class).()
p Callable.new.()
