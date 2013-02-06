#!ruby

# pass modified arguments/block up

class Parent
  def show(*args, &block)
    puts "Args:", args
  end
end

class Child < Parent
  def show(a, b, c)
    a.upcase!
    b = 'b0o'
    super
  end
end

Child.new.show('a', :b, :c)
