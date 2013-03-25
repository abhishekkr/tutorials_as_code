#!ruby

# pass modified arguments/block up

class Parent
  def show(*args, &block)
    puts "Args:", args, "Block:", block
  end
end

class Child < Parent
  def show(a, b, c)
    a.upcase!
    b = 'b0o'
    super(&nil)
  end
end

Child.new.show('a', :b, :c) { puts ">>>" }
