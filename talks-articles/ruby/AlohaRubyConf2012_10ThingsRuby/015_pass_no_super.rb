#!ruby

# pass no arguments/block up

class Parent
  def show(*args, &block)
    puts "Args:", args
    block.yield
  end
end

class Child < Parent
  def show(a, b, c)
    super()
  end
end

Child.new.show(:a, :b, :c){puts ">>>"}
