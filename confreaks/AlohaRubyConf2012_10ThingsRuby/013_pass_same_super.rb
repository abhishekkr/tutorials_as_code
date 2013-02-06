#!ruby

# pass same arguments/block up

class Parent
  def show(*args, &block)
    puts "Args:", args, "Block:", block
    block.yield
  end
end

class Child < Parent
  def show(a, b, c)
    super
  end
end

Child.new.show(:a, :b, :c) { puts ">>> #{ENV['USER']}" }
