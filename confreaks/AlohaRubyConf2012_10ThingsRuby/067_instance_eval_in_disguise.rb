#!ruby
# sinatra uses it, faster than real instance_eval

class Abc
  def self.peek_inside(&block)
    define_method(:peek, &block)
    method = instance_method(:peek)
    remove_method(:peek)
    method
  end

  def initialize(secret)
    @secret = secret
  end
end

magic_key = Abc.peek_inside {@secret}
meaning   = Abc.new(42)
other     = Abc.new(:other)

p magic_key.bind(meaning).call
p magic_key.bind(other).call
