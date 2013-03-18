#!ruby
# module inclusion

def LimitedUse(limit)
  Module.new do
    define_singleton_method(:included) do |parent|
      count = 0
      parent.public_instance_methods.each do |method|
        define_method(method) do |*args|
          fail 'Over use limit' if (count += 1) > limit
          super(*args)
        end
      end
    end
  end
end

class ShortLived
  include LimitedUse(3)
end

limited = ShortLived.new
puts Array.new(3){ limited.to_s }
begin
  limited.to_s
rescue Exception => err
  p err.message
end
