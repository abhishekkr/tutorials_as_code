#!ruby

class LimitedUse < Module
  def initialize(limit)
    @limit = limit
    super() do
      define_singleton_method(:included) do |parent|
        count = 0
        parent.public_instance_methods.each do |method|
          define_method(method) do |*args|
            fail 'Over use limit' if (count += 1) > limit
            super(*args)
          end
        end #parent._
      end #define_.
    end #super_
  end

  def to_s; "LimitedUse.new(#{@limit})" end
end

class ShortLived
  include LimitedUse.new(3)
end
p ShortLived.ancestors

limited = ShortLived.new
puts Array.new(3){ limited.to_s }
begin
  limited.to_s
rescue Exception => err
  p err.message
end
