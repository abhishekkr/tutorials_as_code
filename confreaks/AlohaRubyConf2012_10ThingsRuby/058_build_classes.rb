#!ruby

def build_class(parent, extra_methods = {})
  Class.new(parent) do
    extra_methods.each do |name, result|
      body = result.is_a?(Proc) ? result : -> {result}
      define_method(name, &body)
    end
  end
end

Thingy = build_class(Object,
                     value: 42,
                     dynamic: -> {:called},
                     plus10: ->(n) {n + 10})
thingy = Thingy.new
p thingy
p thingy.value
p thingy.dynamic
p thingy.plus10(90)
