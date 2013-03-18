#!ruby

def build_module(extra_methods = {})
  Module.new do
    extra_methods.each do |name, result|
      body = result.is_a?(Proc) ? result : -> {result}
      define_method(name, &body)
    end
  end
end

Mixin = build_module(value: 42,
                     dynamic: -> {:called},
                     plus10: ->(n) {n + 10})
thingy = Object.new.extend(Mixin)
p thingy
p thingy.value
p thingy.dynamic
p thingy.plus10(90)
