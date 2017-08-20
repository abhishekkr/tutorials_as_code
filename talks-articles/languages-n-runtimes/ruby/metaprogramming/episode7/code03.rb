#!/usr/bin/env ruby

#
## TraceCalls go tracing existing methods
module TraceCalls
  def self.included(klass)
    puts "Tracing #{klass}..."
    klass.const_set(:METHODS, {})
    klass.instance_methods(false).each do |meth| #all class methods not parents
      trace_method(klass, meth)
    end

    def klass.method_added(name)
      return if @_adding_trace_method
      @_adding_trace_method = true
      @methods ||= {}
      @methods["#{name}"] = instance_method(name)
        p @methods
      puts "Tracing #{self}.#{name}..."
      TraceCalls.trace_method(self, name)
      @_adding_trace_method = false
    end
  end

  def self.trace_method(klass, name)
    methods = klass.const_get(:METHODS)
    methods["#{name}"] = klass.instance_method(name)

    klass_body = %{
      def #{name}(*args, &block) # works with Ruby1.8 
        puts "==> Calling #{name} (\#{args.inspect})"
        result = METHODS["#{name}"].bind(self).call(*args, &block)
        puts "<== result = \#{result}"
        result
      end
    }

    klass.class_eval klass_body
  end
end

class Time
  include TraceCalls
end

puts Time.new, (Time.new + 3600), "\n\n"

#
## TraceCalls supressing self calls on the go
module TraceCallsRev2
  def self.included(klass)
    puts "Tracing #{klass}..."
    klass.const_set(:METHODS, {})
    supress_tracing do
      klass.instance_methods(false).each do |meth| #all class methods not parents
        trace_method(klass, meth)
      end
    end

    def klass.method_added(name)
      return if @_adding_trace_method
      @_adding_trace_method = true
      @methods ||= {}
      @methods["#{name}"] = instance_method(name)
        p @methods
      puts "Tracing #{self}.#{name}..."
      TraceCallsRev2.trace_method(self, name)
      @_adding_trace_method = false
    end
  end

  def self.supress_tracing
    old_value = Thread.current['supress tracing']
    Thread.current['supress tracing'] = true
    yield
  ensure
    Thread.current['supress tracing'] = old_value
  end

  def self.can_trace?
    !Thread.current['supress tracing']
  end

  def self.trace_method(klass, name)
    methods = klass.const_get(:METHODS)
    methods["#{name}"] = klass.instance_method(name)

    klass_body = %{
      def #{name}(*args, &block) # works with Ruby1.8
        if TraceCallsRev2.can_trace?
          TraceCallsRev2.supress_tracing do
            puts "==> Calling #{name} (\#{args.inspect})"
          end
        end
        result = METHODS["#{name}"].bind(self).call(*args, &block)
        puts "<== result = \#{result}" if TraceCallsRev2.can_trace?
        result
      end
    }

    klass.class_eval klass_body
  end
end


class String
  include TraceCallsRev2
end
'dog' + 'cat'

class One
  include TraceCallsRev2
  def one
    t = Two.new
    t.two
  end
end
class Two
  include TraceCallsRev2
  def two
    100
  end
end

class Array
  include TraceCallsRev2
end
abc = %w{a b c}
puts abc.size
