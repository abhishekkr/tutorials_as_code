#!/usr/bin/env ruby

# method_added
class Data
  def self.method_added(name)
    puts "yaya I got new method #{name}"
  end

  def fetch; end
  def crunch; end
end


#
## TRACE ME
module TraceCalls
  def self.included(klass)
    puts "Tracing #{klass}..."

    def klass.method_added(name)
      return if @_adding_trace_method
      @_adding_trace_method = true
      puts "Tracing #{self}.#{name}..."
      original_method = "original_#{name}"

      alias_method original_method, name

      define_method(name) do |*args, &block| # block fails in Ruby1.8
        puts "==> Calling #{name} with #{args.inspect}"
        result = send original_method, *args, &block # block fails in Ruby1.8
        puts "<== result = #{result}"
        result
      end
      @_adding_trace_method = false
    end
  end
end
class Ex1
  include TraceCalls

  def meth1(arg1, arg2)
    arg1 + arg2
  end

  def meth2; self; end

  def meth3(arg)
    yield * arg
  end

  def methname=(name) # not with TraceCalls2' result =
    @methname = name
  end

  def <<(name) # not with TraceCalls2' result =
    @methname ||= ''
    @methname = "#{@methname}#{name}"
  end

  def methname; @methname; end
end

ex1 = Ex1.new
puts ex1.meth1(100, 10)
puts ex1.meth2
puts ex1.meth3(100){10}
ex1.methname = 'yes'
ex1.<< 'no'
puts ex1.methname


#
## TraceCalls2 improved (for 1.8 mainly) and some good refactoring
module TraceCalls2
  def self.included(klass)
    puts "Tracing #{klass}..."

    def klass.method_added(name)
      return if @_adding_trace_method
      @_adding_trace_method = true
      puts "Tracing #{self}.#{name}..."
      TraceCalls2.trace_method(self, name)
      @_adding_trace_method = false
    end
  end

  def self.trace_method(klass, name)
    original_method = "original_#{name}"

    klass_body = %{
      alias_method :#{original_method}, :#{name}

      def #{name}(*args, &block) # works with Ruby1.8 
        puts "==> Calling #{name} with \#{args.inspect}"
        result = #{original_method}(*args, &block)
        puts "<== result = \#{result}"
        result
      end
    }

    klass.class_eval klass_body
  end
end
class Ex2
  include TraceCalls2

  def meth3(arg)
    yield - arg
  end
end

ex2 = Ex2.new
puts ex2.meth3(100){10}


#
## TraceCalls3 improved with Memoize way
module TraceCalls3
  def self.included(klass)
    puts "Tracing #{klass}..."
    klass.const_set(:METHODS, {})

    def klass.method_added(name)
      return if @_adding_trace_method
      @_adding_trace_method = true
      @methods ||= {}
      @methods["#{name}"] = instance_method(name)
        p @methods
      puts "Tracing #{self}.#{name}..."
      TraceCalls3.trace_method(self, name)
      @_adding_trace_method = false
    end
  end

  def self.trace_method(klass, name)
    methods = klass.const_get(:METHODS)
    methods["#{name}"] = klass.instance_method(name)

    klass_body = %{
      def #{name}(*args, &block) # works with Ruby1.8 
        puts "==> Calling #{name} with \#{args.inspect}"
        result = METHODS["#{name}"].bind(self).call(*args, &block)
        puts "<== result = \#{result}"
        result
      end
    }

    klass.class_eval klass_body
  end
end
class Ex3
  include TraceCalls3

  def meth3(arg)
    arg - yield
  end

  def methname=(name) # works at Ruby1.9; with TraceCalls' result =
    @methname = name
  end

  def <<(name) # works at Ruby1.9; with TraceCalls' result =
    @methname ||= ''
    @methname = "#{@methname}#{name}"
  end

  def methname; @methname; end
end

ex3 = Ex3.new
puts ex3.meth3(100){10}
ex3.methname = 'yes'
ex3.<< 'no'
puts ex3.methname
