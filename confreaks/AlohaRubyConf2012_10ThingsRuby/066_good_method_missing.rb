#!ruby
#

class Greeter
  GREETING_REGEX = /\Aaloha_\w+\z/

  def method_missing(method, *args, &block)
    if method =~ GREETING_REGEX
      "Aloha #{method.to_s.split("_")[1..-1].map(&:capitalize).join(' ')}"
    else
      super
    end
  end

  def respond_to_missing?(method, include_private = false)
    method =~ GREETING_REGEX
  end
end

greeter = Greeter.new
p greeter.respond_to?(:aloha_jeg_ii)
puts greeter.method(:aloha_jeg_ii).call
