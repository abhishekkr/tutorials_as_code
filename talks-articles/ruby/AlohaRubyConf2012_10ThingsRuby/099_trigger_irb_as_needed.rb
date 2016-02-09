#!ruby
#Trigger IRB as Needed using

require 'irb'
#IRB.start

def my_program_context
  @my_program_context ||= Struct.new(:value).new(40)
end

trap(:INT) do
  IRB.start
  trap(:INT, 'EXIT')
end

loop do
  puts "Current value: #{my_program_context.value}"
  sleep 1
end
