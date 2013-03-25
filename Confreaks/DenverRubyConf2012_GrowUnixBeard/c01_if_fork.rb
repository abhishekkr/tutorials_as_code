#!/usr/bin/env ruby

puts "Start: #{Process.pid}"
if Process.fork
  puts "True: #{Process.pid}"
else
  puts "False: #{Process.pid}"
end
puts "Stop: #{Process.pid}"
