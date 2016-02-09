#!ruby
# 1.9
#
puts 'daemonizing...'
Process.daemon

loop do
  sleep
end
puts '...'
