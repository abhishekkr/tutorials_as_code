#!ruby -w

at_exit do
  if $! and not [SystemExit, Interrupt].include? $!.class
    puts "*"*10
    exec "ruby #{File.expand_path(__FILE__)}"
  end
end

loop do
  left, right = Array.new(2) { rand(-10..10) }
  operator    = %w[+ - * /].sample
  puts "#{left} #{operator} #{right} EQUALS"
  puts "\t\t #{left.send(operator, right)}"
end
