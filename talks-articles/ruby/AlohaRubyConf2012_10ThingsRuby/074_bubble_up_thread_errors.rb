#!ruby
# dbg threaded code, see where it went wrong

Thread.abort_on_exception = true
Thread.new do
  fail 'Ooopps.'
end

begin
  loop do
    sleep
  end
rescue Exception => err
  puts err.backtrace
end
