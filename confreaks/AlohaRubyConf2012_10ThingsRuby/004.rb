#!ruby -w
# only single instance, lock to make your script exclusive

DATA.flock(File::LOCK_EX | File::LOCK_NB) or abort 'Already Running.'
trap("INT", "EXIT")

puts "Running..."
loop do
  sleep
end

__END__
DO NOT DELETE: Used to lock.

