#!ruby -w
# Dirty trick to carry data with code

pos = DATA.pos
lst = DATA.readlines

if ARGV.empty?
  puts lst.shift
else
  lst.push(*ARGV)
end

DATA.reopen(__FILE__, 'r+')
DATA.truncate(pos)
DATA.seek(pos)
DATA.puts lst

__END__
data with source
