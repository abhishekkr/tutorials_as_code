#!ruby

# watch ruby's parser work
puts %x{ruby -e 'puts {a_block}' --dump yydebug}
