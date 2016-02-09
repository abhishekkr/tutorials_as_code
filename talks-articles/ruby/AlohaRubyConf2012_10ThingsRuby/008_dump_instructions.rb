#!ruby

# see how ruby reads as machine instructions
puts %x{ruby -e 'puts {is_this_a_block}' --dump insns}
