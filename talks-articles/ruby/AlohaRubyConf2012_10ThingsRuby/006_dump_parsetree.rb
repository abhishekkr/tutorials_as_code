#!ruby

# see how ruby reads your code
puts %x{ruby -e 'puts {is_this_a_block}' --dump parsetree}
