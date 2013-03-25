#!ruby

# see how ruby reads your code with comments
puts %x{ruby -e 'puts {is_this_a_block}' --dump parsetree_with_comment}
