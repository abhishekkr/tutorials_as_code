#!ruby
# ruby as glue lang

require 'shellwords'

p Shellwords.shellwords("one 'two' 'a longer three'")
p Shellwords.shellwords('one "two" "a longer three"')
p Shellwords.shellwords('"escape \"quote\" char"')
p Shellwords.shellwords('escape\ spaces')
p Shellwords.shellwords(%Q{'back to'" back quoting"})
p Shellwords.shellwords("two words")
p Shellwords.shellwords('"quotes" included')
p Shellwords.shelljoin(["two words", '"quotes" included'])
