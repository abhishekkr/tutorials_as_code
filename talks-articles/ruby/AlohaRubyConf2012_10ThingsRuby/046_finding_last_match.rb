#!ruby

exp = '40 + 2 = 42'

p exp[exp.rindex(/\d+/)..-1]
p exp[exp.rindex(/\b\d+/)..-1]

p exp[exp.index(/\d+/)..3]
