# -*- coding: utf-8 -*-
# reading files

from sys import argv

script, filename = argv

txt = open(filename)

print "Content of %r" % filename
print "+" * 25
print txt.read()
print "#" * 25
print txt.read()

txt_again = open(filename)
print "=" * 25
print txt_again.read()

## if you wanna be sure exactly when/where it closes
txt.close()
txt_again.close()
