# -*- coding: utf-8 -*-
# what was that?

var01 = '\\ \' \nabc\tdef\n'
var02 = 'abcdef\rghi\n'
var03 = "\u1234 | \U12345678 | \" | \v"
var04 = " \a | \222 | \xab |  xyz\b "

print "%r \n %r \n %r \n %r" % (var01, var02, var03, var04)
print "+" * 25
print "%s \n %s \n %s \n %s" % (var01, var02, var03, var04)
print '''
yeah this will work too
'''
