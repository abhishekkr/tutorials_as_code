# -*- coding: utf-8 -*-
# parameters, unpacking, variables
## run this script with EXACT 3 parameters

from sys import argv

itself, arg1, arg2, arg3 = argv   # unpacking

print "Script name is %s" % itself
print "First arg passed was %s" % arg1
print "Second arg passed was %s" % arg2
print "Third arg passed was %s" % arg3

from sys import copyright
print copyright, "\n\n"

from sys import getsizeof
print "%r" % getsizeof(argv)
