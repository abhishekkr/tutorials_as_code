#!/usr/bin/env python
"""
defining functions
"""

def foo1():
    """ seeing how locals and globals differ """
    a = 1
    print "Locals : %s" % locals()
    print "Globals: %s" % globals()
    print a # 1 gets printed here and returns nothing so...

def foo2(*args):
    print type(args)
    print "you passed %d arguments" % len(args)

print foo1
print foo1() #... None gets printed here, default return
print locals()
print foo2(1, 2, 3, 4, 5)

def foo3(**kwargs):
    print type(kwargs), kwargs.items()

foo3()
foo3(arg=123)

def foo4(*args, **kwargs):
    print args
    print kwargs.items()

foo4(1, 2, 3, 4, five=5, six=6, seven=7)
