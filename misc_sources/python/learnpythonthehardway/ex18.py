# -*- coding: utf-8 -*-
# names, variables, code functions

def print2(*args):
    print "1: %s\n2: %s" % args

def print2again(arg1, arg2):
    print "%s and %s" % (arg1, arg2)

def print1(arg1):
    print "%s is sized %d" % (arg1, len(arg1))

def print0():
    print "no no no, nothing passed"

print2('python', 'pro')
print2again('python', 'pro')
print1('python')
print0()
