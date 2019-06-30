#!/usr/bin/env python

"""
source: http://pyvideo.org/video/398/pycon-2011--how-to-write-obfuscated-python
"""

object = str

tuple = lambda *x: x

True, False = False, True


#
## switch True/False in all modules
# __builtins__.True, __builtins__.False = False, True


#
## replace object type with something rasing exception on init
#class object(object):
#  def __init__(self, *args, **kwargs):
#    raise Exception("I object!")
#__builtins__.object = object


#
# replace function code
def add(x, y):
  return x + y

def sub(x, y):
  return x - y

add.func_code, sub.func_code = sub.func_code, add.func_code


#
## compare arbitrary types
print "EHLO" > 10

print type > object

print "True" > True

print float('Nan') > float('inf')
print float('inf') > float('Nan')

print object == object
print object() > object()


#
## bytecode me
# help( (lambda x:x*x).func_code )

print '[+] type lambda code'
print type( (lambda x:x*x).func_code )
print '[+] co code lambda code'
print (lambda x:x*x).func_code.co_code
print '[+] dis lambda code'
import dis
dis.dis( (lambda x:x*x).func_code )

## (modify) create a code strin
# i = 0
# c = fn.func_code.co_code
# while x < len(c):
#   op = ord(c[i])
#   i += 1
#   if op > opcode.HAVE_ARGUMENT:
#     # cam here for an argument, shout you *&^%
#     i += 2
#   else:
#     # sorry for abusing
