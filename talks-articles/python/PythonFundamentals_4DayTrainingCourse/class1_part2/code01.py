#!/usr/bin/env python
## advanced container types: lists

import datetime
print datetime

from datetime import datetime as dt
print dt.now()

x = 10
if 5 < x < 15:
  if x == 10:
    print x

import sys
print sys.argv, len(sys.argv)

dict_ = {'name': __name__}    #mutable
list_ = [dict_, sys.argv, 0]  #mutable
tupl_ = (dict_, list_)        #immutable
print dict_['name']
print list_[-1]
print tupl_[0]
print tupl_[-1]
list_[0] = 100
print list_[0]
print list_.index(0)
print

# for
for elem in list_:
  print "\telem: %s" % elem
print

for key, elem in enumerate(list_):
  print "\t%s: %s" % (key, elem)
print

for key in dict_:
  print "\t%s: %s" % (key, dict_[key])
print

# __builtin__, keyword
import __builtin__ #just for next line, else methods available
print dir(__builtin__)
print
import keyword # keyword
print dir(keyword.kwlist)
print

# while
while len(list_):
  print list_
  list_.pop()
print

# range
list_ = range(0, 10)
print list_, list_[3:7], list_[5:], list_[7:15], list_[7:-1], list_[:]
print list_[0:7:2], list_[1:7:3], list_[1:7:-1]
print

# list
list1_ = list_
list2_ = list_[:]
list1_[0] = 100
list2_[0] = 10
print list_, list1_, list2_
list2_.sort()
print list2_
list2_.reverse()
print list2_
list2_.append(11)
list2_.extend([12, 13, 14])
print list2_
list2_.insert(1, 100)
print list2_
if 100 in list2_:
  if list2_.count(14) > 0:
    if 'a' in 'abcde':
      print "OK"
print list()
print list("abcde")
print

# String magic
abc = 'abc,def.ghi-jkl.mno,pqr'
xyz = abc.split('.')
print abc, xyz, '_'.join(xyz)
print

def prog():
  """gimme a doc string, first line of any namespace gets assigned to $.docstring"""
  print "ABCDE" \
      "FGHIJ" \
      "KLMNO"
  x = r'abc\tde'
  y = 'abc\tde'
  print x, "<>", y
  print 'num: %.5f' % (25/3.0)
prog()
print prog.__doc__
print prog.__doc__[7:19]
