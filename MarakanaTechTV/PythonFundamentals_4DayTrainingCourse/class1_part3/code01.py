#!/usr/bin/env python
## more container types: tuples, dicts, sets

x = (1, 2) # an immutable list
print x

var1 = (1)
var2 = (1,)
print "type of (1) is %s" % type(var1)
print "type of (1,) is %s" % type(var2)

x = tuple()
print x

x = tuple(list('abcde'))
print x
x = tuple([1, 2])
print x

a, b = x #tuple unpacking
print a
print b

x = {'a' : 'A', 'b': 'B'}
if 'b' in x:
  print x
x = dict(a='A', b='B', c='C', d='D')
print x
print x.items()
print x.keys()
print x.values()
for k, v in x.items():
  print k, v
print "'in' works for keys in dict"
if 'A' in x:
    print "A in %s" % x
if 'a' in x:
    print "a in %s" % x

y = raw_input('enter anything or just <enter>: ')
print y

x = dict([(1,2), (3,4), (5,6)])
print x

print x.get(1)
print x.get(10, None)

import pprint
pp = pprint.PrettyPrinter(indent=4, width=5)
pp.pprint(x)

a = 'abcdE fghiJ'
print a.upper()
print a.title()
print a.lower()


a = set([1, 10, 100])
b = set([5, 10, 15])
print a, b
print a.intersection(b), a.difference(b), a.union(b)
c = zip(a, b)
print type(c), c
print dict(c)
