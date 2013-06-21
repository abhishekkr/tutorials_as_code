#!/usr/bin/env python

print "%.5f" % (41/2)
print "%.5f" % float(41/2)
print "%.5f" % float(41/2.0)
print "H" + "i"
print "H" "i"
print "H" + str(4)
x = 1
print id(x)
print id(1) # 1 is immutable, x is just a tag added to it
x = [1, 2]
print id([1, 2])
print id(x)
print x
x[0] = 0
print id(x)
print x
# True, Truthy
print True == False
if not True == False:
  print bool(x)

if True == False:
  print 'not gonna happen'
elif bool(x) == True:
  print bool(x == True)
  print bool(x) == True
  print x == True
else:
  print 'already happened'

#datetime module
from datetime import datetime

hour = datetime.now().hour
if hour < 12:
  time_of_day = 'morning'
elif hour < 15:
  time_of_day = 'afternoon'
elif hour < 20:
  time_of_day = 'evening'
else:
  time_of_day = 'night'
print 'Good %s, world!' % time_of_day

#help(datetime)
#dir(datetime)
