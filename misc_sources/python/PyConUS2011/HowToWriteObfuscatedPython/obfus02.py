#!/usr/bin/env python

##DECORATORS

dec = [].append

@dec
def fn(a):
  return a*a

print dec.__self__[0](10)


# write function entirely in decorators


# tail recursion
Y = lambda x:( lambda p:x( lambda a:p(p) (a) ) ) \
    (lambda p:x( lambda a:p(p) (a) ))
@Y
def factorial(recurse):
  def _factorial(n):
    if n <= 1:
      return n
    return n * recurse(n - 1)
  return _factorial

print factorial(0)
print factorial(1)
print factorial(3)
print factorial(10)
print factorial(100)
## print factorial(1024)  # will fail
