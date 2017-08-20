#!/usr/bin/env python

#Tail Call improved

def call_on_self(fn):
  return fn(fn)

def thunkify(fn):
  def _thunkify(*args, **kwargs):
    return lambda :fn(*args, **kwargs)
  return _thunkify

def runner(fn):
  def _runner(*args, **kwargs):
    _fn = fn(*args, **kwargs)
    while callable(_fn):
      _fn = _fn()
    return _fn
  return _runner

def Y(x):
  @runner
  @call_on_self
  def _y(p):
    @x
    @thunkify
    def __y(*args, **kwargs):
      return call_on_self(p)(*args, **kwargs)
    return __y
  return _y

def factorial(n):
  @Y
  def fact_iter(recurse):
    def _fact_iter(_n, total):
      if _n <= 1:
        return total
      return recurse(_n-1, total*_n)
    return _fact_iter
  return fact_iter(n, 1)

print factorial(0)
print factorial(1)
print factorial(3)
print factorial(10)
print factorial(100)
print factorial(1024)
print factorial(1024 * 1024)
