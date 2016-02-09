#!/usr/bin/env python
#Python 2.7

""" [Python Epiphanies](http://www.youtube.com/watch?v=Pi9NpxAvYSs) from NextDayVideo """

#### Dict and Namespaces
##### list maps int (0..len(list)-1) -> objects

month_number_to_name = [None, 'Jan', 'Feb', 'Mar']
print( month_number_to_name[1] )

month_name_to_number = {'Jan': 1, 'Feb': 2}
print( month_name_to_number['Jan'] )

_namespace = {}
print( _namespace )

_namespace['a'] = 7
print( _namespace )
_namespace['a'] = 8
print( _namespace )
_namespace['8'] = 'Sep'
print( _namespace )

print( dir() )

a = 17
print( dir() )

del a
print( dir() )


#### Objects and Variables
i = 1
j = 1
k = '1'
l = '1'

print(type( 1 ))
print(type( i ))
print(type( k ))
print(type( (1, 2) ))
print(type( [1] ))
print(type( {'a': 1} ))

print("id of 1: %s" % id( 1 ))
print("id of 1: %s" % id( 1 ))
print(id( '1' ))
print("id of i: %s" % id( i ))
print("id of j: %s" % id( j ))
print( id(i) == id(j) )
print( id(k) == id(l) )

print( '1'.__class__ )
print( '1'.__class__.__bases__ )
print( '1'.__class__.__bases__[0].__bases__ )
print( i.__class__ )
print( i.__class__.__bases__ )
print( i.__class__.__bases__[0].__bases__ )

import inspect
print( inspect.getmro(type(i)) )

print( int.__truediv__ )

"""
    namespace : mapping from name to objects
    assignment is namespace operation, not variable/object operation
    scope is section of code with namespace accessible, elsewhere dot notation
"""

global x
x = 1
y = 2
class Any:
  def any():
    x = 2
    y = 3
    global z
    z = 4
  def anywho():
    y = 1
    print( locals().keys() )
    print( globals().keys() )
Any.any()
print("x: %s; y: %s, z: %s" % (x, y, z))
Any.anywho()


#### Modules
import pprint
from pprint import pprint as prettyprint
prettyprint( pprint.pprint is prettyprint )

module_name = 'string'
string_module = __import__(module_name)
prettyprint( string_module.ascii_uppercase )

print( dir() )

""" creating module load files """
import os
if not os.path.exists('module1'):
    os.makedirs('module1')
if not os.path.exists('module1/__init__.py'):
    fyl = open('module1/__init__.py', 'w')
    fyl.close()
if not os.path.exists('module1/file1.py'):
    fyl = open('module1/file1.py', 'w')
    fyl.write("#!/usr/bin/env python\n")
    fyl.write("def attribute1():\n")
    fyl.write("  return 'abcdef'")
    fyl.close()

from module1 import file1
print( dir() )
import module1
print( dir(module1.file1) )



#### Functions
def f(arg1, arg2, kwarg1='kw1', kwarg2='kw2', *args, **kwargs):
  """a function with regular and keyword arguments"""
  print('arg1: {0}, arg2: {1}, kwarg1: {2}, kwarg2: {3}'
      .format(arg1, arg2, kwarg1, kwarg2))
  if args:
    print('args:', str(args))
  if kwargs:
    print('kwargs:', kwargs)
  print()

print(f, f.__name__)
print('f' in dir())
f.__name__ = 'g'
print(f, f.__name__)
print('f' in dir())
print(f.__dict__)
f.foo = 'bar'
print(f.__dict__)

f(1, 2)
f(1, 2, 3)
f(1, 2, kwarg2=4)
f(1, 2, 3, 4, 5, 6, 7, 8, 9)
f(1, 2, 3, 4, 5, 6, 7, 8, {'a': 1, 'b': 2})
f(1, 2, 3, 4, 5, 6, 7, keya='a', keyb='b')



#### Lists are mutable strings are not
old_s = s = 'hello'
print(old_s, s, s is old_s, id(s), id(old_s))
s = s + ' there'
print(old_s, s, s is old_s, id(s), id(old_s))
old_s = s = 'hello'
print(old_s, s, s is old_s, id(s), id(old_s))
s += ' there'
print(old_s, s, s is old_s, id(s), id(old_s))

old_m = m = [1]
print(old_m, m, m is old_m, id(m), id(old_m))
m = m + [2]
print(old_m, m, m is old_m, id(m), id(old_m))
old_m = m = [1]
print(old_m, m, m is old_m, id(m), id(old_m))
m += [3]
print(old_m, m, m is old_m, id(m), id(old_m)) #diff than += string



##### str.__iadd__ copies but list.__iadd__ mutates
import codeop, dis
dis.dis(codeop.compile_command('m = list(); m += [4]'))
dis.dis(codeop.compile_command('m = string; m += "t"'))



#### How are parameters passed? Always by reference.
def nu_str(val):
  print('Before:', val)
  val += ' there'
  print('After:', val)

def nu_lst(val):
  print('Before:', val)
  val += [10]
  print('After:', val)

a = 'hello'
nu_str('hello')
nu_str(a)
print( a ) # strings immutable
b = [1, 2, 3, 4, 5]
nu_lst(b)
print( b ) # lists mutable



#### Decorators
def square(n):
  return n * n

print( square(2) )
print( square(22) )

def stringify(func):
  def convert_to_str(n):
    print('Called convert_to_str({})'.format(n))
    return str(func(n))
  print('called stringify({})'.format(func))
  return convert_to_str

square_str = stringify( square )
print(square_str)
print(square_str(10))
print(type(square_str(10)))

@stringify
def cube(n):
  return n * n * n

print( cube(10) )
print( type(cube(10)) )



##### changing local namespace
def value(f):
  return f()

@value
def x():
  return 1

print(x)
print(type(x))
print(type(value))



#### The Class Statement
""" are class & metaclasses in python a object? yes """

"""
instances created by calling ClassName() or ClassName(params)
ClassName.__init__(<new object>,...) is called automatically passing in new object already created by __new__
"""
class Num(object):
  def __init__(self, amount):
    self.amount = amount

  #
  def add(self, value):
    return self.amount + value

print(Num)
print(Num.__init__)
print(Num.add)
print(dir(Num))
num2 = Num(2)
print(num2.amount)
print(num2.__init__)
print(dir(num2))
print(num2.__dict__)
print(num2.add(3))
print(Num.add(num2, 3))

def double_amount(self, amount):
  self.amount = 2 * amount

Num.__init__ = double_amount
print(Num.__init__)
"""print(help(Num.__init__))"""

num4 = Num(2)
print(num4.add(5))

print(num2.__init__)
print(num4.__init__)

def multiply_by(num, value):
  return num.amount * value

num4.mul = multiply_by

print(num4.mul)
print(num4.mul(num4, 5))

Num.mul = multiply_by
num10 = Num(5)
print(num10.mul)
print(num10.mul.__self__)
print(num10.mul(5))
print(dir(num10.mul))
"""
im_self is __self__
im_func is __func__
but __class__ is <type 'instancemethod'>
"""



##### the 'type' function for classes
"""
The clas statement is just a way to call a function, take the result & put it into a namespace.
type(name, base, dict) is function that gets called when a class statement is used to create a class
"""
print( type.__doc__ )

DoubleNum = type(
  'DoubleNum',
  (object,),
  {'__init__': double_amount,
  'mul': multiply_by,}
)
num6 = DoubleNum(3)
print(type(num6))
print(num6.__class__, num6.__dict__, num6.mul(5))



##### __metaclass__
"""
dynamic call to type is what 'class' triggers
When class def is read, if __metaclass__ is def then callable assigned to it will be called instead of type()
__metaclass__ can be any callable accepting args for name, bases, dicts
Upon class creation the callable is used instead of calling type()
[Language Ref Section 3.4.3]
Synatxes different for Py2 and Py3
"""

def one(name, bases, attrs):
  return 1

class x: #py2.7 syntax
  __metaclass__ = one  # call this to create class

"""at Py3
class x(metaclass=one):
"""

print(x)
print(type(x))



def two(klass):
  return 2

@two
class y(object):
  pass

print(y)
print(type(y))



#### Statndard Class Methods
"""
* __new__
* __init__
* __del__
* __repr__
* __str__
* __format__
* _getattr__
* __getattribute__
* __setattr__
* __delattr__
* __call__
* __dir__
* __len__
* __getitem__
* __missing__
* __setitem__
* __delitem__
* __contains__
* __iter__
"""



#### Iterators
"""
##### iter(foo)
* checks for foo.__iter__() and calls if it exists
* else checks for foo.__getitem__(), calls it starting at Zero and handles IndexError by raising StopIteration
"""

class MyList(object):
  def __init__(self, sequence):
    self.items = sequence

  def len(self):
    return self.items.__sizeof__

  def __getitem__(self, key):
    print('called __getitem__({})'.format(key))
    return self.items[key]

m = MyList(['a', 'b', 'c'])
print(m.__getitem__(0))
print(m.__getitem__(1))

i = iter(m)
print( dir(i) )
print( i.__next__() )
print( i.__next__() )
print( i.__next__() )

print("now for")
for item in m:
  print(item)

print(m.items)
m.items.reverse()
print(m.items)
print(list(m))


##### generators

mi = [2 * i for i in range(5)]
print("mi is %s with type %s" % (mi, type(mi)))
mj = (2 * i for i in range(5))
print("mj is %s with type %s" % (mj, type(mj)))
print(hasattr(mj, '__next__'))
print(list(mj))
print(list(mj))
for i in (2 * i for i in range(5)):
  print(i)

def list123():
  yield 0
  yield 2
  yield 4
  yield 6
  yield 8

mk = list123()
print("mk is %s with type %s" % (mk, type(mk)))
print(list(mk))

def even(limit):
  for i in range(0, limit, 2):
    print('yielding', i)
    yield i
  print('done loop, falling out')

ml = even(10)
print("ml is %s with type %s" % (ml, type(ml)))
print(list(ml))



def paragraphs(lines):
    result = ''
    for line in lines:
        if line.strip() == '':
            yield result
            result = ''
        else:
            result += line
    yield result

print(list(paragraphs(open('../README.md'))))



#### operators

import operator

print( 7 + 3 )
print( operator.add(7, 3) )

expr = '7+3'
lhs, op, rhs = expr
print('ls: %s, op: %s, rhs: %s' % (lhs, op, rhs))
print('ls: %s, op: %s, rhs: %s' % (type(lhs), type(op), type(rhs)))
lhs, rhs = int(lhs), int(rhs)
print('ls: %s, op: %s, rhs: %s' % (type(lhs), type(op), type(rhs)))
ops = {
    '+': operator.add,
    '-': operator.sub,
}

print( ops[op](lhs, rhs) )

def calc(expr):
    lhs, op, rhs = expr
    lhs, rhs = int(lhs), int(rhs)
    result = ops[op](lhs, rhs)
    return result

print(calc('7-3'))
print(dir(operator))

ops['/'] = operator.floordiv
print(calc('7/3'))

ops['/'] = operator.truediv
print(calc('7/3'))



#### Unpacker

class Unpacker:
    slices = {
        'header': slice(0, 3),
        'trailer': slice(12, 18),
        'middle': slice(6, 9)
    }

    def __init__(self, record):
        self.record = record

    def __getattr__(self, attr):
        print('>>>>>>%s' % attr)
        if attr in self.slices:
            return self.record[self.slices[attr]]
        return "'Unpacker' object has no attribute '{}'".format(attr)

u = Unpacker('abcdefghijklmnopqrstuvwxyz')
print( u.header )
print( u.trailer )
print( u.middle )
print( u.headers )

##### Slice

print(u.record[::-1])
print(u.record[::5])
print(u.record[::10])
print(u.record[5:10] == u.record[ slice(5, 10) ])
print(u.record[ slice(5, 10) ])
