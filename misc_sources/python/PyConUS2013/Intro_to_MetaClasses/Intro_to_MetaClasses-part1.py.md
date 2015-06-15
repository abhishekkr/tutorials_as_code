## Python3 & MetaProgramming Part.1(of3)
* David Beazley

``` python -i $script ```

### debug via decorators

import sys

```
"""debugly.py"""

from functools import wraps

def debug(funk):
  msg = funk.__qualname__
  @wrap(funk)
  def wrapper(*args, **kwargs):
    print(func.__name__)
    print(msg)
    return func(*args, **kwargs)
  return wrapper

funk = debug(funk)
```
---


### debug entire Class

```
def debugmethods(cls):
  #cls is a class
  for key, val in vars(cls).items():
    if callable(val):
      setattr(cls, key, debug(val)) # debug() from above example

  return cls

---

@debugmethods
class Spam:
  def grok(self):
    pass
  def bar(self):
    pass
  @classmethod
  def clmeth1(cls): ## doesn't work with classmethods and staticmethods
    pass
  @staticmethod
  def statmeth1(): ## doesn't work with classmethods and staticmethods
    pass
```

* variation: ddebug access
```
def debugattr(cls):
  orig_getattribute = cls.__getattribute__
  def __getattribute__(self, name):
    print('Get:', name)
    return orig_getattribute(self, name)
  cls.__getattribute__ = __getattribute__
  return cls
```
---


#### Class Deconstructed

* consider a class
```
class English(Language):
  def __init__(self, region):
    self.region = region
  def greet():
    if self.region in ["UK","OZ"]:
      return "How you going?"
    elif self.region in ["US"]:
      return "How you doing?"
    else:
      return "How do you do?"
```

* its components
> * Name ("English")
> * Base Class (Language, )
> * Functions(\_\_init\_\_, greet)


---

### MetaClasses

#### Class ~= Type

```
class Car(object):
    foo = 'bar'
```

can also be prepared as
```
Car2 = type('Car', (object,), {'foo': 'bar'})

print("\nFor Class: Car")
print(type(Car))
print(dir(Car))

print("\nFor Type: Car2")
print(type(Car2))
print(dir(Car2))

print(type(type))
print("---------------\n")
```


```
MyClass = type('MyClass', (object,), {"foo2": "bar2"})
```

```
class type(type):
    @classmethod
    def __call__(metaclass, name, bases, attributes):
        klas = type.__new__(metaclass, name, bases, attributes)
        metaclass.__init__(klas, name, bases, attributes)
        return klas

```
---

#### Replacing MetaType instead of Type for a Class

```
#!python3
class mytype(type):
    def __new__(cls, clsname, bases, clsdict):
        print(">>>>>>>>>>>>>> %s" % len(bases))
        return super().__new__(cls, clsname, bases, clsdict)

class Base(metaclass=mytype):
    pass

class A(Base):
    pass

class B(Base):
    pass

class C(A,B):
    pass
```

### structly, avoid writing similar __init__ again'n'again

```
class Structure
  _fields = []
  def __init__(self, *args):
    for name, val in zip(self._fields, args):
      setattr(self, name, val)

class Stock(Structure):
  _fields = ['name', 'shares', 'price']

class Point(Structure):
  _fields = ['x', 'y']

class Address(Structure):
  _fields = ['hostname', 'port']

```

* Signature
``` import inspect ; print(inspect.signature(Stock)) ```

signatures are more than just metadata, in python3 can construct it themselves
```
from inspect import Parameter, Signature
fields = ['name', 'shares', 'price']
params = [ Parameter(name, Parameter.POSITIONAL_OR_KEYWORD) for name in fields ]
sig = Signature(Stock)
```

* above structure looses lot of debug information as signatures leak, kwargs leak, so

```Python3
from inspect import Parameter, Signature

def make_signature(names):
  return Signature(
    Parameter(name, Parameter.POSITIONAL_OR_KEYWORD)
    for name in names
    )

class Structure
  __signature__ = make_signature([])
  _fields = []
  def __init__(self, *args, **kwargs):
    bound = self.__signature__.bind(*args, **kwargs)
    for name, val in bound.arguments.items():
      setattr(self, name, val)

class Stock(Structure):
  _fields = ['name', 'shares', 'price']
  __signature__ = make_signature(_fields)
```
---


#### Decorator Factory

> Decorator vs MetaClass, how much stuff are you gonna place in base-class.
> Use class decorator if tweak classes that might be unrealted.
> Use a metaclass if trying to perform actions in combination with inheritance.
> Don't just plain dismiss.


```
class NiceDecoratorMeta(type):
    def __call__(self, *args, **kwargs):
        if len(args) == 1 and callable(args[0]) and not kwargs:
            func = list(args).pop(0)
            _klas = type.__call__(self, func, *args)
            return _klas
        else:
            def decorate(func):
                decorated = type.__call__(self, func, *args, **kwargs)
                return func(decorated)
            return decorate


class NiceDecorator(object):
    """ Base class for class-based decorators. """
    __metaclass__ = NiceDecoratorMeta

    def __init__(self, func):
        self.func = func

    def __call__(self, *args, **kwargs):
        return self.func(*args, **kwargs)


class debug_function(NiceDecorator):
    def __init__(self, func, output_to=sys.stderr):
        print("debug_function init over %r" % func.__name__)
        if not output_to or output_to == func:
            output_to = sys.stderr
        super(debug_function, self).__init__(func)
        self.output_to = output_to

    def __call__(self, *args, **kwargs):
        print("debug_function called over %r" % self.func.__name__)
        self.output_to.write("%s(args=%r, kwargs=%r)\n" % (
                              self.func.__name__, args, kwargs
                            ))
        return self.func(*args, **kwargs)


@debug_function()
def myfunc(var):
    pass


@debug_function()
def myotherfunc(var):
    print(var)


@debug_function(output_to=sys.stdout)
def myanotherfunc(var):
    print(var)

print("> myfunc")
myfunc(1)
print("> myotherfunc")
myotherfunc(2)
print("> myanotherfunc")
myanotherfunc(3)
```

---

### Descriptor Protocol

* Properties, you can upgrade attributes to have checks
```
class Stock(Structure):
  _fields = ['name', 'shares', price']

  @property
  def shares(self): #getter
    return self._shares

  @shares.setter
  def shares(self, vale): #setter
    if not isinstance(value, int):
      raise TypeError('expected int')
    if value < 0:
      raise ValueError('Must be >=0')
    self._shares = value
```

* Writing similar code for all attributes is pain, complexity  by volume.
In this case checking are two kinds, type and value validation. Structure it.
```
class Descriptor:
  def __init__(self, name=None):
    self.name = name
  def __get__(self, instance, cls):
    return instance.__dict__[self.name]
  def __set__(self, instance, value):
    instance.__dict__[self.name] = value # with checks
  def __delete(self, instance):
    del instance.__dict__[self.name]

class Stock(Structure):
  _fields = ['name', 'shares', price']
  shares = Descriptor('shares')
```

* Specialize the Descriptor for Type Checks
```
class Descriptor:
  def __init__(self, name=None):
    self.name = name
  def __set__(self, instance, value):
    instance.__dict__[self.name] = value # with checks

class Typed(Descriptor):
  ty = object
  def __set__(self, instance, value):
    if not isinstance(value, self.ty):
      raise TypeError("Expected %s" % self.ty)
    super.__set__(instance, value)

class Integer(Typed):
  ty = int

class Float(Typed):
  ty = float

class String(Typed):
  ty = str

class Stock(Structure):
  _fields = ['name', 'shares', price']
  name = String('name')
  shares = Integer('shares')
  price = Float('float')
  
```

* Specialize the Descriptor for Validated Value, Type Checks
```
class NotEmpty(Descriptor):
  def __set__(self, instance, value):
    if value == None:
      raise ValueError("Must be initialize.")
    if value == "":
      raise ValueError("Must not be empty.")
    super().__set__(instance, value)

class Positive(Descriptor):
  def __set__(self, instance, value):
    if value < 0:
      raise ValueError("Must be >= 0")
    super().__set__(instance, value)

class NotEmptyString(String, NotEmpty):
  pass

class PositiveInteger(Integer, Positive): # need Positive after, so make sure is Integer
  pass

class PositiveFloat(Float, Positive):
  pass

class Stock(Structure):
  _fields = ['name', 'shares', price']
  name = NotEmptyString('name')
  shares = PositiveInteger('shares')
  price = PositiveFloat('float')
  
```

---

### MRO (Method Resolution Order)

```
PositiveInteger.__mro__
```

super() call doesn't mean go to Parent, it goes to the next item on the MRO list.

---

### Keyword Arguments

* Generally no __init__ in multi-inheritance, but check
```
class Sized(Descriptor):
  def __init__(self, *args, minlen, maxlen, **kwargs):
    self.minlen = minlen #minlen, maxlen will get passed as kwargs
    self.maxlen = maxlen
    super().__init__(*args, **kwargs)

  def __set__(self, instance, value):
    if len(value) > self.maxlen:
      raise ValueError("Too Big")
    if len(value) < self.minlen:
      raise ValueError("Too Small")
    super().__set__(instance, value)

class SizedString(String, Sized):
  pass

import re
class Regex(Descriptor):
  def __init__(self, *args, pat, **kwargs):
    self.pat = re.compile(pat)
    super().__init__(*args, **kwargs)
  def __set__(self, instance, value):
    if not self.pat.match(value):
      raise ValueError("Invalid String")
    super().__set__(instance, value)

class SizedRegexString(SizedString, Regex):
  pass

class Stock(Structure):
  _fields = ['name', 'shares', price']
  name = SizedRegexString('name', pat="^[A-Za-z]+$", minlen=3, maxlen=128)
  shares = PositiveInteger('shares')
  price = PositiveFloat('float')

```

---
Annoyance : still quite a bit repetition, signature and type checking not unified
check part.2
---

* check up on signature attribute

