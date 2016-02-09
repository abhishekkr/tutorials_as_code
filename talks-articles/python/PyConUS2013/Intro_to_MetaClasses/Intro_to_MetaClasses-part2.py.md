## Python3 & MetaProgramming Part.2(of3)
* David Beazley

### A New Metaclass

```
from collections import OrderDict

class StructMeta(type):
  @classmethod
  def __prepare__(cls, name, bases): #it's python3
    """
    creates and returns dictionary to use for execution of the class body
    OrderDict will preserve the definition order
    """
    return OrderDict()

  def __new__(cls, name, bases, clsdict):
    """
    you can get rid of _fields attribute, and names
    """
    fields = [ key for key, val in clsdict.items() if isinstance(val, Descriptor)]

    for name in fields:
      clsdict[name].name = name

    clsobj = super().__new__(cls, name, bases, dict(clsdict))
    sig = make_signature(fields)
    setattr(clsobj, '__signature__', sig)
    return clsobj

class Structure(metaclass=StructMeta):
  _fields = []
  def __init__(self, *args, **kwargs):
    bound = self.__signature__.bind(*args, **kwargs)
    for name, val in bound.arguments.items():
      setattr(self, name, val)

class Stock(Structure):
  name = SizedRegexString(pat="^[A-Za-z]+", minlen=3, maxlen=70)
  shares = PositiveInteger()
  price = PositiveFloat()
```

---

#### Performance (BAD but not all features)

Option.1 : Simple
```
class Stock:
  def __init__(self, name, shares, price):
    self.name = name
    self.shares = shares
    self.price = price
```

Option.2 : Simple
```
class Stock(Structure):
    name = SizedRegexString(...)
    shares = PositiveInteger()
    price = PositiveFloat()
```

* Instance Creation : Simple(1.07s), Meta(91.8s)
* Attribute Lookup : Simple(0.08s), Meta(0.08s) ## didn't implement GETTER
* Attribute Assignment : Simple(0.11s), Meta(3.40s)
* Attribute Assignment : Simple(0.14s), Meta(8.14s)

---

### Code Generation

```
def _make_init_(fields):
  """
  Give a list of field names, make an __init__ method code
  """
  code = "def __init__(self, %s):\n" % \
    ",".join(fields)

  for name in fields:
    code += "   self.%s = %s\n" % (name, name)

  return code

class StructMeta(type):
  @classmethod
  def __prepare__(cls, name, bases): #it's python3
    """
    creates and returns dictionary to use for execution of the class body
    OrderDict will preserve the definition order
    """
    return OrderDict()

  def __new__(cls, name, bases, clsdict):
    """
    you can get rid of _fields attribute, and names
    """
    fields = [ key for key, val in clsdict.items() if isinstance(val, Descriptor)]

    for name in fields:
      clsdict[name].name = name

    if fields:
      init_code = _make_init_(fields)
      exec(init_code, globals(), clsdict)

    clsobj = super().__new__(cls, name, bases, dict(clsdict))
    sig = make_signature(fields)
    setattr(clsobj, '__signature__', sig)
    return clsobj

class Structure(metaclass=StructMeta):
  _fields = []

```

* Perf Changes
Simple(1.1s)
Old Meta w/ Signatures (91.8s)
New Meta w/ Exec (17.6s)

---

#### Generating __set__

```
class DescriptorMeta(type):
  def __init__(self, clsname, bases, clsdict):
    super().(clsname, bases, clsdict)
    if '__set__' in clsdict:
      raise TypeError('use set_code(), not __set__()')
    code = _make_setter_(self)
    exec(code, globals(), clsdict)
    setattr(iself, cls, '__set__', clsdict['__set__'])

class Descriptor(metaclass=DescriptorMeta):
  def __init__(self, name=None):
    self.name = name
  @staticmethod
  def set_code():
    return ["""instance.__dict__[self.name] = value"""]

class Typed(Descriptor):
  ty = object
  @staticmethod
  def set_code():
    return ["""if not isinstance(value, self.ty):""",
      """  raise TypeError("Expected %s" % self.ty)"""]

class NotEmpty(Descriptor):
  @staticmethod
  def set_code():
    return ["""if value == None:""",
      """  raise ValueError("Must be initialize.")"""
      """if value == "":""",
      """  raise ValueError("Must not be empty.")"""]

class Positive(Descriptor):
  @staticmethod
  def set_code():
    return ["""if value < 0:""",
      """  raise ValueError("Must be >= 0")"""]

class Sized(Descriptor):
  def __init__(self, *args, minlen, maxlen, **kwargs):
    self.minlen = minlen #minlen, maxlen will get passed as kwargs
    self.maxlen = maxlen

  @staticmethod
  def set_code():
    return ["""if len(value) > self.maxlen:""",
      """  raise ValueError("Too Big")""",
      """if len(value) < self.minlen:""",
      """  raise ValueError("Too Small")"""]

class Regex(Descriptor):
  def __init__(self, *args, pat, **kwargs):
    self.pat = re.compile(pat)
    super().__init__(*args, **kwargs)
  @staticmethod
  def set_code():
    return ["""if not self.pat.match(value):""",
      """  raise ValueError("Invalid String")"""]

def _make_setter_(dcls):
  code = 'def __set__(self, instance, value):\n'
  for d in dcls.__mro__:
    if 'set_code' not in d.__dict__:
      code += '  pass'
      break
    for line in d.set_code():
      code += '  %s\n' % line
  return code
```

* Perf Changes
Simple(1.1s)
Old Meta w/ Signatures (91.8s)
New Meta w/ Exec (7.19s)

---
---
