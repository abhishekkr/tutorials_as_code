## Python3 & MetaProgramming Part.3(of3)
* David Beazley

### Solution to Mess : XML (any serializer) ;)
_not really, if you are serious_

```
<structures>
  <structure name="Stock">
    <field type="SizedRegexString" minlen="3" maxlen="70" pat="'^[A-Za-z]+'">name</field>
    <field type="PositiveInteger">shares</field>
    <field type="PositiveFloat">price</field>
  </structure>
  <structure name="Point">
    <field type="Integer">x</field>
    <field type="Integer">y</field>
  </structure>
  <structure name="Address">
    <field type="String">hostname</field>
    <field type="Integer">port</field>
  </structure>
</structures>
```

```
# xml to classes
from xml.etree.ElementTree import parse

def _struct_to_class(structure):
  name = structure.get('name')
  code = 'class %s(Structure):\n' % name
  for field in structure.findall('field'):
    dtype = field.get('type')
    options = ["%s = %s" % (key,val) for key, val in field.items() if key != 'type']
    name = field.text.strip()
    code += '  %s = %s(%s)\n' % (name, dtype, ','.join(options))
  return code

def _xml_to_code(filename):
  doc = parse(filename)
  code = 'import typestruct as _ts\n'
  for st in doc.findall('structure'):
    code += _struct_to_class(st)
  return code

code = _xml_to_code('xml-with-struct.xml')
exec(code)
```

---

### make XML import-able

```
import sys
print(sys.meta_path)
```

```
class MyFinder:
  def find_module(self, fullname, path):
    # fullname is name of what's being imported
    # path is path setting (for packages)
    print(fullname, path)
    return None

sys.meta_path.insert(0, MyFinder())

import math
import threading
import socket
```

* so in code

```
import imp, sys
class StructXMLLoader:
  def __init__(self, filename):
    self.filename = filename
  def load_module(self, fullname, path):
    if fullname in sys.modules:
      mod = sys.modules[fullname]
    else:
      mod = imp.new_module(fullname)
      sys.modules[fullname] = mod
    mod.__file__ = self.filename
    mod.__loader__ = self
    code = _xml_to_code(self.filename)
    exec(code, mode.__dict__, mode.__dict__)
    return mod

class StructFinder:
  @classmethod
  def find_module(self, fullname, path):
    for dirname in sys.path:
      filename = os.path.join(cls, fullname + ".xml")
      if os.path.exists(filename):
        print('Loading XML:', filename)
        return StructXMLLoader(filename)
      return None

import sys
def install_importer():
  sys.meta_path.append(StructFinder)

install_importer()
import xmlfilename
dir(xmlfilename)
inspect.getsource(xmlfilename)
```

---

### Flashback

* descriptors as buildding blocks
* hiding annoying details (signatures, etc)
* dynamic code generation
* customizing import

* Python3 designed to
> * More advanced Metaclasses (e.g. __prepare__)
> * Signatures
> * Import hooks
> * Keyword-only args
> ```funk(self, *args, kwVarName, **kwargs)```
> "2 < 'hello'"
> function annotations; ```funk(x:int, y:int) -> int:```
> non-local variables; ```def a():\n  x = 0\n  nonlocal x\n x = newvalue\n...```

*

```
def add00(x:int, y:int) -> int:
  return x+y
add00.__annotations__

def add01(x:int, y:int) -> lambda x: 2*x:
  return x+y
add01.__annotations__

def add02(x:34, y:'hello') -> lambda x: 2*x:
  return x+y
add02.__annotations__
```

```
#*-encoding=customEncoding
```

---
---

