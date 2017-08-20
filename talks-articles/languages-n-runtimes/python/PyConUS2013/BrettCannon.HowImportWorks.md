## How Import Works
> Brett Cannon

> More accurate to Python3 latest version by 2013

### Terminology

* Loader : loads a module

* Finder : finds a module

* Path entry finder : searched in entry from import path

* Meta path finder : found on sys.meta_path

---

### how spans out

from ..spam import fly

__import__('spam', globals(), locals(), ['fly'], 2)

> 2 is number of dot(depth) for spam; 0 if direct import

---
---

