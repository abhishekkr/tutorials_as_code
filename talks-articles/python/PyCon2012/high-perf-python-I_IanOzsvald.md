## Workshop: High Performance Python I
* by Ian Ozsvald at PyCon 2012

* Profile big Python codebase

```
python -m cProfile -o res.prof pure-python-slow.py

runsnake res.prof
```

* line profile

```
kernprof.py -l -v pure-python-slow-lineprofiler.py
```

### Cython

> * manually add types, converts to C
> * .pyx files (built on pyrex)
> * 10-100 speed-up
> * numpy integration
> * http://cython.org
>
> * ```cython -a calculate_z.pyx``` to get profiling feedback

#### ShedSkin

* Auto-Converts Python to C++ (auto-type inference)
* Handles 10k+ lines of Python
* Can only import modules that have been implemented
* No numpy, PIL etc but great for writing new fast modules

---

#### Numpy

* there's no point using Numpy if not using vector-ized calculations

---

* Multiprocessing Pool

A Python instance spawn sub-python instances and make them all solve part of problems, which give up result back up to parent. Parents combine result and pass along.

---


