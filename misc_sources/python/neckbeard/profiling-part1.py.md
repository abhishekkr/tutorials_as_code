### Profiling Python Part1

import time


def task01():
    x = range(10000)
    sum(x)


def task02():
    x = range(10000)
    total = 0
    for num in x:
        total += num


def check01(_func):
    start = time.time()

    _func()

    finish = time.time()
    runtime = finish - start
    print(runtime)


def check02(_func):
    """
    using 'timeit' module turns off GC and checks multiple times
    For Help:
        python -m timeit -h
    Usage:
        python -m timeit -s 'import sum' 'file01.main()' #with file01.py main()
        python -m timeit -s 'import sum' 'file02.main()' #with file02.py main()
    """
    pass


if __name__ == '__main__':
    check01(task01)
    check01(task02)

