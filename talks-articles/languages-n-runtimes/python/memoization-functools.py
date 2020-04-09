import functools
import math

@functools.lru_cache(maxsize=128)
def pow(num, power):
    print("%d ^ %d" % (num, power))
    return math.pow(num, power)


if __name__ == '__main__':
    print(pow(4,2))
    print(pow(4,2))
    print(pow(4,2))
