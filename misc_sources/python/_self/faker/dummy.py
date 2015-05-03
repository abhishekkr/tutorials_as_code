#!/usr/bin/env python

import sys
import os


def d(_d):
    sys.stdout.write(_d)
    sys.stdout.flush()
    return 1


def u(_u):
    if os.path.isfile(_u):
        return d("ohk %s is there" % _u)
    else:
        return m("ohk %s is missing" % _u)


def m(_m):
    print "ERROR: %s" % _m
    return 3


if __name__ == '__main__':
    u("/tmp/abc")

