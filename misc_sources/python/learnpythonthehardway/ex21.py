# -*- coding: utf-8 -*-
# functions can return something

def add(a, b):
    print "Op: %s + %s" % (a, b)
    return a + b

def subtract(a, b):
    print "Op: %s - %s" % (a, b)
    return a - b

def multiply(a, b):
    print "Op: %s * %s" % (a, b)
    return a * b

def divide(a, b):
    print "Op: %s / %s" % (a, b)
    return a / b

print "with %s, %s" % (10, 2)
print "adding them gives %s" % add(10, 2)
print "subtracting them gives %s" % subtract(10, 2)
print "multiplying them gives %s" % multiply(10, 1)
print "dividing them gives %s" % divide(10, 1)

print "puzzle gives %s" % (
        add(8,
            subtract(7,
                multiply(6,
                    divide(5, 4))))
        )
