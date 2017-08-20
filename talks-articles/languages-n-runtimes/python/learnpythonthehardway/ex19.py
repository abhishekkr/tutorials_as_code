# -*- coding: utf-8 -*-
# functions and variables

def add(num1, num2):
    print "%d" % (int(num1) + int(num2))

add(1, 2)

a, b = 3, 4
add(a, b)

add(5 + 6, 7 + 8)
add(a + 9, b + 10)

print "Enter both numbers to be added..."
add(raw_input('number1: '), raw_input('number2: '))
