# -*- coding: utf-8 -*-
# asking Q

print "First Name:\t",
first_name = raw_input()

print "Last Name:\t",
last_name = raw_input()

print "Hello, %s %s." % (first_name, last_name)

print "Enter Number:",
num1 = int(raw_input())

print "Enter Expression:",
expr = input() # security issues, don't use
print expr(5)
# lambda x: x * x
# expr(10)
