# -*- coding: utf-8 -*-
# reading and writing files

from sys import argv
script, filename = argv

from os import path
if path.isfile(filename):
    print "Give me a filename that doesn't exist."
    from sys import exit
    exit()

fyl = open(filename, 'w')
fyl.write("A\nB\nC\nD\nE")
fyl.close()

print "Content of file"
print "+" * 10
with open(filename) as fyl:
    for lyn in fyl:
        print lyn,

print "Truncating files..."
fyl = open(filename, 'w')
fyl.truncate()
fyl.close()

if path.isfile(filename):
    print "And the %s has been deleted." % filename

print "Now reading it..."
fyl = open(filename)
fyl.readlines()
fyl.close()
