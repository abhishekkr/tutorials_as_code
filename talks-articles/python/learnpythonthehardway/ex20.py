# -*- coding: utf-8 -*-
# functions and files

from sys import argv

script, infile = argv

def print_all(f):
    print f.read()

def rewind(f):
    f.seek(0)

def print_line(line_count, f):
    print line_count, f.readline()

current_file = open(infile)

print "*"*10
print "WHOLE FILE\n"
print_all(current_file)

print "*"*10
print "REWIND to start of File"
rewind(current_file)

current_line = 1
print_line(current_line, current_file)

current_line += 1
print_line(current_line, current_file)

current_line += 1
print_line(current_line, current_file)
