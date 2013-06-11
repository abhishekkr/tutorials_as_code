# -*- coding: utf-8 -*-
# more files

from sys import argv
from os.path import exists

script, from_file, to_file = argv

print "copying from %s to %s" % (from_file, to_file)

indata = open(from_file).read()
print "The input file is %d bytes long" % len(indata)

if exists(to_file):
    print "%s file already exists, I'm gonna append to that." % to_file

out_file = open(to_file, 'a')
out_file.write(indata)
out_file.close()
print "copied."
