#!/usr/bin/env python -tt

import sys

def main():
  print sys.argv
  if len(sys.argv) < 1 or len(sys.argv) > 6:
    print "Too Many/Abnormal args:", sys.argv[1:]
  elif len(sys.argv) >= 2 and len(sys.argv) <= 6:
    print "Decent Amount of args:", sys.argv[1:]
  else:
    print "Program Name: ",sys.argv[0]
  str1 = 'Hello'
  print str1 + 'to you', str1.lower(), str1.upper()
  print len(str1), str1.find('l'), str1[0], str1[-1], str1[1:2], str1[:2], str1[2:], str1[-2:]
  print '%s to you %s' % (str1, 'too')
  # print "\ndir(sys):\n", dir(sys)
  # also available help(sys) giving manpage like help

# avoiding default run behavior when it's imported in another py-code
# not directly run
if __name__ == '__main__':
  main()
