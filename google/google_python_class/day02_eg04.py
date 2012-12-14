#!/usr/bin/env python -tt

# List Comprehension

def ls_a():
  import os
  import re
  fylz = os.listdir('/tmp')
  hidden = [ f    for f in fylz      if re.search(r'^\.', f)]
  print sorted(hidden)[0]

def main():
  lst = ['aaaa', 'b', 'cccccc', 'dd', 'eee']
  print lst
  lst2 = [ len(s) for s in lst ]
  print "lst2 = [ len(s) for s in a ]", "\n gives\n", lst2
  lst3 = [ len(s)     for s in lst    if len(s) > 2  ]
  print "lst3 = [ len(s)     for s in lst    if len(s) > 2  ]", "\n gives\n", lst3
  ls_a()

if __name__ == '__main__':
  main()
