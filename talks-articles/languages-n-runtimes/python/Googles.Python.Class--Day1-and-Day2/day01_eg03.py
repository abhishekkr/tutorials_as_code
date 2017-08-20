#!/usr/bin/env python -tt

# HASH / DICTIONARIES

def hash_story1(h):
  print "Hash:", h
  try: print h['x']
  except: print 'No Key referenced is Error'
  print h
  print h['a'], h.get('a')
  print 'a' in h, 'x' in h

def hash_story2(h):
  print "Hash:", h
  print "Keys:", h.keys()
  print "Values:", h.values()
  for k in h: print "-> ", h[k]
  print h.items()

def cat(fylname):
  f = open(fylname, 'rU')
  wcl = 0
  for line in f:
    wcl += 1
  print wcl
  f.close()

def wcl(fylname):
  f = open(fylname, 'rU')
  print len(f.readlines())
  f.close()

def wcc(fylname):
  f = open(fylname, 'rU')
  fylstr = f.read()
  words  = fylstr.split(' ')
  dict = {}
  for word in words:
    try: dict[word] += 1
    except: dict[word] = 0
  print len(words)
  print dict
  f.close()

def main():
  h = {}
  h['a'] = 1
  h['b'] = 'beta'
  h['c'] = [1, 'beta']
  h['d'] = 'donotknow'
  print "\nHash Story 1:"
  hash_story1(h)
  print "\nHash Story 2:"
  hash_story2(h)
  print "\ncat:"
  cat('day01_eg01.py')
  print "\nwc -l:"
  wcl('day01_eg01.py')
  print "\nwc c:"
  wcc('day01_eg01.py')

if __name__ == '__main__':
  main()
