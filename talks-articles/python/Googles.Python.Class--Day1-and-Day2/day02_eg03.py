#!/usr/bin/env python -tt

# URL and HTTP
import urllib
import shutil

def helpdef():
  """
  would be shown when done
  >> import day02_eg03
  >> help(day02_eg03.helpdef)
  """
  print ''

def url_story1(url):
  try:
    uf = urllib.urlopen(url)
    print uf.read()
    # urllib.urlretrieve(url, 'local_page') ##save it locally
  except: print 'URL cannot be read currently. Check Network'

def main():
  print 'URL and HTTP:'
  print 'URL Story1:'
  url_story1('http://127.0.0.1:8000')
  url_story1('http://www.google.com')

if __name__ == '__main__':
  main()
